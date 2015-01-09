{-# LANGUAGE DeriveDataTypeable, FlexibleContexts,
    FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, OverlappingInstances,
    IncoherentInstances,
    OverloadedStrings, QuasiQuotes, RecordWildCards,
    ScopedTypeVariables, TemplateHaskell, TypeFamilies,
    TypeSynonymInstances #-} 

module Forms where

-------------------------------------------------
import Control.Applicative
import Control.Applicative.Indexed  ( IndexedFunctor(..) , IndexedApplicative(..))
import Control.Monad
import Control.Monad.Identity       ( Identity(runIdentity) )
import Data.Char
import Data.Data		            ( Data, Typeable )
import Data.Maybe
import Data.String                  ( IsString(fromString) ) 
import qualified Data.Text          as Strict
import qualified Data.Text.Lazy     as Lazy
import Data.Text.Lazy               ( Text )
import Happstack.Server
import Happstack.Server.HSP.HTML
import Happstack.Server.XMLGenT
import HSP
import HSP.Monad                    ( HSPT(..) )
import Language.Haskell.HSX.QQ      ( hsx )
import Text.Reform.Happstack
import Text.Reform.HSP.Text

import Text.Reform
    ( CommonFormError(..), Proof(..), (++>), (<++), commonFormErrorStr
    , Form, FormError(..), decimal, prove, transformEither, transform )
--------------------------------------------------
import Database
import Data.IxSet
import Data.Map                   ( Map )
import qualified Data.Map         as Map
import Data.Set                   ( Set )
import qualified Data.Set         as Set
import Control.Exception	        ( bracket )
import Control.Monad.Reader         ( ask )
import Control.Monad.State	        ( get, put )
import Data.List                    hiding (insert)
--------------------------------------------------
import Cards
import Cards.Generic
import Cards.Common hiding (Text)
import Cards.Common.Color
import Cards.Common.Stringe
import Cards.Common.Hint
import Cards.Common.Abbrev
import Cards.Common.Values
import MLPCCG
--------------------------------------------------
import Application
import Reformation
---------------------------------------------------

instance EmbedAsAttr (AppT' IO) (Attr Text Req) where
    asAttr (n := v) = asAttr (n := (Lazy.pack . show . val $ v))
instance EmbedAsAttr (AppT' IO) (Attr Text Cost) where
    asAttr (n := v) = asAttr (n := (Lazy.pack . show . val $ v))
instance EmbedAsAttr (AppT' IO) (Attr Text Power) where
    asAttr (n := v) = asAttr (n := (Lazy.pack . show . val $ v))

newtype AppError = AppCFE (CommonFormError [Input])
    deriving (Show)

type SimpleForm = Form (AppT IO) [Input] AppError [Html] ()

instance (Functor m, Monad m) =>
         EmbedAsAttr (AppT' m) (Attr Text Strict.Text) where
    asAttr (n := v) = asAttr (n := Lazy.fromStrict v)

instance (Functor m, Monad m) =>
         EmbedAsAttr (AppT' m) (Attr Text String) where
    asAttr (n := v) = asAttr (n := Lazy.pack v)

instance (Functor m, Monad m) => EmbedAsChild (AppT' m) AppError where
  asChild (AppCFE cfe)      =
     asChild $ commonFormErrorStr show cfe

instance FormError AppError where
    type ErrorInputType AppError = [Input]
    commonFormError = AppCFE

data Filter = CardFilter
                { powMin :: Power
                , powMax :: Power
                , costMin :: Cost
                , costMax :: Cost
                , reqMin :: Req
                , reqMax :: Req
                , colors :: [Color]
                , sets :: [CSet]
                , types :: [CardType]
                , rarities :: [Rarity]
                }
             | DeckFilter
                { colors :: [Color]
                , sets :: [CSet]
                , types :: [CardType]
                , rarities :: [Rarity]
                }
        deriving (Eq, Ord, Read, Show)


applyFilter :: Filter -> [GenCard]
applyFilter c@CardFilter{..} =
    toList $ cardDB @>= powMin @<= powMax @>= costMin @<= costMax @>= reqMin @<= reqMax @+ colors @+ sets @+ types @+ rarities
applyFilter d@DeckFilter{..} =
    toList $ cardDB @+ colors @+ sets @+ types @+ rarities

renderFilter :: Filter -> Html
renderFilter fil = let gens = applyFilter fil in
  [hsx|
    <table>
      <tr>
        <td>#</td>
        <td>Rarity</td>
        <td>Type</td>
        <td>Cost</td>
        <td>Req.</td>
        <td>Name</td>
        <td>Power</td>
      </tr>
      <% mapM cardLine gens %>
    </table> :: Html
  |]

cardLine :: GenCard -> Html
cardLine g@GenCard{..} =
  [hsx|
    <tr>
      <td><% genset g %></td>
      <td><% brief rar %></td>
      <td><% iconic ctype %></td>
      <td><% fromMaybe "" (show.val <$> mcost) %></td>
      <td><% reqtify (show.val <$> mreq,mcolor) %></td>
      <td><% pronounce (unravel name,genset g) %></td>
      <td><% empower (show.val <$> mpower,mcolor) %></td>
    </tr> :: Html
  |]

iconic :: CardType -> GCL
iconic x = let ipath = "res/icns/"++(show x)++".png" in
  [hsx|
    <%>
      <img class="icon" [ "src" := ipath :: Attr Text String ] />
    </%> :: GCL
  |]

reqtify :: (Maybe String, Maybe Color) -> Html
reqtify (Nothing,_) = [hsx|<span/>|]
reqtify (Just s, c) = 
  [hsx|
    <p [ "style" := ("background-color:"++(colorize c)) :: Attr Text String]>
      <% s %>
    </p> :: Html
  |]
  where
    colorize :: Maybe Color -> String
    colorize (Nothing) = "#c4c0bd"
    colorize (Just Wild) = "#c4c0bd"
    colorize (Just c) = fromJust (lookup (show c) ctab)
    ctab :: [(String,String)]
    ctab = [ ("Blue","#4dc6ce")
           , ("Orange","#ff8c57")
           , ("Pink","#e17baa")
           , ("Purple","#c39bcd")
           , ("White","#dfe7e9")
           , ("Yellow","#f6d673")
           ]

pronounce :: (String, String) -> Html
pronounce (nam, code) =
  [hsx|
    <a [ "href" := ("card/"++code) :: Attr Text String ]>
      <% nam %>
    </a> :: Html
  |]


empower :: (Maybe String, Maybe Color) -> Html
empower = reqtify
      
cardForm :: SimpleForm Filter
cardForm =
  CardFilter
    <$> labelText "Power:"      ++> inputMin (unval 0)
    <*> labelText "to"          ++> inputMax (unval 42)  <++ br
    <*> labelText "Cost:"       ++> inputMin (unval 0)
    <*> labelText "to"          ++> inputMax (unval 42)  <++ br
    <*> labelText "Requirement:"++> inputMin (unval 0)
    <*> labelText "to"          ++> inputMax (unval 42)  <++ br
    <*> labelText "Color"       ++> sv show colorValues  <++ br
    <*> labelText "Set"         ++> sv show setValues    <++ br
    <*> labelText "Type"        ++> sv show typeValues   <++ br
    <*> labelText "Rarity"      ++> sv show rarityValues <++ br
    <*  inputSubmit "filter"
  
sv f vs = selectMultiple (map (\x -> (x, f x)) vs) (const False)
      
deckForm :: SimpleForm Filter
deckForm =
  DeckFilter
    <$> labelText "Color"       ++> sv show colorValues  <++ br
    <*> labelText "Set"         ++> sv show setValues    <++ br
    <*> labelText "Type"        ++> sv show typeValues   <++ br
    <*> labelText "Rarity"      ++> sv show rarityValues <++ br
    <*  inputSubmit "filter"
