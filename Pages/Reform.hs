{-# LANGUAGE DeriveDataTypeable, FlexibleContexts,
    FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, OverlappingInstances,
    IncoherentInstances,
    OverloadedStrings, QuasiQuotes, RecordWildCards,
    ScopedTypeVariables, TemplateHaskell, TypeFamilies,
    TypeSynonymInstances #-} 

module Pages.Reform where

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
import CCG hiding (Text)
import Cards.Generic
--------------------------------------------------
import Application
import Reformation
import Pages.Common (template)
import Pages.Card (reqtify, empower, pronounce)
---------------------------------------------------

newtype AppError = AppCFE (CommonFormError [Input])
    deriving (Show)

type SimpleForm = Form (AppT IO) [Input] AppError [Html] ()
type CFilterSig = ([Maybe Power],[Maybe Cost],[Maybe Req]) -> Maybe [Color] -> Maybe [CSet] -> Maybe [Rarity] -> Maybe [CardType] -> Html
type DFilterSig = Maybe [Color] -> Maybe [CSet] -> Maybe [Rarity] -> Maybe [CardType] -> Html

instance (Functor m, Monad m) => EmbedAsChild (AppT' m) AppError where
  asChild (AppCFE cfe)      =
     asChild $ commonFormErrorStr show cfe

instance FormError AppError where
    type ErrorInputType AppError = [Input]
    commonFormError = AppCFE

data Filter = CardFilter
                { powMin :: Maybe Power
                , powMax :: Maybe Power
                , costMin :: Maybe Cost
                , costMax :: Maybe Cost
                , reqMin :: Maybe Req
                , reqMax :: Maybe Req
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

mhfilter :: Filter -> IxSet GenCard -> IxSet GenCard
mhfilter c@CardFilter{..} = (if isJust powMin then (@>= (fromJust powMin)) else id) . (if isJust powMax then (@<= (fromJust powMax)) else id) . (if isJust costMin then (@>= (fromJust costMin)) else id) . (if isJust costMax then (@<= (fromJust costMax)) else id) . (if isJust reqMin then (@>= (fromJust reqMin)) else id) . (if isJust reqMax then (@<= (fromJust reqMax)) else id)

full :: [a] -> Bool
full [] = False
full _ = True

mcfilter :: Filter -> IxSet GenCard -> IxSet GenCard
mcfilter c@CardFilter{..} = (if full colors then (@+ colors) else id) . (if full sets then (@+ sets) else id) . (if full types then (@+ types) else id) . (if full rarities then (@+ rarities) else id)
mcfilter d@DeckFilter{..} = (if full colors then (@+ colors) else id) . (if full sets then (@+ sets) else id) . (if full types then (@+ types) else id) . (if full rarities then (@+ rarities) else id)

applyFilter :: Filter -> [GenCard]
applyFilter c@CardFilter{..} = toList $ mcfilter c . mhfilter c $ cardDB
applyFilter d@DeckFilter{..} = toList $ mcfilter d $ cardDB

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
      <td><% reqtify g %></td>
      <td><% pronounce (unravel name, genset $ g)%></td>
      <td><% empower g %></td>
    </tr> :: Html
  |]

iconic :: CardType -> GCL
iconic x = let ipath = "res/icns/"++(show x)++".png" in
  [hsx|
    <%>
      <img class="icon" [ "src" := ipath :: Attr Text String ] />
    </%> :: GCL
  |]
      
cardForm :: ([Maybe Power],[Maybe Cost],[Maybe Req]) -> Maybe [Color] -> Maybe [CSet] -> Maybe [Rarity] -> Maybe [CardType] -> SimpleForm Filter
cardForm ([mnp,mxp],[mnc,mxc],[mnr,mxr]) mc ms mr mt =
  CardFilter
    <$> labelText "Power:"      ++> inputMin mnp
    <*> labelText "to"          ++> inputMax mxp            <++ br
    <*> labelText "Cost:"       ++> inputMin mnc
    <*> labelText "to"          ++> inputMax mxc            <++ br
    <*> labelText "Requirement:"++> inputMin mnr
    <*> labelText "to"          ++> inputMax mxr            <++ br
    <*> labelText "Color"       ++> sv show colorValues  mc <++ br
    <*> labelText "Set"         ++> sv show setValues    ms <++ br
    <*> labelText "Type"        ++> sv show typeValues   mt <++ br
    <*> labelText "Rarity"      ++> sv show rarityValues mr <++ br
    <*  inputSubmit "Filter"

sv f vs (Nothing) = selectMultiple (map (\x -> (x, f x)) vs) (const False)
sv f vs (Just xs) = selectMultiple (map (\x -> (x, f x)) vs) (`elem`xs)
      
deckForm :: Maybe [Color] -> Maybe [CSet] -> Maybe [Rarity] -> Maybe [CardType] -> SimpleForm Filter
deckForm mc ms mr mt =
  DeckFilter
    <$> labelText "Color"       ++> sv show colorValues  mc <++ br
    <*> labelText "Set"         ++> sv show setValues    ms <++ br
    <*> labelText "Type"        ++> sv show typeValues   mt <++ br
    <*> labelText "Rarity"      ++> sv show rarityValues mr <++ br
    <*  inputSubmit "Filter"


cardHtml :: CFilterSig
cardHtml x mc ms mr mt = do
    let action = "/card" :: Text
    result <- happstackEitherForm (form action) "card" (cardForm x mc ms mr mt)
    case result of
        (Left formHtml) ->
            template "Card Form" formHtml
        (Right flt) ->
            template "Card Result" $ [renderFilter flt]
    
deckHtml :: DFilterSig
deckHtml mc ms mr mt = do
    let action = "/deck" :: Text
    result <- happstackEitherForm (form action) "deck" (deckForm mc ms mr mt)
    case result of
        (Left formHtml) ->
            template "Deck Form" formHtml
        (Right flt) ->
            template "Deck Result" $ [renderFilter flt]
