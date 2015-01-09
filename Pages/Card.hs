{-# LANGUAGE DeriveDataTypeable, FlexibleContexts,
    FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, OverlappingInstances,
    IncoherentInstances,
    OverloadedStrings, QuasiQuotes, RecordWildCards,
    ScopedTypeVariables, TemplateHaskell, TypeFamilies,
    TypeSynonymInstances #-} 

-------------------------------------------------
import Control.Applicative
import Control.Applicative.Indexed  ( IndexedFunctor(..) , IndexedApplicative(..))
import Control.Monad
import Control.Monad.Identity       ( Identity(runIdentity) )
import Data.Char
import Data.Data		            ( Data, Typeable )
import Data.Maybe
import Data.String                  ( IsString(fromString) ) 
import Data.List.Split
import Data.List
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
import Pages.Common (template, base)
---------------------------------------------------

module Pages.Card (singleCard) where

type SetNum :: (CSet, Number)
type GCR :: GenCard -> Html

instance Read SetNum where
    readsPrec = const readsSN

readsSN :: ReadS SetNum
readsSN x = let (s, n) = splitAt 2 x in
        (readCS s, readN n)


single :: String -> Html
single s = let (s,n) = read s :: SetNum
           in renderCard (head.toList $ (cardDB @= (s) @= (n)))

renderCard :: GCR
renderCard g@GenCard{..} = base (unravel name) $
  [hsx|
    <div>
      <div class="card-imgs">
        <% cardImgs g %>
      </div>
      <div class="card-text">
        <% cardText g %>
      </div>
      <div class="card-info">
        <% cardInfo g %>
      </div>
    </div> :: Html
  |]

cardImgs :: GCR
cardImgs g@GenCard{..} = let urls = curls g in
  [hsx|
    <table>
      <% mapM cimage urls %>
    </table> :: Html
  |]

curls :: Gencard -> [String]
curls g@GenCard{..} = let sn = genset g in map (("res/cards/"++sn)++) (suffixes ctype)
    where
      suffixes :: CardType -> [String]
      suffixes TMane = [".jpg","b.jpg"]
      suffixes _ = [".jpg"]

cimage :: String -> Html
cimage s =
  [hsx|
    <tr>
      <td>
        <a [ "href" := s :: Attr Text String ]><img class="card" [ "src" := s :: Attr Text String ]/></a>
      </td>
    </tr> :: Html
  |]

cardText :: GCR
cardText g@GenCard{ctype=TMane, ..} =
  [hsx|
    <dl>
      <div>
        <dt>Card Text</dt>
        <dd><% front %></dd>
      </div>
      <div>
        <dt>Boosted Card Text</dt>
        <dd><% back %></dd>
      </div>
    </dl> :: Html
  |]
  where
    front = unlines (splitOn " <P> " (fromJust (stripPrefix "Front: " (head (splitOn " Back: " (unravel text))))))
    back = unlines (splitOn " <P> " (last (splitOn " Back: " (unravel text))))
cardText g@GenCard{..} =
  [hsx|
    <dl>
      <div>
        <dt>Card Text</dt>
        <dd><% cardtext %></dd>
      </div>
    </dl> :: Html
  |]
  where
    cardtext = unlines (splitOn " <P> " (unravel text))

cardInfo :: GCR
cardInfo g@GenCard{..} =
  [hsx|
    <div>
      <div class="panel panel-default quick-facts">
        <div class="panel-heading"><h3 class="panel-title">Quick Facts</h3></div>
		  <div class="panel-body">
		    <ul>
              <% info g %>
		    </ul>
		  </div>
        </div>
      </div>
    </div> :: Html
    |]

info :: GenCard -> GCL
info g@GenCard{..} = let items = (mfilter mask allitems) in
  [hsx|
    <%>
      <% mapM renderi items %>
    </%>
  ]
  where
    mask :: [Bool]
    mask = typemask ctype
    renderi :: (String, GCR) -> Html
    renderi (nam, gcr) = [hsx| <li> <b><% nam %>:</b> <% gcr g %> </li> :: Html |]

allitems :: [(String, GCR)]
allitems = [ ("Type", typeLink)
           , ("Rarity", rarLink)
           , ("Color", colLink)
           , ("Power", empower)
           , ("Cost", appraise)
           , ("Req", reqtify)
           , ("Confront", conf)
           , ("Opponent Confront", conf')
           , ("Traits", cardTraits)
           ]

typemask :: CardType -> [Bool]
typemask x = map (==1) $ case x of
    TMane         -> [1,1,1,1,0,0,0,0,1]
    TFriend       -> [1,1,1,1,1,1,0,0,1]
    TEvent        -> [1,1,1,1,1,1,0,0,1]
    TResource     -> [1,1,1,1,1,1,0,0,1]
    TTroublemaker -> [1,1,0,1,0,0,0,0,1]
    TProblem      -> [1,1,0,0,0,0,1,1,0]

propLink :: String -> (GenCard -> String) -> (GCR)
propLink s f g =
  [hsx|
    <a [ "href" := ("http://ponyhead.com/cards?"++s++"="++(f g)) :: Attr Text String ]>
      <% f g %>
    </a> :: Html
  |]

setLink :: GCR
setLink = propLink "set" (show.set)
typeLink :: GCR
typeLink = propLink "type" (show.ctype)
rarLink :: GCR
rarLink = propLink "rarity" (show.rar)
colLink :: GCR
colLink = propLink "color" (fromMaybe "Wild" ((show<$>).mcolor))


reqtify = GCR
reqtify = cbox
empower :: GCR
empower = reqtify

cbox :: (Maybe String, Maybe Color) -> Html
cbox (Nothing,_) = [hsx|<span/>|]
cbox (Just s, c) = 
  [hsx|
    <span [ "class" := ("background-color:"++(colorize c)) :: Attr Text String]>
      <% s %>
    </span> :: Html
  |]
  where
    colorize :: Maybe Color -> String
    colorize (Nothing) = "NoColor"
    colorize (Just Wild) = "Wild"
    colorize (Just c) = show c
    
pronounce :: (String, String) -> Html
pronounce (nam, code) =
  [hsx|
    <a [ "href" := ("card/"++code) :: Attr Text String ]>
      <% nam %>
    </a> :: Html
  |]

      
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


cardHtml :: Html
cardHtml = do
    let action = "/card" :: Text
    result <- happstackEitherForm (form action) "card" cardForm
    case result of
        (Left formHtml) ->
            template "Card Form" formHtml
        (Right flt) ->
            template "Card Result" $ [renderFilter flt]
    
deckHtml :: Html
deckHtml = do
    let action = "/deck" :: Text
    result <- happstackEitherForm (form action) "deck" deckForm
    case result of
        (Left formHtml) ->
            template "Card Form" formHtml
        (Right flt) ->
            template "Card Result" $ [renderFilter flt]


{-
 - Mane { name, set, num, rar, keywords, color, power, boosted, text }
 - Friend { name, set, num, rar, keywords, color, cost, req, power, text}
 - Resource { name, set, num, rar, keywords, color, cost, req, power, text }
 - Event { name, set, num, rar, keywords, color, cost, req, power, text }
 - Troublemaker { name, set, num, rar, keywords, power, points, text }
 - Problem { name, set, num, rar, points, keywords, preqs, text }
 -}

