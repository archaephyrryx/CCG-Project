{-# LANGUAGE DeriveDataTypeable, FlexibleContexts,
    FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, OverlappingInstances,
    OverloadedStrings, QuasiQuotes, RecordWildCards,
    ScopedTypeVariables, TemplateHaskell, TypeFamilies,
    TypeSynonymInstances #-} 

module CCGForms (card', deck') where

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
import Cards.Common
import Cards.Common.Color
import Cards.Common.Stringe
import Cards.Common.Hint
import Cards.Common.Abbrev
import MLPCCG
--------------------------------------------------
import Site                       (Html, AppT, AppT', GCL)
---------------------------------------------------


type SimpleForm = Form (AppT IO) [Input] AppError [Html] ()

type AppError = String

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


applyFilter :: CardFilter -> [GenCard]
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
        <td>Name/td>
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

iconic :: CardType -> GLC
iconic x = let ipath = "res/icns/"++(show x)++".png" in
  [hsx|
    <%>
      <img class="icon" [ "src" := ipath :: Attr Text String ] />
    </%>
  |]

reqtify :: (Maybe String, Maybe Color) -> Html
reqtify (Nothing,_) = [hsx| |]
reqtify (Just s, c) = 
  [hsx|
    <p [ "style" := ("background-color:"++(colorize c)) :: Attr Text String]>
      <% s %>
    </p> :: Html
  |]
  where
    colorize c = fromMaybe "#c4c0bd" ((`lookup`ctab).show <$> c)
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
  cardFilter
    <$> 
  
  
  

card' :: Html
card' =
  [hsx|
    <form method="post" action="/card">
      <table id="filter">
        <tr>
          <td>
            <% minmax %>
          </td>
          <% selectFilter %>
        </tr>
        <% buttons %>
      </table>
    </form>
  |]

deck' :: Html
deck' =
  [hsx|
    <form method="post" action="/deck">
      <table id="filter">
        <tr>
          <% selectFilter %>
        </tr>
        <% buttons %>
      </table>
    </form>
  |]

buttons :: Html
buttons =
  [hsx|
    <tr>
      <td><button type="reset">Reset</button></td>
      <td><button type="submit">Submit</button></td>
    </tr> :: Html
  |]

minmax :: Html
minmax =
  [hsx|
    <div id="minmax">
      <div>
        <label for="powRange">Power</label>
        <div id="powRange">
          <input name="powmin" placeholder="Min" type="number" min="0" step="1" pattern="[0-9]+"/>
          to
          <input name="powmax" placeholder="Max" type="number" min="0" step="1" pattern="[0-9]+"/>
        </div>
      </div>
      <div>
        <label for="costRange">Cost</label>
        <div id="costRange">
          <input name="costmin" placeholder="Min" type="number" min="0" step="1" pattern="[0-9]+"/>
          to
          <input name="costmax" placeholder="Max" type="number" min="0" step="1" pattern="[0-9]+"/>
        </div>
      </div>
      <div>
        <label for="reqRange">Requirement</label>
        <div id="reqRange">
          <input name="reqmin" placeholder="Min" type="number" min="0" step="1" pattern="[0-9]+"/>
          to
          <input name="reqmax" placeholder="Max" type="number" min="0" step="1" pattern="[0-9]+"/>
        </div>
      </div>
    </div> :: Html
  |]

selectFilter :: Html
selectFilter =
  [hsx|
    <div id="selectFilter">
      <td>
        <label for="setFilter">Set</label>
        <select name="setFilter" multiple="true">
          <option value="0">Premiere</option>
          <option value="1">Canterlot Nights</option>
          <option value="2">Rock and Rave</option>
          <option value="3">Celestial Solstice</option>
          <option value="4">Crystal Games</option>
        </select>
      </td>
      <td>
        <label for="colorFilter">Color</label>
        <select name="colorFilter" multiple="true">
          <option value="0">None</option>
          <option value="1">Blue</option>
          <option value="2">Orange</option>
          <option value="3">Pink</option>
          <option value="4">Purple</option>
          <option value="5">White</option>
          <option value="6">Yellow</option>
        </select>
      </td>
      <td>
        <label for="typeFilter">Type</label>
        <select name="typeFilter" multiple="true">
          <option value="0">Mane</option>
          <option value="1">Friend</option>
          <option value="2">Event</option>
          <option value="3">Resource</option>
          <option value="4">Troublemaker</option>
          <option value="5">Problem</option>
        </select>
      </td>
      <td>
        <label for="rarityFilter">Rarity</label>
        <select name="rarityFilter" multiple="true">
          <option value="0">Fixed</option>
          <option value="1">Common</option>
          <option value="2">Uncommon</option>
          <option value="3">Rare</option>
          <option value="4">Ultra-Rare</option>
          <option value="5">Promo</option>
        </select>
      </td>
    </div> :: Html
  |]
