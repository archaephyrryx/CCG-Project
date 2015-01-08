{-# LANGUAGE FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses, ScopedTypeVariables,
    TypeFamilies, TypeSynonymInstances,
    OverlappingInstances,
    QuasiQuotes, OverloadedStrings #-}

module Site ( homePage
            , cardPage
            , deckPage
            , Pager
            , Paginator
            , AppT
            , AppT'
            , Html
            , GCL ) where

import Control.Applicative        ((<$>))
import Control.Monad
import Control.Monad.Identity     (Identity(runIdentity))
import Data.List (intercalate)
import Data.String                (IsString(fromString))
import Data.Text.Lazy             (Text)
import qualified Data.Text        as Strict
import qualified Data.Text.Lazy   as Lazy
import Data.Char                  (toLower)
import Happstack.Server
import Happstack.Server.HSP.HTML
import Happstack.Server.XMLGenT
import HSP
import HSP.Monad                  (HSPT(..))
import Language.Haskell.HSX.QQ    (hsx)
import CCGForms                   (card', deck')

sitename = "HappleJack"

type Pager = ServerPartT IO Response
type Paginator = ServerPartT IO XML
type AppT m  = XMLGenT (AppT' m)
type AppT' m = HSPT XML (ServerPartT m)
type Html = AppT IO XML
type GCL = GenChildList (AppT' IO)

{-
instance (Functor m, Monad m) =>
         EmbedAsAttr (AppT' m) (Attr Text Strict.Text) where
             asAttr (n := v) = asAttr (n := Lazy.fromStrict v)

instance (Functor m, Monad m) =>
         EmbedAsChild (AppT' m) Strict.Text where
             asChild t = asChild (Lazy.fromStrict t)

instance (XMLGenerator m, StringType m ~ Text) =>
         EmbedAsAttr m (Attr Text String) where
            asAttr (n := v) = asAttr (n := Lazy.pack v)
-}

homePage :: Pager
homePage = page home

cardPage :: Pager
cardPage = page card

deckPage :: Pager
deckPage = page deck

page :: Paginator -> Pager
page = (toResponse<$>)

paginate :: Html -> Paginator
paginate = unHSPT . unXMLGenT

base :: String -> Html -> Paginator
base pagename content = paginate $
  [hsx|
    <html>
      <head>
        <meta http-equiv="content-type" content="text/html;charset=UTF-8"/>
        <meta charset="utf-8"/>
        <title> <% pagename %> </title>
        <meta name="description" content="My Little Pony CCG Metadatabase"/>
        <meta name="keywords" content="my little pony, mlp, ccg, tcg, enterplay, tabletop, brony"/>
      </head>
      <body>
        <% nav %>
        <% content %>
      </body>
    </html> :: Html
  |]

nav :: Html
nav =
  [hsx|
    <nav>
      <a href="/home"><img class="logo" src="res/logo.png" alt="Home Page"/></a> |
      <a href="/card">Cards</a> |
      <a href="/deck">Decks Builder</a> |
      <form>
        <input placeholder="Search" type="search"/>
        <ul class="dropdown-menu">
        </ul>
      </form>
    </nav>
  |]

home :: Paginator
home = base pagename $
  [hsx|
    <div>
      <h1><% pagename %></h1>
      <p>Welcome to <% pagename %>, an in-development website for the MLP:CCG.</p>
      <p>This site was inspired by, and modelled on, <a href="http://ponyhead.com">PonyHead</a></p>
      <p>This site has several components that behave similarly to those of
      PonyHead, as well as several novel features. Bear in mind, though, that this
      site is still under development, so those features might be missing, or buggy
      if they are present.</p>
    </div> :: Html
  |]
  where
    pagename = sitename

card :: Paginator
card = base pagename card'
  where
    pagename = sitename ++ ": Cards"

deck :: Paginator
deck = base pagename deck'
  where
    pagename = sitename ++ ": Deck Builder"

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
