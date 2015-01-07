{-# LANGUAGE FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses, ScopedTypeVariables,
	TypeFamilies, TypeSynonymInstances,
	OverlappingInstances, QuasiQuotes, OverloadedStrings #-}

module Main where

import Control.Applicative        ((<$>))
import Control.Monad
import Control.Monad.Identity     (Identity(runIdentity))
import Data.List (intercalate)
import Data.String                (IsString(fromString))
import Data.Text                  (Text)
import qualified Data.Text        as T
import Data.Char                  (toLower)
import Happstack.Server
import Happstack.Server.HSP.HTML
import Happstack.Server.XMLGenT
import HSP
import HSP.Monad                  (HSPT(..))
import Language.Haskell.HSX.QQ    (hsx)

main :: IO ()
main = simpleHTTP nullConf $ home {- msum
    [ dir "card" $ ok $ card
    , dir "deck" $ ok $ deck
    , ok $ home
    ] -}

sitename = "HappleJack"

type Paginator = ServerPartT IO XML
type Html = XMLGenT (HSPT XML (ServerPartT IO)) XML

paginate = unHSPT . unXMLGenT

hello :: Paginator
hello = paginate
  [hsx|
    <html>
      <head>
        <title><% sitename %></title>
        <meta content="text/html;charset=utf-8"/>
      </head>
      <body>
        Foo
      </body>
    </html> :: XMLGenT (HSPT XML (ServerPartT IO)) XML
  |]
        
base :: String -> Html -> Paginator
base title content = paginate
  [hsx|
    <html>
      <head>
        <title><% title %></title>
        <meta content="text/html;charset=utf-8"/>
        <meta keywords="my little pony, mlp, ccg, tcg, enterplay, tabletop, brony"/>
        <meta description="My Little Pony CCG Metadatabase"/>
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
      <a href="/home"><% logo %></a>
      <a href="/card">Cards</a>
      <a href="/deck">Deck Builder</a>
      <form> <input type="search" placeholder="Search"/> <ul class="dropdown-list"/> </form>
    </nav>
  |]
    where
        logo :: Html
        logo = [hsx| <img class="logo" src="/res/logo.png"/> |]


home :: Paginator
home = base pagename $
  [hsx|
    <div>
      <h1><% pagename %></h1>
      <p>Welcome to <%pagename%>, an in-development website for the MLP:CCG.</p>
      <p>This site was inspired by, and modelled on <a href="http://ponyhead.com">PonyHead</a></p>
      <p>
        This site has several components that behave similarly to those
        of PonyHead, as well as several novel features. Bear in mind,
        though, that this site is still under development, so those
        features might be missing, or buggy if they are present.
      </p>
    </div> :: Html
  |]
  where
    pagename = sitename

card :: Paginator
card = base (pagename ++ ": Cards") card'
    where
        pagename = sitename

deck :: Paginator
deck = base (pagename ++ ": Deck Builder") deck'
    where
        pagename = sitename

inputNumber :: String -> String -> Html
inputNumber nam plac =
  [hsx|
    <input type="number" min="0" step="1" pattern="[0-9]+"
        name= <% nam %>
        placeholder= <% plac %>
        />
  |]

inputRange :: (String,String) -> Html
inputRange (lab,nam) = let labl = (lab++"Range") in
  [hsx|
    <label [ "for" :=  labl ] > <% nam %> </label>
    <div [ "id" := (lab ++ "Range") ] >
      <% inputNumber (lab++"min") "Min" %>
      to
      <% inputNumber (lab++"max") "Max" %>
    </div>
  |]
 
inputFilter :: String -> [String] -> Html
inputFilter lab vals = let lb = T.pack $ ((map toLower lab)++"Filter") in
  [hsx|
    <label [ "for" := lb ]> <% lab %> </label>
    <select multiple="true" [ "name" := lb ] >
    <% [f i c | (i,c)<-(zip [0..] vals)] %>
    </select>
  |]
  where
    f :: Int -> String -> Html
    f i c =
      [hsx|
        <%>
          <option [ "value" := <% T.pack $ show i%> ] > <% c %> </option>
        </%>
      |]
                    
card' :: Html
card' =
  [hsx|
    <form action="/card" method="post">
      <table id="filter">
        <tr>
          <td>
            <div id="minmax">
              <% inputRange ("pow","Power") %>
              <% inputRange ("cost","Cost") %>
              <% inputRange ("req","Requirement") %>
            </div>
          </td>
          <% selectFilters %>
        </tr>
        <tr>
          <td> <button type="reset" value="Clear"/> </td>
          <td> <button type="submit" value="Sumbit"/> </td>
        </tr>
      </table>
    </form>
  |]

selectFilters :: Html
selectFilters =
  [hsx|
    <td><% inputFilter "Set" sets %></td>
    <td><% inputFilter "Color" colors %></td>
    <td><% inputFilter "Type" types %></td>
    <td><% inputFilter "Rarity" rarities %></td>
  |]
  where
    sets = ["Premiere","Canterlot Nights","Rock and Rave","Celestial Solstice","Crystal Games"]
    colors = ["None","Blue","Yellow","Pink","Purple","Orange","White"]
    types = ["Mane","Friend","Resource","Event","Troublemaker","Problem"]
    rarities = ["Fixed","Common","Uncommon","Rare","Ultra-Rare","Promotional"]
                    
deck' :: Html
deck' =
  [hsx|
    <form action="/deck" method="post">
      <table id="filter">
        <tr>
          <% selectFilters %>
        </tr>
        <tr>
          <td></td>
          <td> <button type="reset" value="Clear"/> </td>
          <td> <button type="submit" value="Sumbit"/> </td>
        </tr>
      </table>
    </form>
  |]
