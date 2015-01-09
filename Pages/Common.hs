{-# LANGUAGE FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses, ScopedTypeVariables,
    TypeFamilies, TypeSynonymInstances,
    OverlappingInstances,
    QuasiQuotes, OverloadedStrings #-}

import Control.Monad
import Data.String                (IsString(fromString))
import Data.Text.Lazy             (Text)
import qualified Data.Text        as Strict
import qualified Data.Text.Lazy   as Lazy
import Happstack.Server
import Happstack.Server.XMLGenT
import HSP
import HSP.Monad                  (HSPT(..))
import Language.Haskell.HSX.QQ    (hsx)
import Application

module Pages.Common where

base :: String -> Html -> Html
base pagename content = 
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

template :: String -> [Html] -> Html
template pagename content =
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
        <% content %>
      </body>
    </html> :: Html
  |]
