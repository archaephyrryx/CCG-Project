{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.HtmlS (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.HtmlS.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.HtmlS as H
import qualified Text.Blaze.HtmlS.Attributes as A

import DataState

main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum
      [ dir "browse" 	$ browse
      , dir "build"	$ buildDeck
      , dir "query"	$ queryCards
      , dir "tag"	$ tagCards
      , dir "relate"	$ relateTags
      , homePage
      ]

template :: Text -> Html -> Response
template title body = toResponse $
    H.html $ do
	H.head $ do
	    H.title (toHtml title)
	H.body $ do
	    body
	    p $ a ! href "/" $ "back home"

homePage :: ServerPart Response
homePage =
    ok $ template "home page" $ do
	H.h1 "Hello!"
	H.p  "Welcome to the MLP CCG Meta-Database (Archaephyrryx)."
	H.p  "This site is in pre-pre-alpha, but you are welcome to explore!"
	H.p  $ a ! href "/browse/all"  $ "browse"
	H.p  $ a ! href "/build/start" $ "build"
	H.p  $ a ! href "/query"       $ "query"
	H.p  $ a ! href "/tag"	       $ "tag"
	H.p  $ a ! href "/relate"      $ "relate"

relateTags :: ServerPart Response
relateTags =
    ok $ template "relate tags" $ do
	H.h1 "Hello!"
	H.p  "This part of the site has not been built yet :("
	H.p  "(sorry)"

tagCards :: ServerPart Response
tagCards =
    ok $ template "tag cards" $ do
	H.h1 "Hello!"
	H.p  "Here you can tag cards!"
	H.p  "This is not yet fully developed."
	H.p  "At all"

queryCards :: ServerPart Response
queryCards =
    ok $ template "query cards" $ do
	H.h1 "Hello!"
	H.p  "This is probably going to be the first component that gets developed."
	H.p  "Not yet, though"

buildDeck :: ServerPart Response
buildDeck =
    ok $ template "build deck" $ do
	H.h1 "Hello!"
	H.p  "This is where you can build a deck"
	H.p  "(except not right now)"

browse :: ServerPart Response
browse =
    ok $ template "browse" $ do
	H.h1 "Hello!"
	H.p  "Maybe you can browse cards sometime soon"
