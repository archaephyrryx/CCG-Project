{-# LANGUAGE FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses, ScopedTypeVariables,
    TypeFamilies, TypeSynonymInstances,
    OverlappingInstances, OverloadedStrings #-}

module Pages.Common where

import Cards.Common
import Control.Applicative        ((<$>))
import Control.Monad
import Data.Monoid ((<>))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze ((!))
import Text.Blaze.Html5 (Html)

template :: String -> Html -> Html
template pagename content =
    H.html $ do 
        H.head $ do 
            H.title (H.toHtml pagename)
            H.meta ! A.httpEquiv "Content-Type"
                   ! A.content "text/html;charset=UTF-8"
            H.meta ! A.charset "utf-8"
            H.link ! A.rel "stylesheet"
                   ! A.href "/res/style.css"
            H.meta ! A.name "description"
                   ! A.content "My Little Pony CCG Metadatabase"
            H.meta ! A.name "keywords"
                   ! A.content "my little pony, mlp, ccg, tcg, enterplay, tabletop, brony"
        H.body $ do 
            content

base :: String -> Html -> Html
base pagename content = template pagename (nav >> content)

nav :: Html
nav = H.nav $ do
        H.a ! A.href "/home" $ logo
        H.a ! A.href "/card" $ "Cards"
        H.a ! A.href "/deck" $ "Deck Builder"
        H.form $ do
            H.input ! A.type_ "search" ! A.placeholder "Search"
    where
        logo = H.img ! A.class_ "logo" ! A.src "/res/logo.png"
