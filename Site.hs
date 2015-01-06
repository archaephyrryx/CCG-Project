{-# LANGUAGE FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses, ScopedTypeVariables,
	TypeFamilies, TypeSynonymInstances,
	OverloadedStrings #-}

module Site where

import Control.Monad
import Text.Blaze ((!))
import Data.Monoid ((<>))
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

sitename = "HappleJack"

appTemplate :: String -> [Html] -> Html -> Html
appTemplate title headers body = 
    H.html $ do
        H.head $ do
            H.title (H.toHtml title)
            H.meta ! A.httpEquiv "Content-Type"
                   ! A.content "text/html;charset=utf-8"
            sequence_ headers
        H.body $ do
            body

base :: String -> Html -> Html
base title content =
    H.html $ do
        H.head $ do
            H.title (H.toHtml title)
            H.meta ! A.httpEquiv "Content-Type"
                   ! A.content "text/html;charset=utf-8"
            sequence_ headers
        H.body $ do
            nav
            content
    where
        headers = [ H.meta ! A.name "keywords"
                           ! A.content "my little pony, mlp, ccg, tcg, enterplay, tabletop, brony"
                  , H.meta ! A.name "description"
                           ! A.content "My Little Pony CCG Metadatabase"
                  ]

nav :: Html
nav = H.nav $ do
        H.a ! A.href "/home" $ logo
        H.a ! A.href "/card" $ "Cards"
        H.a ! A.href "/deck" $ "Deck Builder"
        H.form $ do
            H.input ! A.type_ "search" ! A.placeholder "Search"
    where
        logo = H.img ! A.class_ "logo" ! A.src "/res/logo.png"
        
        
home :: Html
home = base pagename (H.div $ do
                        H.h1 $ (H.toHtml $ pagename)
                        H.p $ (H.toHtml $ "Welcome to " <> pagename <> ", an in-development website for the MLP:CCG.")
                        H.p $ do "This site was inspired by, and modelled on, "
                                 H.a ! A.href "http://ponyhead.com" $ "PonyHead"
                        H.p $ "This site has several components that behave similarly to those of PonyHead, as well as several novel features. Bear in mind, though, that this site is still under development, so those features might be missing, or buggy if they are present."
                        )
    where
        pagename = sitename

card :: Html
card = base (pagename ++ ": Cards") card'
    where
        pagename = sitename

deck :: Html
deck = base (pagename ++ ": Deck Builder") deck'
    where
        pagename = sitename

inputNumber :: String -> String -> H.Html
inputNumber name placeholder = H.input ! A.type_ "number"
                                       ! A.min "0"
                                       ! A.step "1"
                                       ! A.pattern "\\d+"
                                       ! A.name (H.toValue name)
                                       ! A.placeholder (H.toValue placeholder)

card' :: Html
card' = H.form ! A.action "/card" ! A.method "post" $ do 
    H.table ! A.id "filter" $ do
        H.tr $ do
            H.td $ do
                H.div ! A.id "minmax" $ do
                    H.label ! A.for "powRange" $ "Power"
                    H.div ! A.id "powRange" $ do
                        (inputNumber "powmin" "Min")
                        "to"
                        (inputNumber "powmax" "Max")
                    H.label ! A.for "costRange" $ "Cost"
                    H.div ! A.id "costRange" $ do
                        (inputNumber "costmin" "Min")
                        "to"
                        (inputNumber "costmax" "Max")
                    H.label ! A.for "reqRange" $ "Requirement"
                    H.div ! A.id "reqRange" $ do
                        (inputNumber "reqmin" "Min")
                        "to"
                        (inputNumber "reqmax" "Max")
            H.td $ do
                H.label ! A.for "setFilter" $ "Set"
                H.select ! A.name "setFilter" ! A.multiple "true" $ do
                    H.option ! A.value "0" $ "Premiere"
                    H.option ! A.value "1" $ "Canterlot Nights"
                    H.option ! A.value "2" $ "Rock and Rave"
                    H.option ! A.value "3" $ "Celestial Solstice"
                    H.option ! A.value "4" $ "Crystal Games"
            H.td $ do        
                H.label ! A.for "colFilter" $ "Color"
                H.select ! A.name "colFilter" ! A.multiple "true" $ do
                    H.option ! A.value "0" $ "None"
                    H.option ! A.value "1" $ "Blue"
                    H.option ! A.value "2" $ "Yellow"
                    H.option ! A.value "3" $ "Purple"
                    H.option ! A.value "4" $ "Pink"
                    H.option ! A.value "5" $ "Orange"
                    H.option ! A.value "6" $ "White"
            H.td $ do        
                H.label ! A.for "tyepFilter" $ "Type"
                H.select ! A.name "typeFilter" ! A.multiple "true" $ do
                    H.option ! A.value "0" $ "Mane"
                    H.option ! A.value "1" $ "Friend"
                    H.option ! A.value "2" $ "Resource"
                    H.option ! A.value "3" $ "Event"
                    H.option ! A.value "4" $ "Troublemaker"
                    H.option ! A.value "5" $ "Problem"
            H.td $ do        
                H.label ! A.for "rarFilter" $ "Rarity"
                H.select ! A.name "rarFilter" ! A.multiple "true" $ do
                    H.option ! A.value "0" $ "Fixed"
                    H.option ! A.value "1" $ "Common"
                    H.option ! A.value "2" $ "Uncommon"
                    H.option ! A.value "3" $ "Rare"
                    H.option ! A.value "4" $ "Ultra-Rare"
                    H.option ! A.value "5" $ "Promo"


deck' :: Html
deck' = H.form ! A.action "/deck" ! A.method "post" $ do 
    H.table ! A.id "filter" $ do
        H.tr $ do
            H.td $ do
                H.label ! A.for "setFilter" $ "Set"
                H.select ! A.name "setFilter" ! A.multiple "true" $ do
                    H.option ! A.value "0" $ "Premiere"
                    H.option ! A.value "1" $ "Canterlot Nights"
                    H.option ! A.value "2" $ "Rock and Rave"
                    H.option ! A.value "3" $ "Celestial Solstice"
                    H.option ! A.value "4" $ "Crystal Games"
            H.td $ do        
                H.label ! A.for "colFilter" $ "Color"
                H.select ! A.name "colFilter" ! A.multiple "true" $ do
                    H.option ! A.value "0" $ "None"
                    H.option ! A.value "1" $ "Blue"
                    H.option ! A.value "2" $ "Yellow"
                    H.option ! A.value "3" $ "Purple"
                    H.option ! A.value "4" $ "Pink"
                    H.option ! A.value "5" $ "Orange"
                    H.option ! A.value "6" $ "White"
            H.td $ do        
                H.label ! A.for "tyepFilter" $ "Type"
                H.select ! A.name "typeFilter" ! A.multiple "true" $ do
                    H.option ! A.value "0" $ "Mane"
                    H.option ! A.value "1" $ "Friend"
                    H.option ! A.value "2" $ "Resource"
                    H.option ! A.value "3" $ "Event"
                    H.option ! A.value "4" $ "Troublemaker"
                    H.option ! A.value "5" $ "Problem"
            H.td $ do        
                H.label ! A.for "rarFilter" $ "Rarity"
                H.select ! A.name "rarFilter" ! A.multiple "true" $ do
                    H.option ! A.value "0" $ "Fixed"
                    H.option ! A.value "1" $ "Common"
                    H.option ! A.value "2" $ "Uncommon"
                    H.option ! A.value "3" $ "Rare"
                    H.option ! A.value "4" $ "Ultra-Rare"
                    H.option ! A.value "5" $ "Promo"
