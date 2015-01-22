{-# LANGUAGE FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses, ScopedTypeVariables,
    TypeFamilies, TypeSynonymInstances,
    OverlappingInstances, OverloadedStrings #-}

module Pages.Vanilla where

import Control.Monad
import Data.Char
import Data.List
import Data.Monoid ((<>))
import Pages.Card
import Pages.Common
import Text.Blaze ((!))
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

sitename = "HappleJack"

inputNumber :: String -> String -> H.Html
inputNumber name placeholder = H.input ! A.type_ "number"
                                       ! A.min "0"
                                       ! A.step "1"
                                       ! A.pattern "\\d+"
                                       ! A.name (H.toValue name)
                                       ! A.placeholder (H.toValue placeholder)

cardHtml :: Html
cardHtml = H.form ! A.action "/card" ! A.method "post" $ do 
    H.table ! A.id "filter" $ do
        H.tr $ do
          minmax
          selectFilter
          

deckHtml :: Html
deckHtml = H.form ! A.action "/deck" ! A.method "post" $ do 
    H.table ! A.id "filter" $ do
        H.tr $ do
           selectFilter 

minmax :: Html
minmax = H.td $ do
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

selectFilter :: Html
selectFilter = do
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
