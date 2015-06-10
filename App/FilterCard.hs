{-# LANGUAGE RecordWildCards #-}

module App.FilterCard where

import CCG
--------------------------
import Control.Monad
import Data.List
import Data.Maybe
-----------------------------
import qualified Graphics.UI.Threepenny          as UI
import qualified Graphics.UI.Threepenny.Core     as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import Graphics.UI.Threepenny.Core hiding (get, set)
-----------------------------
import App.Core
import App.Universal
import App.Widgets
import App.Filtering
-----------------------------
import Renderer.FilterCard
import Renderer.Cards
-----------------------------

{-
theader :: UI Element
theader = UI.tr #+ (map (\x -> UI.th #+ [string x]) ["#", "Rarity", "Type", "Cost", "Req.", "Name", "Power"])
-}

tabulate :: UniCard c => c -> LiquidLink Int -> UI Element
tabulate c l = trow (const (element l)) c
{-tabulate g@GenCard{..} l = UI.tr #+ (map (\x -> UI.td #+ [x]) $
            [ UI.string $ setnum g
            , UI.string $ brief grar
            , iconic ctype
            , UI.string $ fromMaybe "" (show.val <$> mcost)
            , reqtify g
            , element l
            , empower g
            ])
-}

namedMultiSelect :: String -> Element -> MultiSelect a -> UI Element
namedMultiSelect s cler sel = column [ row [ UI.bold #+ [ string s ], element cler ], row [ element sel ] ]

namedMinMax :: String -> Min a -> Max a -> UI Element
namedMinMax s mmin mmax = column [ row [ UI.bold #+ [ string s ] ], row [ element mmin, string "to", element mmax ] ]

freeRange :: [UI Element] -> UI Element
freeRange xs = column (map (\x -> row [ x ]) xs)

fcAmsHeader :: AMS -> UI Element
fcAmsHeader a@AMS{..} = do 
        uiSelectTyp <- namedMultiSelect "Type"   clearsTyp uSelectTyp
        uiSelectCol <- namedMultiSelect "Color"  clearsCol uSelectCol
        uiSelectSet <- namedMultiSelect "Set"    clearsSet uSelectSet
        uiSelectRar <- namedMultiSelect "Rarity" clearsRar uSelectRar
        uiSelects <- selectAll [element uiSelectTyp, element uiSelectCol, element uiSelectSet, element uiSelectRar]
        uiPowRange <- namedMinMax "Power" minPow maxPow
        uiCostRange <- namedMinMax "Cost" minCost maxCost
        uiReqRange <- namedMinMax "Requirement" minReq maxReq
        uiRanges <- freeRange [element uiPowRange, element uiCostRange, element uiReqRange]
        fcHeader <- row [ column [ element uiRanges ], column [ element uiSelects ] ]
        return fcHeader
