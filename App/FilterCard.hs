{-# LANGUAGE RecordWildCards, DoRec #-}

module App.FilterCard where

import Cards
import Cards.Common
import Cards.Differentiation
import Cards.Generic
--------------------------
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Database
import Data.Data (Data, Typeable)
import Data.IORef
import Data.IxSet hiding (null)
import qualified Data.IxSet as IxSet
import TagState
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
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
import App.Renderer.FilterCard
import App.Renderer.Cards
-----------------------------

theader :: UI Element
theader = UI.tr #+ (map (\x -> UI.th #+ [string x]) ["#", "Rarity", "Type", "Cost", "Req.", "Name", "Power"])

tabulate :: GenCard -> LiquidLink Int -> UI Element
tabulate g@GenCard{..} l = UI.tr #+ (map (\x -> UI.td #+ [x]) $
            [ UI.string $ genset g
            , UI.string $ brief rar
            , iconic ctype
            , UI.string $ fromMaybe "" (show.val <$> mcost)
            , reqtify g
            , element l
            , empower g
            ])


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


{-
setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title appname
    UI.addStyleSheet window "style.css"

      
        let 
            namedMultiSelect :: String -> Element -> MultiSelect a -> UI Element
            namedMultiSelect s cler sel = column [ row [ UI.bold #+ [ string s ], element cler ], row [ element sel ] ]

            uiSelectTyp = namedMultiSelect "Type"   clearsTyp selectTyp
            uiSelectCol = namedMultiSelect "Color"  clearsCol selectCol
            uiSelectSet = namedMultiSelect "Set"    clearsSet selectSet
            uiSelectRar = namedMultiSelect "Rarity" clearsRar selectRar

            selectAll :: [UI Element] -> UI Element
            selectAll xs = row (map (\x -> column [ x ]) xs)

            uiSelects = selectAll [uiSelectTyp, uiSelectCol, uiSelectSet, uiSelectRar]

          let
            namedMinMax :: String -> Min a -> Max a -> UI Element
            namedMinMax s mmin mmax = column [ row [ UI.bold #+ [ string s ] ], row [ element mmin, string "to", element mmax ] ]

            uiPowRange = namedMinMax "Power" minPow maxPow
            uiCostRange = namedMinMax "Cost" minCost maxCost
            uiReqRange = namedMinMax "Requirement" minReq maxReq

            freeRange :: [UI Element] -> UI Element
            freeRange xs = column (map (\x -> row [ x ]) xs)

            uiRanges = freeRange [uiPowRange, uiCostRange, uiReqRange]


        let fcbl = FCBL{..}

        let pageSize = 100
            bcView = ListView <$> (pure pageSize) <*> bCur

        stRanger <- ranger bCur bFirst bLast (psss)
        let tRanger = userLoc stRanger
            eRanger = rumors   tRanger
            bRanger = facts    tRanger
            bFirst = pure 0
            bLast = (pred).(`cdiv`pageSize) <$> bNoMatches
        bCur <- stepper 0 $ head <$> unions [ eRanger, 0 <$ eModeChange ]

        let 
            bcLabel = pure gname
            
            bcRower = pure tabulate

            bcAggra = pure (theader:)
            
        qList <- derangedCask bQMatches pageSize stRanger bLabel bRower bAggra

        let tResults = if_  <$> (tidings bMulti never) <*> (userActive qList) <*> (userActive qGrid)
            eResults = rumors     tResults
        bResults <- stepper (-1) $ eResults

    scSelect <- UI.span
    element scSelect # sink UI.text ((gname!?"Nothing selected") <$> bResults <*> bQMatches)

    scIndex <- UI.span
    element scIndex # sink UI.text (show <$> bResults)

    deckSide <- UI.div
    element deckSide # sink schildren (construct <$> bDeck)


    let
        noop :: UI Element
        noop = UI.a

        fcHeader :: UI Element
        fcHeader = row [ column [ uiRanges ], column [ uiSelects ] ]
        fcContent :: UI Element
        fcContent = element qList
        fcFooter :: UI Element
        fcFooter = element stRanger
        fcSideBar :: UI Element
        fcSideBar = noop
        fcDebugger :: UI Element
        fcDebugger = row [element scSelect, element scIndex]
-}
