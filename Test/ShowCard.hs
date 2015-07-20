{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RecursiveDo, RecordWildCards #-}
module Test.ShowCard where

import App.Core
import App.Home
import App.Renderer.Cards
import App.Renderer.SingleCard
import App.Renderer.FilterCard
import App.Widgets
import App.Universal
import App.FilterCard
import App.Filtering
------------------------
import Control.Applicative
import Control.Monad
import Data.IxSet
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
------------------------
import CCG
import API.Database
----------------------------
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
-----------------------------

main :: IO ()
main = do
	startGUI defaultConfig
		{ jsPort	= Just 10000
		, jsStatic	= Just "res/"
		} setup

setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title "Test"

    butAHome <- UI.button # settext "Home"
    butQCard <- UI.button # settext "Cards"

    let eAHome = UI.click butAHome
        eQCard = UI.click butQCard

        navigator :: [UI Element]
        navigator = [element butAHome, element butQCard]

    let eModeChange = head <$> unions [ FilterCard <$ eQCard, Home <$ eAHome ]
    bMode <- stepper Home eModeChange

        
    (fcbl@FCBL{..}, _, ams@AMS{..}) <- selectors (pure True) eModeChange
    let bQFilter  = behaveBFilter fcbl 
        bQMatches = toList.applyFilter <$> bQFilter
        bNoMatches = length <$> bQMatches
        pageSize = 10
        label = gname
        fRower = tabulate
        bAggra = pure (theader:)

    rec stRanger <- ranger bCur bFirst bLast (psss)
        let tRanger = userLoc stRanger
            eRanger = rumors   tRanger
            bRanger = facts    tRanger
            bFirst = pure 0
            bLast = (pred).(`cdiv`pageSize) <$> bNoMatches
        bCur <- stepper 0 $ head <$> unions [ eRanger, 0 <$ eModeChange ]

    qList <- derangedCask bQMatches pageSize stRanger (pure label) (pure fRower) bAggra

    let tResults = userActive qList
        eResults = rumors tResults
    bResults <- stepper (-1) $ eResults

    let ePositive = filterE (>=0) eResults
        eSCard = whenE ((>=0) <$> bResults) ((!!) <$> bQMatches <@> ePositive)

    let eShowMode = head <$> unions [ False <$ eQCard, False <$ eAHome, True <$ eSCard ]
    bShow <- stepper False eShowMode
    
    bSingle <- stepper Nothing $ head <$> unions
        [ Just <$> eSCard
        , Nothing <$ eQCard
        , Nothing <$ eAHome
        ]

    scSelect <- UI.span
    element scSelect # sink UI.text ((gname!?"Nothing selected") <$> bResults <*> bQMatches)

    scIndex <- UI.span
    element scIndex # sink UI.text (show <$> bResults)

    cardSide <- UI.div
    element cardSide # sink schildren (maybe [] ((:[]).cardInfo) <$> bSingle)

    scCenter <- UI.div
    element scCenter # sink schildren (maybe [] renderCard <$> bSingle)

    let
        fcContent = element qList
        scContent = element scCenter

        fcHeader = fcAmsHeader ams
        scHeader = bstring "Show Card"

        fcFooter = element stRanger
        scFooter = bstring "Show Card"

        fcSideBar = noop
        scSideBar = element cardSide

        fcDebugger = row [element scSelect, element scIndex]
        scDebugger = row [element scSelect, element scIndex]

    let
          displayHeader   = ((:[]).) . hfdsCase hmHeader fcHeader noop scHeader
          displayContent  = ((:[]).) . hfdsCase hmContent fcContent noop scContent
          displayFooter   = ((:[]).) . hfdsCase hmFooter fcFooter noop scFooter
          displaySideBar  = ((:[]).) . hfdsCase hmSideBar fcSideBar noop scSideBar
          displayDebugger = ((:[]).) . hfdsCase hmDebugger fcDebugger noop scDebugger

    content <- UI.div
    header <- UI.div
    footer <- UI.span
    sidebar <- UI.div
    dbg <- UI.span

    element content # sink schildren (displayContent <$> bMode <*> bShow)
    element header # sink schildren (displayHeader <$> bMode <*> bShow)
    element footer # sink schildren (displayFooter <$> bMode <*> bShow)
    element sidebar # sink schildren (displaySideBar <$> bMode <*> bShow)
    element dbg # sink schildren (displayDebugger <$> bMode <*> bShow)

    getBody window # UI.set schildren ([column [ row navigator, row [element header], row [column [ element content ], column [ element sidebar ]], row [element footer], row [element dbg]]])
