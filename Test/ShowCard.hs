{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DoRec #-}
module Test.ShowCard where

import App.Core
import App.Renderer.Cards
import App.Renderer.SingleCard
import App.Widgets
import Control.Applicative
import Control.Monad
import Data.IxSet
import Data.List
import Data.Map (Map)
import Data.Maybe
import App.FilterCard
import Cards
import Cards.Common
import Cards.Generic
import Cards.Differentiation
import Database
import Graphics.UI.Threepenny.Core
import qualified Data.Map as Map
import qualified Graphics.UI.Threepenny as UI



main :: IO ()
main = do
	startGUI defaultConfig
		{ tpPort	= Just 10000
		, tpStatic	= Just "res/"
		} setup

setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title "Test"

    let bQMatches = pure (take 10 . toList $ cardDB)
        bNoMatches = length <$> bQMatches
        pageSize = 10
        label = gname
        fRower = tabulate
        
    qList <- liquidCast bQMatches pageSize label fRower

    let tResults = userActive qList
        eResults = rumors tResults
    bResults <- stepper (-1) $ eResults

    rec
        let eSCard = whenE ((>=0) <$> bResults) ((!!) <$> bQMatches <@> eResults)
        
        bSingle <- stepper Nothing $ head <$> unions
            [ Just <$> eSCard ]

        let [buiCardImgs, buiCardText, buiCardInfo] = blTranspose 3 noop $ (maybe [] renderCard <$> bSingle)
        

    scSelect <- UI.span
    element scSelect # sink UI.text ((gname!?"Nothing selected") <$> bResults <*> bQMatches)

    scIndex <- UI.span
    element scIndex # sink UI.text (show <$> bResults)

    cardSide <- UI.div
    element cardSide # sink schildren ((:[]) <$> buiCardInfo)

    scCenter <- UI.div
    element scCenter # sink schildren ((\x y -> [ column [ x ], column [ y ]]) <$> buiCardImgs <*> buiCardText)

    let
        scContent :: UI Element
        scFooter :: UI Element
        scSideBar :: UI Element
        scDebugger :: UI Element

        scContent = element scCenter
        scHeader = noop
        scFooter = noop
        scSideBar = element cardSide
        scDebugger = row [element scSelect, element scIndex]

    let
        displayHeader   = (:[]) . const scHeader
        displayContent  = (:[]) . const scContent
        displayFooter   = (:[]) . const scFooter
        displaySideBar  = (:[]) . const scSideBar
        displayDebugger = (:[]) . const scDebugger

    content <- UI.div
    header <- UI.div
    footer <- UI.span
    sidebar <- UI.div
    dbg <- UI.span

    let bMode = pure ()
    element content # sink schildren (displayContent <$> bMode)
    element header # sink schildren (displayHeader <$> bMode)
    element footer # sink schildren (displayFooter <$> bMode)
    element sidebar # sink schildren (displaySideBar <$> bMode)
    element dbg # sink schildren (displayDebugger <$> bMode)

    getBody window # UI.set schildren ([column [ row [ element qList ], row [element header], row [column [ element content ], column [ element sidebar ]], row [element footer], row [element dbg]]])
