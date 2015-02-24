{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DoRec #-}

module Test.Res where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.IORef
import Data.List
import Data.IxSet
import qualified Data.Map as Map
import Data.Map (Map)
import qualified	Graphics.UI.Threepenny 			as UI
import				Graphics.UI.Threepenny.Core
import App.Widgets
import Cards.Differentiation
import Database

main :: IO ()
main = do
	startGUI defaultConfig
		{ tpPort	= Just 10000
		, tpStatic	= Just "res/"
		} setup

setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title "Test"

    foo <- UI.image # set UI.src "static/icns/Friend.png"
    getBody window #+ [element foo]
