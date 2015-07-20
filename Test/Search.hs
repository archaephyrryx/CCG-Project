{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RecursiveDo #-}

module Test.Search where

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
import CCG.Cards.Differentiation
import API.Database

main :: IO ()
main = do
	startGUI defaultConfig
		{ jsPort	= Just 10000
		, jsStatic	= Just "../wwwroot/"
		} setup

setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title "Test"

    rec elSearch <- searchBar (pure cardDB) (bFilterString) (pure (string . show . gname))
        bFilterString <- stepper "" . rumors $ userSearch elSearch
        let tMatches = searchMatches elSearch
            bMatches = facts tMatches
            eMatches = rumors tMatches

    getBody window # set schildren ([column [ row [element elSearch]]])
