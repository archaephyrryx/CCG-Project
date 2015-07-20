{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RecursiveDo #-}

module Test.Obscura where

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
import App.Core.Helper
import CCG.Cards.Differentiation
import API.Database

main :: IO ()
main = do
	startGUI defaultConfig
		{ jsPort	= Just 10000
		, jsStatic	= Just "res/"
		} setup

setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title "Test"
    UI.addStyleSheet window "test.css"

    bar <- ranger (pure 0) (pure 0) (pure 0) (pure (const (string "")))
    foo <- oculus (pure ['a'..'z']) 26 bar (pure (\x -> "static/test/"++(x:".png"))) (pure (const element)) (pure (id))

    bFoo <- stepper (-1) $ rumors . userActive $ foo
    elClick <- UI.a
    element elClick # sink UI.text (((:[])!?"") <$> bFoo <*> (pure ['A'..'Z']))

    getBody window #+ [row [element elClick, element foo]]
