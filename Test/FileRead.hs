{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RecursiveDo #-}

module Test.FileRead where

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
import App.Widgets.Ranger
import App.Widgets.MultiSelect
import Util
import System.Posix.Files

main :: IO ()
main = do
	startGUI defaultConfig
		{ jsPort	= Just 10000
		, jsStatic	= Just "../wwwroot/"
		} setup

setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title "Test"

    let bfName = pure ("/home/peter/ccg/CCG-Project/res/foo.txt")
    readit <- liquidLink (pure ("Read file: "++)) bfName
    
    let

      redoLayout :: String -> UI ()
      redoLayout fname = void $ do
          fcont <- liftIO (fileExist fname >>= \x -> if_ x (readFile fname) (return ""))
          layout <- mkLayout fcont []
          getBody window # set children [layout]

      mkLayout :: String -> [Element] -> UI Element
      mkLayout s _ = column [row [element readit], row [string s]]

      foo :: UI ()
      foo = readit`sinksTo`redoLayout

    redoLayout "" >> foo
