{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.IORef
import qualified Data.Map as Map
import Data.Map (Map)
import qualified	Graphics.UI.Threepenny 			as UI
import				Graphics.UI.Threepenny.Core

main :: IO ()
main = do
	startGUI defaultConfig
		{ tpPort	= Just 10000
		, tpStatic	= Just "../wwwroot"
		} setup

setup :: Window -> UI ()
setup window = do
	-- active elements
	return window # set UI.title "Hello World!"
	elEntr <- UI.button # set UI.text "Add Entry"
	elSave <- UI.button # set UI.text "Save Changes"
	elDrop <- UI.button # set UI.text "Discard Changes"

	elMode <- UI.button # set UI.text "View"
	result <- UI.span

	inputs <- liftIO $ newIORef []
	mode   <- liftIO $ newIORef View

	-- functionality
	let 
		displayAll = void $ do
			xs <- mapM (get value) =<< liftIO (readIORef inputs)
			element result # set text (unlines $ xs)
		
		toggler = void $ do
			emode <- liftIO (readIORef mode)
			element elMode # set text (show.toggle$emode)

		redoLayout :: UI ()
		redoLayout = void $ do
			emode <- liftIO (readIORef mode)
			let canedit = canEdit emode
			layout <- mkLayout canedit =<< liftIO (readIORef inputs)
			getBody window # set children [layout]
			displayAll
		
		mkLayout :: Bool -> [Element] -> UI Element
		mkLayout e xs = column $
			([row (if e then [element elEntr, element elMode] else [element elMode]), UI.hr] ++
			mkLine e xs "Foo")
			
		mkLine :: Bool -> [Element] -> String -> [UI Element]
		mkLine e xs s = [row [UI.span # set text s, (if e then row (map element xs) else element result)]]

		toggleMode :: UI ()
		toggleMode = do
			liftIO $ modifyIORef mode (toggle)
			
		addInput :: UI ()
		addInput = do
			elInput <- UI.input # set value ""
			on (domEvent "livechange") elInput $ \_ -> displayAll
			liftIO $ modifyIORef inputs (elInput:)

	on UI.click elEntr $ const $ do
		addInput
		redoLayout
	on UI.click elMode $ const $ do
		toggleMode
		toggler
		redoLayout
	toggleMode >> toggler >> redoLayout

-- Mode toggle

data Mode = View | Edit
	deriving (Read, Show, Eq)

toggle :: Mode -> Mode
toggle View = Edit
toggle Edit = View

canEdit :: Mode -> Bool
canEdit View = False
canEdit Edit = True
