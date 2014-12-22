{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
    DeriveDataTypeable, GeneralizedNewtypeDeriving,
	RecordWildCards, TemplateHaskell, TypeFamilies,
	OverloadedStrings #-}

module CCGServer.Filtering where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.IORef
import Data.List
import Data.IxSet
import Data.Data (Data, Typeable)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified	Graphics.UI.Threepenny 			as UI
import				Graphics.UI.Threepenny.Core

setup :: Window -> UI ()
setup window = do
	-- GUI elements
	return window # set UI.title "Hello World!"

	elType	<- UI.input  # set (attr "placeholder") "Type"
	elColo	<- UI.input  # set (attr "placeholder") "Color"
	elCler	<- UI.button # set UI.text "Clear All"
	result	<- UI.span

	-- GUI Layout
	
	let dats = entries

	let
		displayAll = void $ do
			ftype <- get value elType
			fcolo <- get value elColo
			element result # set text (unlines (zipWith prepend ["Type: ","Color: "] [ftype,fcolo]))
		
		redoLayout :: UI ()
		redoLayout = void $ do
			ftype <- get value elType
			fcolo <- get value elColo
			layout <- mkLayout []
			getBody window # set children [layout]
			displayAll
		
		mkLayout :: [Element] -> UI Element
		mkLayout xs = column $
			([row [element elCler], UI.hr, row [element elType, element elColo]]
			++ (concatMap mkLine xs) ++ [row [element result]])
			
		mkLine :: Element -> [UI Element]
		mkLine x = [row [element x]]

		clearAll :: UI ()
		clearAll = do
			element elType # set value ""
			element elColo # set value ""
			redoLayout

	on (domEvent "livechange") elType $ \_ -> displayAll
	on (domEvent "livechange") elColo $ \_ -> displayAll
	on UI.click elCler $ const $ do
		clearAll
		redoLayout
	redoLayout

prepend :: String -> String -> String
prepend x "" = ""
prepend x y  = x++y


-- IxSets

data Entry = Entry Name Type Color
newtype Type = Type !String
newtype Color = Color !String
newtype Name = Name !String

showName :: Entry -> String
showName (Entry (Name x) _ _) = x


instance Indexable Entry where
	empty = ixSet
			  [ ixGen (Proxy :: Proxy Type)
			  , ixGen (Proxy :: Proxy Color)
			  , ixGen (Proxy :: Proxy Name)
			  ]

els = (\x y -> Entry (Type x) (Color y)) <$> ["Main","Event","Spell"] <*> ["Black","White","Grey"]
entries = foldr insert empty (els)
