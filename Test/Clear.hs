{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RecursiveDo #-}

module Test where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.IORef
import Data.List
import Data.IxSet
import App.Core
import qualified Data.Map as Map
import Data.Map (Map)
import qualified	Graphics.UI.Threepenny 			as UI
import				Graphics.UI.Threepenny.Core
import App.Widgets

main :: IO ()
main = do
	startGUI defaultConfig
		{ jsPort	= Just 10000
		, jsStatic	= Just "res/"
		} setup

setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title "Test"
    let bAnthology = pure anthology
    
    manFlush <- UI.button # settext "Flush"

    rec (multiShake,clearShake) <- multiSelect bAnthology bSel (plss)
        let eShake = rumors . userSelections $ multiShake
            eClear = UI.click clearShake
        bSel <- stepper ([]) $ head <$> unions [ eShake, [] <$ eClear ]
    
    shower <- UI.li
    element shower # sink (mapschildren (estring UI.li)) (facts . userSelections $ multiShake)

    let
        clearAllSel :: UI ()
        clearAllSel = void $ do
            element multiShake # set clearSels ()
            

    on UI.click manFlush $ \_ -> clearAllSel
    on UI.click clearShake $ \_ -> clearAllSel

    let displayMulti = [element multiShake, element clearShake, element manFlush, element shower]
    getBody window #+ displayMulti
   

anthology :: [String]
anthology = [ "Hamlet"
            , "Macbeth"
            , "King Lear"
            , "A Midsummer Night's Dream"
            , "Much Ado About Nothing"
            , "A Comedy of Errors"
            , "Othello"
            , "Romeo and Juliet"
            , "The Merchant of Venice"
            , "Richard III"
            ]
