{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DoRec #-}

module Test where

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

main :: IO ()
main = do
	startGUI defaultConfig
		{ tpPort	= Just 10000
		, tpStatic	= Just "../wwwroot/"
		} setup

setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title "Test"

    let bAnthology = pure anthology 

    rec (multiShake, cBut) <- multiSelect (pure False) bAnthology bmShakes (pure ((UI.li #+).(:[]).string))
        let tShakes = userSelections multiShake
            eShakes = rumors tShakes
            bShakes = facts tShakes
            eClear = UI.click cBut
        bmShakes <- stepper ["Hamlet"] $ head <$> unions [eShakes, [] <$ eClear]

    let displayMulti = [ row [UI.bold #+ [string "Shakespeares"], element cBut]
                 , row [element multiShake]
                 ]

    getBody window # set schildren (displayMulti)
   

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
