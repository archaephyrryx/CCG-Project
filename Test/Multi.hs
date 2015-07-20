{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RecursiveDo #-}

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
		{ jsPort	= Just 10000
		, jsStatic	= Just "../wwwroot/"
		} setup

setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title "Test"
    let bAnthology = pure anthology

    rec (monoShake, c) <- monoSelectVDC bAnthology bSel (pure string) (string "Pick a Shakespeare")
        bSel <- stepper ([]) $ head <$> unions
          [ rumors . userSelections $ monoShake
          , [] <$ UI.click c
          ]
    rec (monoShake', c') <- monoSelectIDC bAnthology bSel' (pure string) (string "Pick a Shakespeare")
        bSel' <- stepper ([]) $ head <$> unions
          [ rumors . userSelections $ monoShake'
          , [] <$ UI.click c'
          ]
    rec (monoShake'', c'') <- monoSelectHDC bAnthology bSel'' (pure string) (string "Pick a Shakespeare")
        bSel'' <- stepper ([]) $ head <$> unions
          [ rumors . userSelections $ monoShake''
          , [] <$ UI.click c''
          ]
    rec (monoShake''', c''') <- monoSelectSVC bAnthology bSel''' (pure string)
        bSel''' <- stepper ([]) $ head <$> unions
          [ rumors . userSelections $ monoShake'''
          , [] <$ UI.click c'''
          ]

    let displayMulti = grid [[ row [element monoShake, element c], row [element monoShake', element c']],[ row [ element monoShake'', element c''], row [ element monoShake''', element c''']]]

    getBody window #+ [displayMulti]
   

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
