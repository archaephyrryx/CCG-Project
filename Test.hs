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

    iShakespeare <- liftIO $ newIORef 0
    butFoo <- UI.button # set text "Foo"
    rec butSoft <- accumB 0 $ (+1) <$ UI.click butFoo
        let shakes = take <$> butSoft <*> (pure ["Hamlet", "Macbeth", "King Lear", "A Midsummer Night's Dream", "Much Ado About Nothing", "A Comedy of Errors", "Othello", "Romeo and Juliet", "The Merchant of Venice", "Richard III"])
        softBar <- liquidLink ((++" Shakespeares").show) (butSoft)
        multiShake <- multiSelect "Shakespeares: " True shakes bmShakes (pure ((UI.li #+).(:[]).string))
        let tShakes = userSelections multiShake
            eShakes = rumors tShakes
            bShakes = facts tShakes
        bmShakes <- stepper [] eShakes

    let
      tShakes = userSelections multiShake

      redoLayout :: UI ()
      redoLayout = void $ do
          shake <- liftIO $ readIORef iShakespeare
          layout <- mkLayout shake []
          getBody window # set children [layout]
    
      mkLayout :: Int -> [Element] -> UI Element
      mkLayout n _ = column ([row [element butFoo], row [element softBar], row [element multiShake]]++([UI.ul #+ (map ((UI.li #+).(:[]).string) (take n ["Hamlet", "Macbeth", "King Lear", "A Midsummer Night's Dream", "Much Ado About Nothing", "A Comedy of Errors", "Othello", "Romeo and Juliet", "The Merchant of Venice", "Richard III"]))]))

      foo :: UI ()
      foo = softBar`sinksTo`(\x -> do liftIO $ writeIORef iShakespeare x
                                      redoLayout)

    redoLayout >> foo
