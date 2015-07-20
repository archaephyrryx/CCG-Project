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
import App.Widgets.Ranger
import App.Widgets.MultiSelect

main :: IO ()
main = do
	startGUI defaultConfig
		{ jsPort	= Just 10000
		, jsStatic	= Just "../wwwroot/"
		} setup

setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title "Test"

    iShakespeare <- liftIO $ newIORef 0
    butFoo <- UI.button # set text "Once More into the Breach"
    shaken <- UI.li
    sonnet <- UI.h1

    rec butSoft <- accumB 1 $ (+1) <$ UI.click butFoo
        let bAnthology = pure anthology
            butWhich = (!!) <$> (cycle <$> bAnthology) <*> butSoft
            shakes = take <$> butSoft <*> bAnthology
        softBar <- liquidLink (pure ((++" Shakespeares").show)) (butSoft)
        (multiShake, cBut) <- multiSelect bAnthology bmShakes (pure ((UI.li #+).(:[]).string))
        stRanger <- ranger bAnon bFirst bLast (pure (string.show)) 
        let tRanger = userLoc stRanger
            eRanger = rumors tRanger
            bRanger = facts tRanger
            bFirst = pure 0
            bLast = (pred) <$> butSoft
        bAnon <- stepper 0 $ eRanger
        let tShakes = userSelections multiShake
            eShakes = rumors tShakes
            bShakes = facts tShakes
            eClear = UI.click cBut
        bmShakes <- stepper ["Hamlet"] $ head <$> unions
          [ eShakes
          , [] <$ eClear
          ]
    element shaken # sink curview (map <$> (pure ((UI.li #+).(:[]).string)) <*> bmShakes)
    element sonnet # sink UI.text ((\xs i -> show $ (xs!!i)) <$> shakes <*> bRanger)

    let

      redoLayout :: UI ()
      redoLayout = void $ do
          shake <- liftIO $ readIORef iShakespeare
          layout <- mkLayout shake []
          getBody window # set children [layout]

      uiShaken = column [ row [UI.bold #+ [string "Shakespeares"], element cBut]
                        , row [element multiShake]
                        , row [element shaken]
                        , row [element sonnet]
                        , row [element stRanger]
                        ]
    
      mkLayout :: Int -> [Element] -> UI Element
      mkLayout n _ = column ([row [element butFoo], row [element softBar], row [uiShaken]]++([UI.ul #+ (map ((UI.li #+).(:[]).string) (take n anthology))]))

      foo :: UI ()
      foo = softBar`sinksTo`(\x -> do liftIO $ writeIORef iShakespeare x
                                      redoLayout)

    redoLayout >> foo


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
