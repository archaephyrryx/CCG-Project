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

  --- General things

    let bAnthology = pure anthology

    iPlay <- liftIO $ newIORef "Much Ado About Nothing"

  --- Modes!
    butHome <- UI.button # set text "Home"
    butRanger <- UI.button # set text "Ranger"
    butMulti <- UI.button # set text "MultiSelect"
    butLinks <- UI.button # set text "Links"

    let eHome = UI.click butHome
        eRanger = UI.click butRanger
        eMulti = UI.click butMulti
        eLinks = UI.click butLinks

        navigator :: [UI Element]
        navigator = [element butHome, element butRanger, element butMulti, element butLinks]

    bMode <- stepper "Home" $ head <$> unions
        [ "Home" <$ eHome
        , "Ranger" <$ eRanger
        , "Multi" <$ eMulti
        , "Links" <$ eLinks
        ]

  --- Mode specific things (ranger)
    butFoo <- UI.button # set text "Once More into the Breach"

    rec stRanger <- ranger bAnon bFirst bLast (pure (string.show)) 
        butSoft <- accumB 1 $ (+1) <$ UI.click butFoo
        let tRanger = userLoc stRanger
            eRanger = rumors tRanger
            bRanger = facts tRanger
            bFirst = pure 0
            bLast = (pred) <$> butSoft
            shakes = take <$> butSoft <*> bAnthology
            butWhich = (!!) <$> (cycle <$> bAnthology) <*> butSoft
        bAnon <- stepper 0 $ eRanger

    sonnet <- UI.h1
    element sonnet # sink UI.text ((\xs i -> show $ (xs!!i)) <$> shakes <*> bRanger)

  --- Mode specific things (multi)
    rec (multiShake, cBut) <- multiSelect (pure True) bAnthology bmShakes (pure ((UI.li #+).(:[]).string))
        let tShakes = userSelections multiShake
            eShakes = rumors tShakes
            bShakes = facts tShakes
            eClear = UI.click cBut
        bmShakes <- stepper ["Hamlet"] $ head <$> unions [eShakes, [] <$ eClear]

  --- Mode specific things (links)
    bMacbeth <- softLink "The Scottish Play" ("Macbeth")
    bHamlet  <- softLink "The Danish Play" ("Hamlet")
    bPlay <- liquidLink (pure ("Shakespeare's "++)) butWhich

    --- UI actions

    shaken <- UI.li
    element shaken # sink curview (map <$> (pure ((UI.li #+).(:[]).string)) <*> bmShakes)

    headlight <- UI.h1
    element headlight # sink text (bMode)

    footlight <- UI.span
    element footlight # sink schildren ((:[]).string.("A Taste of "++) <$> bMode)


    let
        displayMode :: String -> [UI Element]
        displayMode mod = case mod of
            "Home" -> []
            "Ranger" -> displayRanger
            "Multi" -> displayMulti
            "Links" -> displayLinks 
        
        displayRanger = [ row [element butFoo]
                        , row [element sonnet]
                        , row [element stRanger]
                        ]
        displayMulti = [ row [UI.bold #+ [string "Shakespeares"], element cBut]
                       , row [element multiShake]
                       , row [element shaken]
                       ]

        displayLinks = [ row [element bMacbeth, element bHamlet, element bPlay] ]

    content <- UI.div
    element content # sink schildren (displayMode <$> bMode)

    getBody window # set schildren ([column [ row navigator , row [ element headlight ], row [ element content ], row [ element footlight ] ]])
   

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
