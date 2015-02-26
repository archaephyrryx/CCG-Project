{-# LANGUAGE RecordWildCards, OverloadedStrings, DoRec #-}
module App.Home where
------------------------------
import Control.Applicative
import Control.Monad
------------------------------
import Data.Data (Data, Typeable)
import Data.IORef
import Data.IxSet hiding (null)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
------------------------------
import App.FilterCard
--import App.SingleCard
import App.Renderer.FilterCard
import App.Renderer.SingleCard
import App.Renderer.Deck
import App.Core.AppData
import App.Core.Helper
import App.Core.Modes
--import App.Deck
import App.Filtering
import App.Widgets
------------------------------
import qualified Graphics.UI.Threepenny          as UI
import qualified Graphics.UI.Threepenny.Core     as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Elements (addStyleSheet)

appname = "ThreePony"
appdesc = "An Advanced MLP:CCG Toolkit"
appfname = (appname ++ ": " ++ appdesc)

welcomeText =
    [ "Welcome to ThreePony! This is an in-development browser-based GUI for the MLP:CCG, borrowing somewhat from PonyHead."
    , "This is very much in-progress, so don't expect fully functional or reliable performance, but thanks for helping test this!"
    , "Any comments, bug reports, questions, feature requests, or other feedback should go to the GitHub page for this project."
    , "A number of other implementations of this app are being developed as well, though they go by different names."
    ]

hlink :: String -> String -> UI Element
hlink url str = UI.a # UI.set UI.href url # settext str

glue :: UI Element
glue = string " "

homeFoot :: UI Element
homeFoot = UI.span #. "footer" #+ [ devel , code , proj ]
    where
        devel = UI.p #. "" [string "Developer:", glue, arch]
            where arch = hlink "https://github.com/archaephyrryx" "Archaephyrryx"
        code =  UI.p #. "" [string "Project code on", glue, gitpage]
            where gitpage = hlink "https://github.com/archaephyrryx/CCG-Project/tree/threepenny" "GitHub"
        proj = UI.p #. "" [string "Sister projects also on", glue, githome ]
            where githome = hlink "https://github.com/archaephyrr://github.com/archaephyrryx/CCG-Project/" "GitHub"
              
noop :: UI Element
noop = UI.a

hmHeader :: UI Element
hmHeader = noop

hmContent :: UI Element
hmContent = column ([ UI.h1 #+ [string appfname] ]++(map ((UI.p #+).(:[]).string) welcomeText))

hmFooter :: UI Element
hmFooter = element hooves

hmSideBar :: UI Element
hmSideBar = noop

hmDebugger :: UI Element
hmDebugger = row [string "This is the debugger! Anything you see here is special information used to debug the app."]
