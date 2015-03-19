{-# LANGUAGE RecordWildCards #-}
module App.Home where
------------------------------
import Control.Applicative
import Control.Monad
------------------------------
import Data.List
------------------------------
import App.Core
------------------------------
import qualified Graphics.UI.Threepenny          as UI
import qualified Graphics.UI.Threepenny.Core     as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import Graphics.UI.Threepenny.Core

appname = "ThreePony"
appdesc = "An Advanced MLP:CCG Toolkit"
appfname = (appname ++ ": " ++ appdesc)

welcomeText =
    [ "Welcome to ThreePony! This is an in-development browser-based GUI for the MLP:CCG, borrowing somewhat from PonyHead."
    , "This is very much in-progress, so don't expect fully functional or reliable performance, but thanks for helping test this!"
    , "Any comments, bug reports, questions, feature requests, or other feedback should go to the GitHub page for this project."
    , "A number of other implementations of this app are being developed as well, though they go by different names."
    ]

homeFoot :: UI Element
homeFoot = UI.span #. "footer" #+ [ devel , code , proj ]
    where
        base = "https://github.com/archaephyrryx"
        devel = UI.p #. "dev" #+ [string "Developer:", glue, gitarch]
            where gitarch = hlink base "Archaephyrryx"
        code =  UI.p #. "cod" #+ [string "Project code on", glue, gitpage]
            where gitpage = hlink (base++"/CCG-Project/tree/threepenny") "GitHub"
        proj = UI.p #. "proj" #+ [string "Sister projects also on", glue, githome ]
            where githome = hlink (base++"/CCG-Project") "GitHub"
