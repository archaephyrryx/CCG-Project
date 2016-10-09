{-# LANGUAGE RecordWildCards #-}
module App.Home
    ( appname
    , appdesc
    , appfname
    , welcomeText
    , homeFoot
    ) where
------------------------------
import Control.Applicative
import Control.Monad
------------------------------
import Prelude hiding (span)
import Data.List hiding (span)
------------------------------
import App.Core
import Renderer.Core hiding (string)
import qualified Renderer.Core as R (string)

string :: Renderer String
string = head . R.string

appname = "ThreePony"
appdesc = "An Advanced MLP:CCG Toolkit"
appfname = (appname ++ ": " ++ appdesc)

welcomeText =
    [ "Welcome to "++appname++"! This is an in-development browser-based GUI for the MLP:CCG, borrowing somewhat from PonyHead."
    , "This is very much in-progress, so don't expect fully functional or reliable performance, but thanks for helping test this!"
    , "Any comments, bug reports, questions, feature requests, or other feedback should go to the GitHub page for this project."
    , "A number of other implementations of this app are being developed as well, though they go by different names."
    ]

homeFoot :: Rendered
homeFoot = span #. "footer" #+ [ devel , code , proj ]
    where
        base = "https://github.com/archaephyrryx"
        devel = p #. "dev" #+ [string "Developer:", glue, gitarch]
            where gitarch = hlink base "Archaephyrryx"
        code =  p #. "cod" #+ [string "Project code on", glue, gitpage]
            where gitpage = hlink (base++"/CCG-Project/tree/threepenny") "GitHub"
        proj = p #. "proj" #+ [string "Sister projects also on", glue, githome]
            where githome = hlink (base++"/CCG-Project") "GitHub"
