module App.Home where

import Widgets.Core

appname = "FRPony"

homeText :: String
homeText = unlines $
    [ "Welcome to "++appname++"! This is an in-development window-based GUI for the MLP:CCG, borrowing somewhat from PonyHead."
    , "This is very much in-progress, so don't expect fully functional or reliable performance, but thanks for helping test this!"
    , "Any comments, bug reports, questions, feature requests, or other feedback should go to the GitHub page for this project."
    , "A number of other implementations of this app are being developed as well, though they go by different names."
    ]

welcomeText :: Window w => w -> IO (StaticText ())
welcomeText w = staticText w [ text := homeText ]
