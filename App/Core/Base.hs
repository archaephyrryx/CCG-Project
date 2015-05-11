{-# LANGUAGE OverloadedStrings #-}
module App.Core.Base where

import Renderer.Core hiding (nav)
import qualified Renderer.Core as R (nav)

template :: String -> Rendered' -> Rendered
template pagename content =
    html #+ [
      head #+ [ title #$ string pagename
              , meta ! set httpEquiv "Content-Type"
                     ! set content "text/html;charset=UTF-8"
              , meta ! set charset "utf-8"
              , link ! set rel "stylesheet"
                     ! set href "/res/style.css"
              , meta ! set name "description"
                     ! set content "My Little Pony CCG Metadatabase"
              , meta ! set name "keywords"
                     ! set content "my little pony, mlp, ccg, tcg, enterplay, tabletop, brony"
              ]
    , body #$ content
    ]

base :: String -> [Rendered] -> Rendered
base pagename = template pagename . morph . (nav:)

nav :: Rendered
nav = R.nav #+ [ a # set href "/home" #: logo
               , a # set href "/card" #$ string "Cards"
               , a # set href "/deck" #$ string "Deck Builder"
               , form #: input # set type_ "search" # set placeholder "Search"
               ]
    where
        logo = img #. "logo" # set src "/res/logo.png"
