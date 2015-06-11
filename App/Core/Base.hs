module App.Core.Base where

import Renderer.Core hiding (nav)
import qualified Renderer.Core as R (nav)
import Util hiding (body)
import Prelude hiding (span, div, head)

template :: String -> Rendered' -> Rendered
template pagename content =
    html #+ [
      head #+ [ title #$ string pagename
              , meta ! set httpEquiv "Content-Type"
                     ! set content_ "text/html;charset=UTF-8"
                     ! zap
              , meta ! set charset "utf-8"
                     ! zap
              , link ! set rel "stylesheet"
                     ! set href "/res/style.css"
                     ! zap
              , meta ! set name "description"
                     ! set content_ "My Little Pony CCG Metadatabase"
                     ! zap
              , meta ! set name "keywords"
                     ! set content_ "my little pony, mlp, ccg, tcg, enterplay, tabletop, brony"
                     ! zap
              ]
    , body #$ content
    ]

base :: String -> [Rendered] -> Rendered
base pagename = template pagename . morph . (nav:)

content :: Renderer Rendered'
content = (div #. "content" #$)

sidebar :: Renderer Rendered'
sidebar = (div #. "side-bar" #$)

nav :: Rendered
nav = R.nav #+ [ a # set href "/home" #: logo
               , a # set href "/card" #$ string "Cards"
               , a # set href "/deck" #$ string "Deck Builder"
               , form #: input # set type_ "search" # set placeholder "Search" # zap
               ]
    where
        logo = img #. "logo" # set src "/res/logo.png" # zap
