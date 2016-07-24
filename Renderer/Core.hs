{-# LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Renderer.Core (
    module Renderer.Core.Internal
  , module Renderer.Core
  ) where

import Control.Applicative
---------------------------------------------------
import Util
import Language.Haskell.TH
import Renderer.Core.Internal
import Prelude hiding (div, span)

infixl 8 #+
infixl 8 #.
infixl 8 #
infixr 7 #:
infixl 7 #$

--- Operators

set :: String -> String -> (a, Mattrs) -> (a, Mattrs)
set a v = ((atval a v)>$)

(#) :: a -> (a -> b) -> b
(#) = flip ($)

(#.) :: (a, Mattrs) -> String -> (a, Mattrs)
(#.) b s = b # set "class" s

(#+) :: Builder -> [Rendered] -> Rendered
(#+) (renr,attrs) rens = renr attrs $ morph rens

(#$) :: Builder -> Rendered' -> Rendered
(#$) (renr,attrs) ren' = renr attrs ren'

(#:) :: Builder -> Rendered -> Rendered
(#:) (renr,attrs) rens = renr attrs $ orph rens

--- Elements

$(makeElement "a")
$(makeElement "p")
$(makeElement "span")
$(makeElement "div")
$(makeElement "nav")
$(makeElement "form")
$(makeElements ((:) <$> "h" <*> (show <$> [1..6])))
$(makeElements ["table", "tr", "th", "td"])
$(makeElements ["dl", "dd", "dt"])
$(makeElements ["ul", "li"])
$(makeElements ["b", "i"])
$(makeElement "label")
$(makeVacuum "img")
$(makeVacuum "input")

--- Attributes

$(makeAttr "src")
$(makeAttr "href")
$(makeAttr "text")

--- Backbone

{-
$(makeElement "html")
$(makeElements ["head", "title"])
$(makeElement "body")
$(makeVacuum "meta")
$(makeVacuum "link")

$(makeAttr' ("httpEquiv", "http-equiv"))
$(makeAttrs ["charset","rel","name","content"])
-}
