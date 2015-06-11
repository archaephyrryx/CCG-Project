{-# LANGUAGE TemplateHaskell #-}
module Renderer.Core (
    module Renderer.Core.Internal
  , module Renderer.Core
  ) where

import Control.Applicative
import Text.Blaze (ToValue)
---------------------------------------------------
import Util
import Language.Haskell.TH
import Renderer.Core.Internal
import Prelude hiding (div, span)

infixl 8 #+
infixl 8 #.
infixl 8 #
infixl 8 !
infixr 7 #:
infixl 7 #$

--- Operators
 
set :: ToValue v => String -> v -> (a, Mattrs) -> (a, Mattrs)
set a v = (>$(atval a v))

(#) :: a -> (a -> b) -> b
(#) = flip ($)

(!) :: a -> (a -> b) -> b
(!) = flip ($)

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
$(makeAttr' ("type_", "type"))
$(makeAttr' ("id_", "id"))
$(makeAttr' ("for_", "for"))
$(makeAttr "placeholder")
$(makeAttr "min")
$(makeAttr "step")
$(makeAttr "pattern")
$(makeAttr "value")


--- Backbone

$(makeElement "html")
$(makeElements ["head", "title"])
$(makeElement "body")
$(makeVacuum "meta")
$(makeVacuum "link")

$(makeAttr' ("httpEquiv", "http-equiv"))
$(makeAttr' ("content_", "content"))
$(makeAttrs ["charset","rel","name"])
