module App.Core.Helper where

import Renderer.Core
import Util

glue :: Rendered
glue = string " "

settext = set text

hlink :: String -> String -> Rendered
hlink url str = a # set href url # settext str

noop :: Rendered
noop = a #+ []

estring :: Builder -> String -> Rendered
estring el = (el #$).string

bstring = estring b
istring = estring i
