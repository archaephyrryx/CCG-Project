module App.Core.Helper where

import Renderer.Core
import Util

glue :: Rendered
glue = string " "


pss :: Show a => Behavior (a -> UI Element)
pss = pure (string . show)

psss :: (Show a, Enum a) => Behavior (a -> UI Element)
psss = pure (string . show . succ)

plss :: Show a => Behavior (a -> UI Element)
plss = pure (estring li . show)
settext = set text

noop :: UI Element
noop = a
hlink :: String -> String -> Rendered
hlink url str = a # set href url # settext str


estring :: Builder -> String -> Rendered
estring el = (el #$).string

bstring = estring b
istring = estring i
