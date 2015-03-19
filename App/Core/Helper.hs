module App.Core.Helper where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Elements as UI
import Reactive.Threepenny
import Util

glue :: UI Element
glue = string (" " :: String)

hlink :: String -> String -> UI Element
hlink url str = UI.a # UI.set UI.href url # settext str

pss :: Show a => Behavior (a -> UI Element)
pss = pure (string . show)

psss :: (Show a, Enum a) => Behavior (a -> UI Element)
psss = pure (string . show . succ)

plss :: Show a => Behavior (a -> UI Element)
plss = pure (estring UI.li . show)

noop :: UI Element
noop = UI.a

estring :: UI Element -> String -> UI Element
estring el = (el #+).(:[]).string

bstring = estring UI.bold
istring = estring UI.italics

settext = set text
