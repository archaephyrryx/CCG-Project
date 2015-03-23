module Renderer.Core where

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import qualified Graphics.UI.Threepenny.Attributes as UI

type Rendered = UI Element
type Rendered' = [UI Element]

img :: Rendered
img = UI.img

span :: Rendered
span = UI.span

div :: Rendered
div = UI.div
