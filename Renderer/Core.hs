module Renderer.Core ( Rendered , Rendered') where

import Graphics.UI.Threepenny.Core (UI, Element)

type Rendered = UI Element
type Rendered' = [UI Element]
