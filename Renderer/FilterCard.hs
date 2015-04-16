{-# LANGUAGE RecordWildCards #-}

module Renderer.FilterCard where

import CCG
--------------------------
import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
-------------------------
import Util
-------------------------
import Renderer.Core
import Renderer.Cards
-----------------------------

render :: UniCard c => [c] -> (UCR) -> Rendered
render matches pronounce =
  let theader = tr #+ (map ((th#$).string) ["#", "Rarity", "Type", "Cost", "Req.", "Name", "Power"])
      trows = for matches (\c ->
          tr #+ [ td #$ (string $ genset g)
                , td #$ (string $ brief rar)
                , td #: iconic ctype
                , td #$ (string $ fromMaybe "" (show.val <$> mcost))
                , td #: reqtify g
                , td #: pronounce g
                , td #: empower g
                ])
  in table #+ (theader:trows)
