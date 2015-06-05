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
          tr #+ [ td #$ (string . setnum $ c)
                , td #$ (string . brief . urar $ c)
                , td #: (iconic . utype $ c)
                , td #$ (string (fromMaybe "" (show.val <$> ucost c)))
                , td #: reqtify c
                , td #: pronounce c
                , td #: empower c
                ])
  in table #+ (theader:trows)
