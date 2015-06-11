{-# LANGUAGE RecordWildCards, RankNTypes #-}

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

theader :: Rendered
theader = tr #+ (map ((th#$).string) ["#", "Rarity", "Type", "Cost", "Req.", "Name", "Power"])

trow :: UCR -> UCR
trow pronounce c = 
    tr #+ [ td #$ (string . setnum $ c)
          , td #$ (string . brief . urar $ c)
          , td #: (iconic . utype $ c)
          , td #$ (string (fromMaybe "" (show.val <$> ucost c)))
          , td #: reqtify c
          , td #: pronounce c
          , td #: empower c
          ]
