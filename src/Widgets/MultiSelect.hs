{-# LANGUAGE OverloadedStrings #-}
module Widgets.MultiSelect where

import qualified Data.Text as T
import           Control.Applicative
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Data.Maybe
import           Data.Char
import           Control.Monad
import           Heist
import qualified Heist.Interpreted as I
---------------------------------------------
import           Application
import           Widgets.Select
import           Renderer.Core

multiSplice :: (x -> String) -> [x] -> Splices (Splice App)
multiSplice f vs =
    let ivals = zip [0..] $ map f vs
    in do
        "choices" ## (I.mapSplices (I.runChildrenWith . choiceSplice) ivals)
