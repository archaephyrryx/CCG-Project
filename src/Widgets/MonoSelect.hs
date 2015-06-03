{-# LANGUAGE OverloadedStrings #-}
module Widgets.MonoSelect where

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
import           Renderer.Core
import           Widgets.Select

vdcSplice :: String -> (x -> String) -> [x] -> Splices (Splice App)
vdcSplice vdc f vs =
    let ivals = zip [1..] $ map f vs
    in do
        "vdc" ## string vdc 
        "choices" ## (I.mapSplices (I.runChildrenWith . choiceSplice) ivals)
