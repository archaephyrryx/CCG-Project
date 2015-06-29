{-# LANGUAGE OverloadedStrings #-}
module Widgets.Select where

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

choiceSplice :: (Int, String) -> Splices (Splice App)
choiceSplice (i, v) = do
    "num" ## string . show $ i
    "choice" ## string v
