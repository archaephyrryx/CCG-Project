{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Splices where

---------------------------------------------
import qualified Data.Text as T
import           Control.Applicative
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Data.Maybe
import           Control.Monad
import           Heist
import qualified Heist.Interpreted as I
---------------------------------------------
import           Application
import           CCG hiding (render)

cardRowSplice :: Splices (SnapletISplice App)
cardRowSplice = do
    items <- getItems
    "splicer" ## (I.mapSplices (I.runChildrenWithText . itemSplice) items)

itemSplice i = "spliced" ## i
