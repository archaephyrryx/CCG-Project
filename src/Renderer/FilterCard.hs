{-# LANGUAGE OverloadedStrings #-}
module Renderer.FilterCard where

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
import           API.Filter
import           Renderer.Cards
import           Renderer.Core
import           Data.IxSet


resultSplice :: Filter -> Splices (SnapletISplice App)
resultSplice f = let cards = toList . applyFilter $ f
                 in "cardrows" ## (I.mapSplices (I.runChildrenWith . cardSplice) cards)

pronounce :: UniCard c => c -> String
pronounce = uname

cardSplice :: UniCard c => c -> Splices (SnapletISplice App)
cardSplice c = do
    "setnum" ## string . setnum $ c
    "rarity" ## string . brief . urar $ c
    "typeicon" ## I.callTemplate "_typeicon" (iconSplice . utype $ c)
    "cost"  ## string (fromMaybe "" (show.val <$> ucost c))
    "req"   ## reqtify $ c
    "cname" ## string . pronounce $ c
    "power" ## empower $ c
