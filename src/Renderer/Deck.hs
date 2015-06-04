{-# LANGUAGE OverloadedStrings #-}
module Renderer.Deck where

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
import           Formal
import           Util.Conditional
import           Data.List.Split

nColumns :: Int
nColumns = 4

dfSplice :: Filter -> Splices (SnapletISplice App)
dfSplice f = do
  "deck-header" ## I.callTemplate "_deck" (noSplices)
  "deck-content" ## I.callTemplate "_deckc" (drSplice f)
  "deck-footer" ## I.callTemplate "_deckf" (noSplices)
  "deck-sidebar" ## I.callTemplate "_decks" (noSplices)

drSplice :: Filter -> Splices (SnapletISplice App)
drSplice f = let cards = chunksOf nColumns . toList . applyFilter $ f
             in "cards-rows" ## (I.mapSplices (I.runChildrenWith . cardsColSplice) cards)

cardsColSplice :: UniCard c => [c] -> Splices (SnapletISplice App)
cardsColSplice cs = "cards-columns" ## (I.mapSplices (I.runChildrenWith . setnumSplice) cs)

setnumSplice :: UniCard c => c -> Splices (SnapletISplice App)
setnumSplice c = "setnum" ## string . setnum $ c
