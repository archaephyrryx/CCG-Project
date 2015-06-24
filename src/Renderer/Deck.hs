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

data DeckCtx = ManeCtx { dat :: DeckP, n :: Int }
             | ProbCtx { dat :: DeckP, n :: Int }
             | DrawCtx { dat :: DeckP, n :: Int }

partname :: DeckCtx -> String
partname (ManeCtx _ _)  = "mane"
partname (ProbCtx _ _ _) = "problem"
partname (DrawCtx _ _) = "draw"



constructSplice :: Deck -> Splices (SnapletISplice App)
constructSplice d = "deck-splices" ## (I.mapSplices (I.runChildrenWith . structureSplice) $ ctxs)
  where
    parts@(manes, probs, draws) = tpart d
    lens@(nmane,nprob,ndraw) = mhall (length,length,length) parts
    ctxs = [ ManeCtx manes nmane
           , ProbCtx probs nprob
           , DrawCtx draws ndraw
           ]

structureSplice :: DeckCtx -> Splices (SnapletISplice App)
structureSplice ctx = do
    "deckpart" ## string . partname $ ctx
    "partH" ## headerSplice ctx
    "card-lines" ## (I.mapSplices (I.runChildrenWith . conSplice) $ struct . dat $ ctx)

headerSplice :: DeckCtx -> Splices (SnapletISplice App)
headerSplice x = case x of
    (ManeCtx _ n) -> string $ "Manes ("++(show n)++")")
    (ProbCtx _ n) -> let s = hasStarting x
                     in ((s?:nonstarter) (string $ "Problem Deck ("++(show n)++"/10)"))
    (DrawCtx _ n) -> string $ "Draw Deck ("++(show n)++"/45)"

nonstarter :: Splices (SnapletISplice App)
nonstarter = do
    "class" ## string "no-start"
    "foo" ## "No Starting Problem!"

conSplice :: Unicard c => (c,Int) -> Splices (SnapletISplice App)
conSplice x = "card-line" ## ((accountSplice n $ ctab c, string . uname $ c)

accountSplice :: Int -> ?
accountSplice n = (("ccount badge" (string $ show n)])?+n)


ctab = cond (utype.=TProblem) conf empower
