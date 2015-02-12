{-# LANGUAGE RecordWildCards #-}

module App.Core.Modes where

import App.Filtering
import Cards.Generic
import Cards
import Deck

data AppMode = Home
             | FilterCard
             | ShowCard
             | DeckBuilder
     deriving (Read, Show, Eq)

data ViewMode = ListView { n :: Int, pn :: Int }
              | GridView { rs  :: Int, cs :: Int, pn :: Int }
              | FlatView { pn :: Int }
                deriving (Read, Show, Eq)

npp :: ViewMode -> Int
npp l@ListView{n=x,..} = x
npp g@GridView{..} = rs * cs
npp f@FlatView{..} = 1

setpage :: Int -> ViewMode -> ViewMode
setpage x ListView{..} = let pn = x in ListView{..}
setpage x GridView{..} = let pn = x in GridView{..}

incpage :: ViewMode -> ViewMode
incpage ListView{pn=x,..} = let pn = x + 1 in ListView{..}
incpage GridView{pn=x,..} = let pn = x + 1 in GridView{..}

decpage :: ViewMode -> ViewMode
decpage ListView{pn=x,..} = let pn = max (x - 1) 0 in ListView{..}
decpage GridView{pn=x,..} = let pn = max (x - 1) 0 in GridView{..}
