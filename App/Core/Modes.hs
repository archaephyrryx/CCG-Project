{-# LANGUAGE RecordWildCards #-}

module App.Core.Modes where

import App.Filtering
import Cards.Generic
import Cards
import Deck

data AppMode = HomeMode
             | FilterCardMode Filter ViewMode
             | ShowCardMode GenCard
             | DeckBuilderMode Filter ViewMode Deck
     deriving (Show, Eq)

data AppModeS = Home | FilterCard | ShowCard | DeckBuilder
    deriving (Read, Show, Eq)

modes :: AppMode -> AppModeS
modes HomeMode = Home
modes (FilterCardMode _ _) = FilterCard
modes (ShowCardMode _) = ShowCard
modes (DeckBuilderMode _ _ _) = DeckBuilder

data ViewMode = ListView { n :: Int, pn :: Int }
              | GridView { rs  :: Int, cs :: Int, pn :: Int }
                deriving (Read, Show, Eq)

npp :: ViewMode -> Int
npp l@ListView{n=x,..} = x
npp g@GridView{..} = rs * cs

setpage :: Int -> ViewMode -> ViewMode
setpage x ListView{..} = let pn = x in ListView{..}
setpage x GridView{..} = let pn = x in GridView{..}

incpage :: ViewMode -> ViewMode
incpage ListView{pn=x,..} = let pn = x + 1 in ListView{..}
incpage GridView{pn=x,..} = let pn = x + 1 in GridView{..}

decpage :: ViewMode -> ViewMode
decpage ListView{pn=x,..} = let pn = max (x - 1) 0 in ListView{..}
decpage GridView{pn=x,..} = let pn = max (x - 1) 0 in GridView{..}
