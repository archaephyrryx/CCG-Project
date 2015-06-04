{-# LANGUAGE OverloadedStrings #-}
module Snaplets where

import Snap.Snaplet

data Config = Config
    { n :: Int -- ^ Number of cards to display per page (list view)
    , rs :: Int -- ^ Number of card rows to display per page (grid view)
    , cs :: Int -- ^ Number of card columns to display (grid view)
    }

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
