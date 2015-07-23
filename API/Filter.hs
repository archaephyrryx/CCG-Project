module API.Filter (
    -- * Filter
    Filter(..), blankCardFilter, blankDeckFilter,
    -- * Filtering Functions
    -- ** Transformers
    -- | Functions that take a "Filter" and return a transformation of an "GenCard"-"IxSet"
    betwixt, mhfilter, mcfilter, 
    -- ** Applicators
    applyFilter
    ) where

import CCG
import Control.Applicative
import API.Database
import Data.Char
import Data.IxSet
import Data.List
import Data.Maybe
import Data.Typeable (Typeable)
import Util

-- | A data type representing any kind of filter on cards, with
-- constructors for the different query types.
data Filter = CardFilter -- ^ Constructor for filtering cards when browsing
                { powMin :: Maybe Power -- ^ Optional minimum power value
                , powMax :: Maybe Power -- ^ Optional maximum power value
                , costMin :: Maybe Cost -- ^ Optional minimum cost value
                , costMax :: Maybe Cost -- ^ Optional maximum cost value
                , reqMin :: Maybe Req -- ^ Optional minimum requirement value
                , reqMax :: Maybe Req -- ^ Optional maximum requirement value
                , colors :: [Color] -- ^ Permitted colors, all if empty
                , sets :: [CSet] -- ^ Permitted card sets, all if empty
                , types :: [CardType] -- ^ Permitted card types, all if empty
                , rarities :: [Rarity] -- ^ Permitted rarities, all if empty
                }
             | DeckFilter -- ^ Constructor for filtering cards when building decks
                { colors :: [Color] -- ^ Permitted colors, all if empty
                , sets :: [CSet] -- ^ Permitted card sets, all if empty
                , types :: [CardType] -- ^ Permitted card types, all if empty
                , rarities :: [Rarity] -- ^ Permitted rarities, all if empty
                }
        deriving (Eq, Ord, Read, Show)

-- | ADT representing the constructor type of a filter
data FilterType = CFilter -- ^ Unary constructor for CardFilter type
                | DFilter -- ^ Unary cosntructor for DeckFilter type
                deriving (Eq, Ord, Read, Show)

-- |Constructor reflection for Filters
ftype :: Filter -> FilterType
ftype (CardFilter _ _ _ _ _ _ _ _ _ _) = CFilter
ftype (DeckFilter _ _ _ _) = DFilter

-- | Elementary CardFilter that is all-inclusive
blankCardFilter, blankDeckFilter :: Filter
(blankCardFilter, blankDeckFilter) =
    let f = ($Nothing)
        g = ($[])
    in tmup (g.g.g.g) $ f.<f.<f.<f.<f.<f$<(CardFilter, DeckFilter)

-- | Query on an "IxSet" of "GenCard"s for a min-max "Hint"-value range
betwixt :: (Hint a, Typeable a) => (Maybe a, Maybe a) -> (IxSet GenCard -> IxSet GenCard)
betwixt (x,y) = (getGTE?x) . (getLTE?y)

-- | Queries for hint-values within the ranges specified by a Filter
mhfilter :: Filter -> IxSet GenCard -> IxSet GenCard
mhfilter =
  ftype.=CFilter?$hfilter
  where
    hfilter :: Filter -> IxSet GenCard -> IxSet GenCard
    hfilter = procmap [ betwixt.tap (powMin, powMax)
                      , betwixt.tap (costMin, costMax)
                      , betwixt.tap (reqMin, reqMax)
                      ]

-- | Queries for choice-values among the options specified by a Filter
mcfilter :: Filter -> IxSet GenCard -> IxSet GenCard
mcfilter = procmap [flip(@+)??.colors, flip(@+)??.sets, flip(@+)??.types, flip(@+)??.rarities]

-- | The Filter applicator
applyFilter :: Filter -> IxSet GenCard
applyFilter = ($cardDB) . ((.) <$> mcfilter <*> mhfilter)
