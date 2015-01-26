{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
    DeriveDataTypeable, GeneralizedNewtypeDeriving,
	RecordWildCards, TemplateHaskell, TypeFamilies,
	OverloadedStrings #-}

module CCGServer.Filtering where

import Cards
import Cards.Common
import Cards.Common.Abbrev
import Cards.Common.Color
import Cards.Common.Hint
import Cards.Common.Stringe
import Cards.Common.Values
import Cards.Generic
import Control.Applicative
import Control.Monad
import Database
import Data.Char
import Data.Data (Data, Typeable)
import Data.IORef
import Data.IxSet
import Data.List
import Data.Maybe
import MLPCCG

infix 9 ?
infix 9 ??

data Filter = CardFilter
                { powMin :: Maybe Power
                , powMax :: Maybe Power
                , costMin :: Maybe Cost
                , costMax :: Maybe Cost
                , reqMin :: Maybe Req
                , reqMax :: Maybe Req
                , colors :: [Color]
                , sets :: [CSet]
                , types :: [CardType]
                , rarities :: [Rarity]
                }
             | DeckFilter
                { colors :: [Color]
                , sets :: [CSet]
                , types :: [CardType]
                , rarities :: [Rarity]
                }
        deriving (Eq, Ord, Read, Show)

blankCardFilter :: Filter
blankCardFilter = let [powMin,powMax] = replicate 2 Nothing
                      [costMin,costMax] = replicate 2 Nothing
                      [reqMin,reqMax] = replicate 2 Nothing
                      colors = []
                      sets = []
                      types = []
                      rarities = []
                  in CardFilter{..}

blankDeckFilter :: Filter
blankDeckFilter = let colors = []
                      sets = []
                      types = []
                      rarities = []
                  in DeckFilter{..}

(?) :: (a -> (b -> b)) -> Maybe a -> (b -> b)
f?Nothing = id
f?_ = f

(??) :: ([a] -> (b -> b)) -> [a] -> (b -> b)
f??[]=id
f??_ =f

betwixt :: Hint a => (Maybe a, Maybe a) -> IxSet GenCard -> IxSet GenCard
betwixt (x,y) = flip getGTE ? x . flip getLTE ? y

mhfilter :: Filter -> IxSet GenCard -> IxSet GenCard
mhfilter c@CardFilter{..} = betwixt (powMin,powMax) . betwixt (costMin,costMax) . betwixt (reqMin,reqMax)

full :: [a] -> Bool
full [] = False
full _ = True

mcfilter :: Filter -> IxSet GenCard -> IxSet GenCard
mcfilter c@CardFilter{..} = flip(@+)??colors . flip(@+)??sets . flip(@+)??types . flip(@+)??rarities
mcfilter d@DeckFilter{..} = flip(@+)??colors . flip(@+)??sets . flip(@+)??types . flip(@+)??rarities

applyFilter :: Filter -> IxSet GenCard
applyFilter c@CardFilter{..} = mcfilter c . mhfilter c $ cardDB
applyFilter d@DeckFilter{..} = mcfilter d $ cardDB
