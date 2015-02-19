{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
    DeriveDataTypeable, GeneralizedNewtypeDeriving,
	RecordWildCards, TemplateHaskell, TypeFamilies,
	OverloadedStrings #-}

module App.Filtering where

import App.Core.AppData
import App.Core.Helper
import App.Widgets
import Cards
import Cards.Common
import Cards.Generic
import Control.Applicative
import Control.Applicative.Indexed  ( IndexedFunctor(..) , IndexedApplicative(..))
import Control.Monad
import Database
import Data.Char
import Data.Data (Data, Typeable)
import Data.IORef
import Data.IxSet
import Data.List
import Data.Maybe
import MLPCCG
import qualified Graphics.UI.Threepenny          as UI
import qualified Graphics.UI.Threepenny.Core     as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import Graphics.UI.Threepenny.Core

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

betwixt :: (Hint a, Typeable a) => (Maybe a, Maybe a) -> (IxSet GenCard -> IxSet GenCard)
betwixt (x,y) = (getGTE?x) . (getLTE?y)

full :: [a] -> Bool
full [] = False
full _ = True

-- | The 'Maybe Hint' Filter
mhfilter :: Filter -> IxSet GenCard -> IxSet GenCard
mhfilter c@CardFilter{..} = betwixt (powMin,powMax) . betwixt (costMin,costMax) . betwixt (reqMin,reqMax)

-- | The 'Multi-choice' Filter
mcfilter :: Filter -> IxSet GenCard -> IxSet GenCard
mcfilter c@CardFilter{..} = foldr (.) id [flip(@+)??colors, flip(@+)??sets, flip(@+)??types, flip(@+)??rarities]
mcfilter d@DeckFilter{..} = foldr (.) id [flip(@+)??colors, flip(@+)??sets, flip(@+)??types, flip(@+)??rarities]

-- | The Filter applicator
applyFilter :: Filter -> IxSet GenCard
applyFilter c@CardFilter{..} = mcfilter c . mhfilter c $ cardDB
applyFilter d@DeckFilter{..} = mcfilter d $ cardDB

behaveBFilter :: ABL -> Behavior Filter
behaveBFilter bls@FCBL{..} = CardFilter <$> bPowMin  <*> bPowMax
                                        <*> bCostMin <*> bCostMax
                                        <*> bReqMin  <*> bReqMax
                                        <*> bColSelect
                                        <*> bSetSelect
                                        <*> bTypSelect
                                        <*> bRarSelect
behaveBFilter bls@DBBL{..} = DeckFilter <$> bColSelect 
                                        <*> bSetSelect
                                        <*> bTypSelect
                                        <*> bRarSelect
