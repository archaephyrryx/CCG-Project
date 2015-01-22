{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    TemplateHaskell, TypeFamilies, RecordWildCards,
	OverloadedStrings #-}

module Database where

import Cards
import Cards.Common
import Cards.Common.Hint
import Cards.Common.Stringe
import Cards.Common.Abbrev
import Cards.Common.Color
import Cards.Parser
import Cards.Generic
import Cards.Pretty
import MLPCCG
import Control.Applicative	( (<$>) )
import Control.Exception	( bracket )
import Control.Monad		( msum )
import Control.Monad.Reader	( ask )
import Control.Monad.State	( get, put )
{- import Data.Acid			( AcidState, Query, Update , makeAcidic, openLocalState )
 - import Data.Acid.Advanced	( query', update' )
 - import Data.Acid.Local 		( createCheckpointAndClose )
 - import Data.Acid.Memory
 - import Data.Acid.Memory.Pure
 - import Data.SafeCopy		( base, deriveSafeCopy )
 -}
import Data.Data		( Data, Typeable )
import Data.Maybe
import Data.Char
import Data.List hiding (insert)
import Data.IxSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)

-- Index accessor functions

newtype Nameword = Nameword String deriving (Eq, Ord, Typeable)

instance Stringe Nameword where
   ravel x = Nameword x
   unravel (Nameword x) = x

instance Show Nameword where
    show = shew

newtype Boosted = Boosted Int deriving (Read, Show, Eq, Ord, Typeable)

instance Hint Boosted where
   val (Boosted x) = x
   unval x = (Boosted x)

fromBoosted :: Boosted -> Power
fromBoosted (Boosted x) = (Power x)

toBoosted :: Power -> Boosted
toBoosted (Power x) = (Boosted x)

getCardType :: GenCard -> [CardType]
getCardType c@GenCard{..} = [ctype]

getCardName :: GenCard -> [Name]
getCardName c@GenCard{..} = [name]

getNameWords :: GenCard -> [Nameword]
getNameWords c@GenCard{..} = map (ravel.(filter isAlphaNum)) (words name)

getSet :: GenCard -> [CSet]
getSet c@GenCard{..} = [set]

getNum :: GenCard -> [Number]
getNum c@GenCard{..} = [num]

getRar :: GenCard -> [Rarity]
getRar c@GenCard{..} = [rar]

getKeyword :: GenCard -> [Keyword]
getKeyword c@GenCard{..} = keywords

getColor :: GenCard -> [Color]
getColor c@GenCard{ctype = TProblem, ..} = concat (map classify (fromMaybe [Wild] (mpreqs >>= return.(map fst).fst)))
getColor c@GenCard{..} = classify $ fromMaybe Wild mcolor

getCost :: GenCard -> [Cost]
getCost c@GenCard{..} = fromMaybe ([]) (mcost >>= return.(:[]))

getReq :: GenCard -> [Req]
getReq c@GenCard{..} = fromMaybe ([]) (mreq >>= return.(:[]))

getPower :: GenCard -> [Power]
getPower c@GenCard{..} = fromMaybe ([]) (mpower >>= return.(:[]))

getBoosted :: GenCard -> [Boosted]
getBoosted c@GenCard{..} = map (toBoosted) (fromMaybe ([]) (mboosted >>= return.(:[])))

getPoints :: GenCard -> [Points]
getPoints c@GenCard{..} = fromMaybe ([]) (mpoints >>= return.(:[]))

instance Indexable GenCard where
    empty = ixSet 
                [ ixFun getCardType
                , ixFun getCardName
                , ixFun getNameWords
                , ixFun getSet
                , ixFun getNum
                , ixFun getRar
                , ixFun getKeyword
                , ixFun getColor
                , ixFun getCost
                , ixFun getReq
                , ixFun getPoints
                , ixFun getPower
                , ixFun getBoosted
                ]

allcards :: Set Card
allcards = parsage mlpccg

cardDB :: IxSet GenCard
cardDB = Set.foldr (\x y -> insert (toGeneric x) y) empty (allcards)
