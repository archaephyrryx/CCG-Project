{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TypeFamilies, RecordWildCards #-}

module API.Database where

import CCG
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
import Util

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

newtype SetNum = SetNum String deriving (Eq, Ord, Typeable)

instance Stringe SetNum where
    ravel x = SetNum x
    unravel (SetNum x) = x

fromBoosted :: Boosted -> Power
fromBoosted (Boosted x) = (Power x)

toBoosted :: Power -> Boosted
toBoosted (Power x) = (Boosted x)

getCardType :: GenCard -> [CardType]
getCardType c@GenCard{..} = [ctype]

getCardName :: GenCard -> [Name]
getCardName c@GenCard{..} = [gname]

getNameWords :: GenCard -> [Nameword]
getNameWords c@GenCard{..} = concatMap ((map ravel).subsequences.(filter isAlphaNum)) (words gname)

getSet :: GenCard -> [CSet]
getSet c@GenCard{..} = [gset]

getNum :: GenCard -> [Number]
getNum c@GenCard{..} = [gnum]

getSetNum :: GenCard -> [SetNum]
getSetNum c = [ravel . setnum $ c]

getRar :: GenCard -> [Rarity]
getRar c@GenCard{..} = [grar]

getKeyword :: GenCard -> [Keyword]
getKeyword c@GenCard{..} = gkeywords

getColor :: GenCard -> [Color]
getColor c@GenCard{ctype = TProblem, ..} = concat (map classify (fromMaybe [Wild] (mpreqs >>= return.(map fst).fst)))
getColor c@GenCard{..} = classify $ fromMaybe Wild mcolor

getCost :: GenCard -> [Cost]
getCost c@GenCard{..} = fromMaybe ([]) (mcost >>= wrapped return)

getReq :: GenCard -> [Req]
getReq c@GenCard{..} = fromMaybe ([]) (mreq >>= wrapped return)

getPower :: GenCard -> [Power]
getPower c@GenCard{..} = fromMaybe ([]) (mpower >>= wrapped return)

getBoosted :: GenCard -> [Boosted]
getBoosted c@GenCard{..} = map (toBoosted) (fromMaybe ([]) (mboosted >>= wrapped return))

getPoints :: GenCard -> [Points]
getPoints c@GenCard{..} = fromMaybe ([]) (mpoints >>= wrapped return)

instance Indexable GenCard where
    empty = ixSet
                [ ixFun getSetNum
                , ixFun getSet
                , ixFun getNum
                , ixFun getCardName
                , ixFun getNameWords
                , ixFun getCardType
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
