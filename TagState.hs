{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    TemplateHaskell, TypeFamilies, RecordWildCards  #-}

module TagState where

import Cards
import Cards.Common
import Cards.Common.Abbrev
import Cards.Common.Color
import Cards.Common.Hint
import Cards.Common.Stringe
import Cards.Generic
import Cards.Pretty
import Control.Applicative	( (<$>) )
import Control.Exception	( bracket )
import Control.Monad		( msum )
import Control.Monad.Reader	( ask )
import Control.Monad.State	( get, put )
import Data.Acid			( AcidState, Query, Update , makeAcidic, openLocalState )
import Data.Acid.Advanced	( query', update' )
import Data.Acid.Local 		( createCheckpointAndClose )
import Data.Acid.Memory
import Data.Acid.Memory.Pure
import Database
import Data.Char
import Data.Data		( Data, Typeable )
import Data.IxSet
import Data.List hiding (insert)
import qualified Data.List (insert) as linsert
import Data.Maybe
import Data.SafeCopy		( base, deriveSafeCopy )
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

data Tag = Tag !String
    deriving (Eq, Ord, Data, Typeable)

instance Stringe Tag where
    ravel x = Tag x
    unravel (Tag !x) = x

instance Show Tag where
    show = shew

data Assoc = Assoc { card :: GenCard, tags :: [Tag] }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Indexable Assoc where
    empty = ixSet 
                [ ixFun (fromGeneric.card) -- non-generic card-based lookup
                , ixFun (card) -- generic card-based lookup
                , ixFun (getCardType.card)
                , ixFun (getCardName.card)
                , ixFun (getNameWords.card)
                , ixFun (getSet.card)
                , ixFun (getNum.card)
                , ixFun (getRar.card)
                , ixFun (getKeyword.card)
                , ixFun (getColor.card)
                , ixFun (getCost.card)
                , ixFun (getReq.card)
                , ixFun (getPoints.card)
                , ixFun (getPower.card)
                , ixFun (getBoosted.card)
                , ixFun ((map unravel).tags) -- String-based tag lookup
                , ixFun (tags) -- Tag-based tag lookup
                ]

emptyDB = fromList (zipWith Assoc (toList cardDB) (repeat [])) -- cardDB with initially blank taglists


type AssocList = IxSet Assoc

type TagTable = Set Tag

data DataState = DataState { assocs :: AssocList, tags :: TagTable }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''DataState)

initialDataState :: DataState
initialDataState = DataState (emptyDB) (Set.empty)

-- Assoc-level update functions

addTag :: Tag -> Assoc -> Assoc
addTag t (Assoc c ts) = Assoc c (linsert t ts)

addTags :: [Tag] -> Assoc -> Assoc
addTags ts (Assoc c tss) = Assoc c (foldr (linsert) tss ts)


-- AssocList-level update functions

addCardTag :: Card -> Tag -> AssocList -> AssocList
addCardTag c t al = updateIx old (addTag t old) al
  where
    (fromJust.getOne $ al @= c) 

addCardTags :: Card -> [Tag] -> AssocList -> AssocList
addCardTags c ts al = updateIx old (addTags ts old) al
  where
    (fromJust.getOne $ al @= c) 

-- Update Functions

updateTag :: Card -> Tag -> Update DataState ()
updateTag c t =
    do d@DataState{..} <- get
	let newTags = (Set.insert t tags)
	    newAssocs = addCardTag c t assocs
	put $ d { assocs = newAssocs, tags = newTags }


updateTags :: Card -> [Tag] -> Update 
updateTags c ts =
    do d@DataState{..} <- get
	let newTags = (foldr (Set.insert) tags ts)
	    newAssocs = addCardTags c t assocs
	put $ d { assocs = newAssocs, tags = newTags }


addCards :: Cardlist -> Update DataState ()
addCards cs =
    do d@DataState{..} <- get
	let newDB = DB.addCards cs database
	put $ d { database = newDB }

initialize :: Cardlist -> DataState
initialize = flip addCards initialDataState

addCard :: Card -> Update DataState ()
addCard c =
    do d@DataState{..} <- get
	let newDB = DB.addCard c database
	put $ d { database = newDB }

addEntry :: Card -> Attributes -> Update DataState ()
addEntry c as =
    do d@DataState{..} <- get
	let newTags = (Set.union as tags)
	    newDB = DB.addEntry c database

-- Query Functions

queryCard :: Card -> Query DataState Bool
queryCard c = (member c.assocs.database) <$> ask

queryCards :: Query DataState Cardlist
queryCards = (keysSet.assocs.database) <$> ask

queryAssocs :: Card -> Query DataState Attributes
queryAssocs c = (lookup c.assocs.database) <$> ask

queryAttrs :: Query DataState Attributes
queryAttrs = tags <$> ask

queryEntries :: (Card -> Attributes -> Bool) -> Query DataState Database
queryEntries p = ((filterWithKey p).assocs.database) <$> ask




