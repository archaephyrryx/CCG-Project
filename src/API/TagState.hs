{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    TemplateHaskell, TypeFamilies, RecordWildCards #-}

module TagState where

import CCG
--------------------------------------------
import API.Database
import API.IxMap
--------------------------------------------
import Control.Applicative	( (<$>), (<*>) )
import Control.Exception	( bracket )
import Control.Monad		( msum )
import Control.Monad.Reader	( ask )
import Control.Monad.State	( get, put )
--------------------------------------------
import Data.Acid			( AcidState, Query, Update , makeAcidic, openLocalState )
import Data.Acid.Advanced	( query', update' )
import Data.Acid.Local 		( createCheckpointAndClose )
import Data.Acid.Memory
import Data.Acid.Memory.Pure
--------------------------------------------
import Data.Char
import Data.Data		( Data, Typeable )
import Data.IxSet
import Data.Maybe
import Data.Bifunctor
import Data.SafeCopy		( base, deriveSafeCopy )
import Data.Map (Map)
import Data.Set (Set)
import Data.List hiding (insert)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

data Tag = Tag !String
    deriving (Eq, Ord, Data, Typeable)

instance Stringe Tag where
    ravel x = Tag x
    unravel (Tag x) = x

instance Show Tag where
    show = shew
$(deriveSafeCopy 0 'base ''Tag)

data Assoc = Assoc { card :: GenCard, tags :: [Tag] }
    deriving (Eq, Ord, Show, Data, Typeable)

instance Indexable Assoc where
    empty = ixSet 
                [ ixFun (one.fromGeneric.card) -- non-generic card-based lookup
                , ixFun (one.card) -- generic card-based lookup
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
                , ixFun (one.length.tags) -- Number-of-tags lookup
                ]

type AssocList = IxSet Assoc

emptyDB :: AssocList
emptyDB = fromList (zipWith Assoc (toList cardDB) (repeat [])) -- cardDB with initially blank taglists

showSum :: AssocList -> String
showSum x = let z = toList (x @= (0 :: Int))
                m = toList (x @> (0 :: Int))
            in unlines $ [ "Untagged: "++(show $ length z)
                         , "Tagged: "++(show $ length m)
                         ] ++ map (\x -> (gname.card $ x) ++ ": " ++ (unwords.map unravel.tags $ x)) m
$(deriveSafeCopy 0 'base ''Assoc)

type TagTable = Set Tag

data DataState = DataState { assocs :: AssocList, tagt :: TagTable }
    deriving (Eq, Ord, Data, Typeable)

instance Show DataState where
    show d@DataState{..} = "Tags: "++(intercalate ", " . map unravel . Set.toList $ tagt)++"\n"++showSum assocs

$(deriveSafeCopy 0 'base ''DataState)

initialDataState :: DataState
initialDataState = DataState (emptyDB) (Set.empty)

-- Helper functions for ixset, set and list manipulation

-- List INTO Set
lintos :: Ord a => [a] -> Set a -> Set a
lintos l s = foldr (Set.insert) s l

-- List insert

linsert :: Ord a => a -> [a] -> [a]
linsert = List.insert

-- Assoc-level update functions

addTag :: Tag -> Assoc -> Assoc
addTag t (Assoc c ts) = Assoc c (linsert t ts)

addTags :: [Tag] -> Assoc -> Assoc
addTags ts (Assoc c tss) = Assoc c (foldr (linsert) tss ts)

swapTag :: Tag -> Tag -> Assoc -> Assoc
swapTag old new (Assoc c ts) = Assoc c (linsert new (List.delete old ts))

-- AssocList-level update functions

addCardTag :: GenCard -> Tag -> AssocList -> AssocList
addCardTag c t al = updateIx (card old) (addTag t old) al
  where
    old = (fromJust.getOne $ al @= c)

addCardTags :: GenCard -> [Tag] -> AssocList -> AssocList
addCardTags c ts al = updateIx old (addTags ts old) al
  where
    old = (fromJust.getOne $ al @= c)

replaceCardTag :: Tag -> Tag -> AssocList -> AssocList
replaceCardTag old new al = (al &&& al') ||| al'
  where
    al' = mapmix (swapTag old new) (al @= old)

-- Atomic Update Functions

-- Adds a tag to a card on condition that the tag is already in the
-- tag-table. This condition is not checked
updateETag :: GenCard -> Tag -> Update DataState ()
updateETag c t  =
    do d@DataState{..} <- get
       let newAssocs = addCardTag c t assocs
       put $ d { assocs = newAssocs, tagt = tagt }

-- Adds tags to a card on condition that the tags are already in the
-- tag-table. This condition is not checked
updateETags :: GenCard -> [Tag] -> Update DataState ()
updateETags c ts  =
    do d@DataState{..} <- get
       let newAssocs = addCardTags c ts assocs
       put $ d { assocs = newAssocs, tagt = tagt }

updateTTag :: Tag -> Update DataState ()
updateTTag t =
    do d@DataState{..} <- get
       let newTags = (Set.insert t tagt)
       put $ d { assocs = assocs, tagt = newTags }

updateTTags :: [Tag] -> Update DataState ()
updateTTags ts =
    do d@DataState{..} <- get
       let newTags = lintos ts tagt
       put $ d { assocs = assocs, tagt = newTags }

mergeTTags :: TagTable -> Update DataState ()
mergeTTags ts =
    do d@DataState{..} <- get
       let newTags = Set.union ts tagt
       put $ d { assocs = assocs, tagt = newTags }

-- Compound Update Functions

updateTag :: GenCard -> Tag -> Update DataState ()
updateTag c t = updateTTag t >> updateETag c t

updateTags :: GenCard -> [Tag] -> Update DataState ()
updateTags c ts = updateTTags ts >> updateETags c ts

replaceTag :: Tag -> Maybe Tag -> Update DataState ()
replaceTag old new =
    do d@DataState{..} <- get
       let newTags = (maybe id (Set.insert) new) $ (Set.delete old tagt)
           newAssocs = (maybe id (replaceCardTag old) new) $ assocs
       put $ d { assocs = newAssocs, tagt = newTags }

deleteTag :: Tag -> Update DataState ()
deleteTag old = replaceTag old (Nothing)

massUpdateTags :: ([(GenCard,[Tag])], Set Tag) -> Update DataState ()
massUpdateTags (cts, tst) = mergeTTags tst >> mapM_ (uncurry updateETags) cts

computeTags :: (GenCard -> [Tag]) -> ([(GenCard,[Tag])], Set Tag)
computeTags comp =computeCardTags comp $ toList cardDB
    where
      computeCardTags :: (GenCard -> [Tag]) -> [GenCard] -> ([(GenCard,[Tag])], Set Tag)
      computeCardTags comp gs = foldmap (autoTag comp) Set.empty gs

      autoTag :: (GenCard -> [Tag]) -> (GenCard -> ((GenCard,[Tag]), (Set Tag -> Set Tag)))
      autoTag comp = \g -> let ts = comp g in ((g,ts), (lintos ts))

-- Query Functions

queryAssocs :: Query DataState AssocList
queryAssocs = assocs <$> ask

queryCardTags :: Card -> Query DataState [Tag]
queryCardTags c = (tags . fromJust . getOne . getEQ (toGeneric c) . assocs) <$> ask

queryTagTable :: Query DataState TagTable
queryTagTable = tagt <$> ask

$(makeAcidic ''DataState ['updateETag, 'updateTTag, 'updateTag, 'updateETags, 'updateTTags, 'mergeTTags, 'updateTags, 'replaceTag, 'deleteTag, 'massUpdateTags, 'queryAssocs, 'queryCardTags, 'queryTagTable])
