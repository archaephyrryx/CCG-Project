{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    TemplateHaskell, TypeFamilies, RecordWildCards,
	OverloadedStrings #-}

module Database where

import Cards
import Cards.Common
import Cards.Common.Hint
import Cards.Common.Stringe
import Cards.Common.Abbrev
import Cards.Generic
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
import Data.List
import Data.IxSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)

{-
 -data GenCard = GenCard { ctype    :: CardType
 -                       , name     :: Name
 -                       , set      :: CSet
 -                       , num      :: Number
 -                       , rar      :: Rarity
 -                       , keywords :: Keywords
 -                       , mcolor    :: Maybe Color
 -                       , mcost     :: Maybe Cost
 -                       , mreq      :: Maybe Req
 -                       , mpower    :: Maybe Power
 -                       , mboosted  :: Maybe Power
 -                       , mpoints   :: Maybe Points
 -                       , mpreqs    :: Maybe ProblemReq
 -                       , text     :: Text
 -                       } deriving (Show)
 -}
