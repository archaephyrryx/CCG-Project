{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    TemplateHaskell, TypeFamilies, RecordWildCards  #-}

module DataState where

import Control.Applicative	( (<$>) )
import Control.Exception	( bracket )
import Control.Monad		( msum )
import Control.Monad.Reader	( ask )
import Control.Monad.State	( get, put )
import Data.Data		( Data, Typeable )
import Happstack.Server		( Response, ServerPart, dir
				, nullDir, nullConf, ok
				, simpleHTTP, toResponse )
import Data.Acid		( AcidState, Query, Update
				, makeAcidic, openLocalState )
import Data.Acid.Advanced	( query', update' )
import Data.Acid.Local 		( createCheckpointAndClose )
import Data.SafeCopy		( base, deriveSafeCopy )
import Database			( Database, Attribute , Attributes, emptyDB)
import qualified Database as DB
				  
import MLPCCG (mlpccg)


data DataState = DataState { database :: Database, tags :: Attributes }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''DataState)

initialDataState :: DataState
initialDataState = DataState (emptyDB)

-- Update Functions

addAttribute :: Attribute -> Update DataState ()
addAttribute a =
    do d@DataState{..} <- get
	let newTags = (Set.insert a tags)
	put $ d { tags = newTags }

addAttributes :: Attributes -> Update DataState ()
addAttributes as =
    do d@DataState{..} <- get
	let newTags = (Set.union as tags)
	put $ d { tags = newTags }

addTag :: Card -> Attribute -> Update DataState ()
addTag c a =
    do d@DataState{..} <- get
	let newTags = (Set.insert a tags)
	    newDB   = DB.addAttrib c a database
	put $ d { database = newDB, tags = newTags }


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
