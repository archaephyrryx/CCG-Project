module Database where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List
import Cards

type Attributes = Set Attribute
type Attribute = String

newtype Database = Database { assocs :: Map Card Attributes }

instance Show (Database) where
    show = cardshow show

cardshow :: ShowCard -> Database -> String
cardshow f x = (show.(map (\(x,y) -> (f x, (unwords.(map show).(Set.toAscList)$y)))).Map.toList.assocs$x)

emptyDB :: Database
emptyDB = Database Map.empty

addAttrib :: Card -> Attribute -> Database -> Database
addAttrib c a d = Database (Map.alter (addTag) c (assocs d))
    where
	addTag :: Maybe Attributes -> Maybe Attributes
	addTag Nothing = Just (Set.singleton a)
	addTag (Just x) = Just (Set.insert a x)

addCards :: Cardlist -> Database -> Database
addCards xs db = Database (Map.unionWith (flip const) (Map.fromSet (const Set.empty) xs) (assocs db))

initialize :: Cardlist -> Database
initialize = flip addCards emptyDB

addCard :: Card -> Database -> Database
addCard c d = Database (Map.insertWith (flip const) c (Set.empty) (assocs d))

addEntry :: Card -> Attributes -> Database -> Database
addEntry c a d = Database (Map.insertWith Set.union c a (assocs d))
