{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards,
    DeriveDataTypeable #-}

module CCG.Cards where

import CCG.Cards.Common
import Data.List
import Data.Char
import Data.Set (Set)
import Data.Ord
import Data.Function (on)
import Data.Monoid
import Data.Data (Data, Typeable)
import qualified Data.Set as Set

type ShowCard = (Card -> String)
type Cardlist = Set Card

data Card = Mane { name		:: Name
				 , set		:: CSet
				 , num		:: Number
				 , rar		:: Rarity
				 , keywords :: Keywords
				 , color	:: Color
				 , power	:: Power
				 , boosted 	:: Power
				 , text 	:: Text
				 }
		| Friend { name 	:: Name
			     , set		:: CSet
				 , num 		:: Number
				 , rar		:: Rarity
				 , keywords :: Keywords
				 , color 	:: Color
				 , cost		:: Cost
				 , req		:: Req
				 , power 	:: Power
				 , text		:: Text
				 }
	  | Resource { name 	:: Name
	             , set		:: CSet
				 , num		:: Number
				 , rar		:: Rarity
				 , keywords :: Keywords
				 , color 	:: Color
				 , cost 	:: Cost
				 , req		:: Req
				 , power	:: Power
				 , text		:: Text
				 }
		 | Event { name 	:: Name
		 		 , set 		:: CSet
				 , num 		:: Number
				 , rar		:: Rarity
				 , keywords :: Keywords
				 , color	:: Color
				 , cost		:: Cost
				 , req		:: Req
				 , power	:: Power
				 , text		:: Text
				 }
  | Troublemaker { name		:: Name
  				 , set		:: CSet
				 , num		:: Number
				 , rar		:: Rarity
				 , keywords :: Keywords
				 , power 	:: Power
				 , points 	:: Points
				 , text 	:: Text
				 }
	   | Problem { name		:: Name
	             , set		:: CSet
				 , num		:: Number
				 , rar		:: Rarity
				 , points	:: Points
				 , keywords :: Keywords
				 , preqs	:: ProblemReq
				 , text		:: Text
				 }
	  deriving (Show, Data, Typeable)

cardtype :: Card -> CardType
cardtype (Mane{..})		= TMane
cardtype (Friend{..})		= TFriend
cardtype (Resource{..})	= TResource
cardtype (Event{..})		= TEvent
cardtype (Troublemaker{..})= TTroublemaker
cardtype (Problem{..})		= TProblem

instance Eq Card where
    (==) = (==)`on`name

instance Ord Card where
    compare = (comparing set)`mappend`(comparing num)
