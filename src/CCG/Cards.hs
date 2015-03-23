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

typename :: ShowCard
typename x = (show.cardtype$x)++(':':' ':(name x))

setnum :: ShowCard
setnum x = (brief.set$x)++(show.num$x)

instance Eq Card where
    (==) = (==)`on`name

instance Ord Card where
    compare = (comparing set)`mappend`(comparing num)

{-
cSplit :: String -> [String]
cSplit "" = []
cSplit "\t" = ["",""]
cSplit x | '\t'`elem`x = let (a,(_:b)) = break (=='\t') x in ((filter (/='\'') a):(cSplit b))
	 | otherwise = [filter (/='\'') x]

cardParse :: [String] -> Card
cardParse x = case typ of
		"Mane" -> let
					boo' = readH boo
					pow' = readH pow
					col' = readC col
				  in (Mane nam set num rar key' col' pow' boo' text)
		"Friend" -> let
					pow' = readH pow
					col' = readC col
					cos' = readH cos
					req' = readH req
					in (Friend nam set num rar key' col' cos' req' pow' text)
		"Resource" -> let
						pow' = readH pow
						col' = readC col
						cos' = readH cos
						req' = readH req
					  in (Resource nam set num rar key' col' cos' req' pow' text)
		"Event"	-> let
                        pow' = readH pow
                        col' = readC col
                        cos' = readH cos
                        req' = readH req
                        in (Event nam set num rar key' col' cos' req' pow' text)
		"Troublemaker" -> let
                            poi' = readH poi
                            pow' = readH pow
                          in (Troublemaker nam set num rar key' pow' poi' text)
		"Problem" -> let
                        poi' = readH poi
                        preqs' = if preq2 == ""
                                    then ([(readC col1, readH preq1)], readH opreq)
                                    else ([(readC col1, readH preq1), (readC col2, readH preq2)], readH opreq)
                     in (Problem nam set num rar poi' key' preqs' text)
    where
        (nam:set':num':rar':typ:key:col:cos:req:pow:boo:poi:preq1:col1:preq2:col2:opreq:text':[]) = x
        key' = keySplit key
        text = ravel text'
        set = long set'
        num = readN num'
        rar = readR rar'

keySplit :: String -> Keywords
keySplit "" = []
keySplit ('[':x) = let (a,b) = break (==']') x in (ravel ('[':a++"]")):(if null b then [] else keySplit (tail b))
keySplit (' ':x) = keySplit x
keySplit (',':x) = keySplit x
keySplit x | ','`elem`x = let (a,(_:b)) = break (==',') x in (ravel a):(keySplit b)
	   | otherwise = [(ravel x)]

parsage = Set.fromList.(map(cardParse.cSplit)).tail.lines
-}
