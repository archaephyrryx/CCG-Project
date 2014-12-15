{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cards where

import Data.List
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set

type Keywords = [Keyword]
type Keyword = String
type Text = String
type ShowCard = (Card -> String)
type Cardlist = Set Card

newtype Power = Power { getPower :: Int } deriving (Eq, Ord, Num, Real, Enum, Integral, Read, Show)
newtype Cost = Cost { getCost :: Int } deriving (Eq, Ord, Num, Real, Enum, Integral, Read, Show)
newtype Req = Req { getReq :: Int } deriving (Eq, Ord, Num, Real, Enum, Integral, Read, Show)
newtype Points = Points { getPoints :: Int } deriving (Eq, Ord, Num, Real, Enum, Integral, Read, Show)

type Name = String

type ProblemReq = ([(Color,Power)],Power)

data Color =    Blue |    Yellow |    Purple |    Pink |    White |    Orange
	   | NonBlue | NonYellow | NonPurple | NonPink | NonWhite | NonOrange
	   | Wild deriving (Show, Read)


readC :: String -> Color
readC "" = Wild
readC x = read x :: Color

data CardType = TMane | TFriend | TResource | TEvent | TTroublemaker | TProblem deriving (Eq)

instance Show CardType where
    show TMane = "Mane"
    show TFriend = "Friend"
    show TResource = "Resource"
    show TEvent = "Event"
    show TTroublemaker = "Troublemaker"
    show TProblem = "Problem"

data Card = Mane { name :: Name, set :: String, num :: String,  keywords :: Keywords, color :: Color, power :: Power, boosted :: Power, text :: Text }
	  | Friend { name :: Name, set :: String, num :: String,  keywords :: Keywords, color :: Color, cost :: Cost, req :: Req, power :: Power, text :: Text }
	  | Resource { name :: Name, set :: String, num :: String,  keywords :: Keywords, color :: Color, cost :: Cost, req :: Req, power :: Power, text :: Text }
	  | Event { name :: Name, set :: String, num :: String,  keywords :: Keywords, color :: Color, cost :: Cost, req :: Req, power :: Power, text :: Text }
	  | Troublemaker { name :: Name, set :: String, num :: String,  keywords :: Keywords, power :: Power, points :: Points, text :: Text }
	  | Problem { name :: Name, set :: String, num :: String, points :: Points, keywords :: Keywords, preqs :: ProblemReq, text :: Text }
	  deriving (Show)

ctype :: Card -> CardType
ctype (Mane _ _ _ _ _ _ _ _) = TMane
ctype (Friend _ _ _ _ _ _ _ _ _) = TFriend
ctype (Resource _ _ _ _ _ _ _ _ _) = TResource
ctype (Event _ _ _ _ _ _ _ _ _) = TEvent
ctype (Troublemaker _ _ _ _ _ _ _) = TTroublemaker
ctype (Problem _ _ _ _ _ _ _) = TProblem

typename :: ShowCard
typename x = (show.ctype$x)++(':':' ':(name x))

setnum :: ShowCard
setnum x = (set x)++(num x)

instance Eq Card where
    (==) x y = (==) (name x) (name y)

instance Ord Card where
    compare x y = compare (name x) (name y)


cSplit :: String -> [String]
cSplit "" = []
cSplit "\t" = ["",""]
cSplit x | '\t'`elem`x = let (a,(_:b)) = break (=='\t') x in ((filter (/='\'') a):(cSplit b))
	 | otherwise = [filter (/='\'') x]

cardParse :: [String] -> Card
cardParse (nam:set':num:rar:typ:key:col:cos:req:pow:boo:poi:preq1:col1:preq2:col2:opreq:text:[]) = case typ of
		"Mane" -> let boo' = Power (read boo :: Int)
		              pow' = Power (read pow :: Int)
			      col' = readC col
			      cos' = Cost  (read cos :: Int)
			      req' = Req (read req :: Int)
		          in Mane {name=nam, set = set, num = num, keywords=key', color=col', power=pow', boosted=boo', text=text }
		"Friend"   -> let pow' = Power (read pow :: Int)
				  col' = readC col
				  cos' = Cost ( read cos :: Int )
				  req' = Req ( read req :: Int )
			      in Friend {name=nam, set = set, num = num, keywords=key', color=col', cost=cos', req=req', power=pow', text=text }
		"Resource" -> let pow' = Power ( read pow :: Int )
				  col' = readC col
				  cos' = Cost ( read cos :: Int )
				  req' = Req ( read req :: Int )
			      in Resource {name=nam, set = set, num = num, keywords=key', color=col', cost=cos', req=req', power=pow', text=text }
		"Event"    -> let pow' = Power ( read pow :: Int )
				  col' = readC col
				  cos' = Cost ( read cos :: Int )
				  req' = Req ( read req :: Int )
			      in Event {name=nam, set = set, num = num, keywords=key', color=col', cost=cos', req=req', power=pow', text=text }
		"Troublemaker" -> let poi' = Points ( read poi :: Int )
				      pow' = Power (read pow :: Int)
				  in  Troublemaker {name=nam, set = set, num = num, keywords=key', power=pow', points=poi', text=text}
		"Problem" -> let poi' = Points ( read poi :: Int )
			         preqs' = if preq2 == "" then ([(readC col1, Power ( read preq1 :: Int ))], Power ( read opreq :: Int ))
                                                         else ([(readC col1, Power ( read preq1 :: Int )), (readC col2, Power ( read preq2 :: Int))], Power ( read opreq :: Int ))
			     in Problem {name=nam, set = set, num = num, points=poi', keywords=key', preqs=preqs', text=text }
    where
	key' = keySplit key
	set = shorten set'

shorten :: String -> String
shorten x = let y = (filter (\(a:b) -> isUpper a) (words x))
	    in if (length y) == 1 then (map toLower (take 2 (head y)))
				  else (map toLower (map (head) y))


keySplit :: String -> Keywords
keySplit "" = []
keySplit ('[':x) = let (a,b) = break (==']') x in ('[':a++"]"):(if null b then [] else keySplit (tail b))
keySplit (' ':x) = keySplit x
keySplit (',':x) = keySplit x
keySplit x | ','`elem`x = let (a,(_:b)) = break (==',') x in a:(keySplit b)
	   | otherwise = [x]

parsage = Set.fromList.(map(cardParse.cSplit)).tail.lines
