{-# LANGUAGE FlexibleInstances #-}

module Cards.Common ( Keywords
                    , Name
                    , ProblemReq
                    , Power(..)
                    , Cost(..)
                    , Req(..)
                    , Points(..)
                    , Keyword(..)
                    , Text(..)
                    , Color(..)
                    , Number(..)
                    , readN
                    , CSet(..)
                    , Rarity(..)
                    , CardType(TMane
                              ,TFriend
                              ,TResource
                              ,TEvent
                              ,TTroublemaker
                              ,TProblem
                              )
                    , readC) where

import Data.Char
import Data.List
import Data.Function (on)
import Data.Set (Set)

-- Type aliases: Keywords, Name, ProblemReq
type Keywords = [Keyword]
type Name = String
type ProblemReq = ([(Color,Power)],Power)

-- Integral newtypes: Power, Cost, Req, Points

newtype Power = Power Int deriving (Read, Show, Ord, Eq)
newtype Cost = Cost Int deriving (Read, Show, Ord, Eq)
newtype Req = Req Int deriving (Read, Show, Ord, Eq)
newtype Points = Points Int deriving (Read, Show, Ord, Eq)

-- String newtypes: Keyword, card text

newtype Keyword = Keyword String
newtype Text = Text String

-- Datatypes: Color, card number, card set, rarity, card type

data Color =    Blue |    Yellow |    Purple |    Pink |    White |    Orange
	       | NonBlue | NonYellow | NonPurple | NonPink | NonWhite | NonOrange
           | Wild deriving (Show, Read)

data Number = Regular Int | F Int | PF Int deriving (Eq, Ord)

instance Show Number where
    show (Regular x) = show x
    show (F x) = 'f':(show x)
    show (PF x) = 'p':'f':(show x)

readN :: String -> Number
readN ('p':'f':x) = PF (read x)
readN ('P':'f':x) = PF (read x)
readN ('f':x) = F (read x)
readN x@(k:_) | isDigit k = Regular (read x)
              | otherwise = error ("Could not parse Number: "++x)


-- Abbreviated strings

data CSet = Premiere | CanterlotNights | RockNRave | CelestialSolstice | CrystalGames deriving (Enum, Eq, Ord)

data Rarity = Common | Uncommon | Fixed | Rare | UltraRare | Promotional deriving (Enum, Eq, Ord, Show, Read)

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
