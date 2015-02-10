{-# LANGUAGE FlexibleInstances, DeriveDataTypeable,
    GeneralizedNewtypeDeriving #-}

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
                    , readC
                    , readT
                    ) where

import Cards.Common.Color (Color(..), readC)
import Data.Data ( Data, Typeable)
import Data.Char
import Data.SafeCopy (SafeCopy)
import Data.List
import Data.Function (on)
import Data.Set (Set)

-- Type aliases: Keywords, Name, ProblemReq
type Keywords = [Keyword]
type Name = String
type ProblemReq = ([(Color,Power)],Power)

-- Integral newtypes: Power, Cost, Req, Points

newtype Power = Power Int deriving (Read, Show, Ord, Eq, Data, Typeable, SafeCopy)
newtype Cost = Cost Int deriving (Read, Show, Ord, Eq, Data, Typeable, SafeCopy)
newtype Req = Req Int deriving (Read, Show, Ord, Eq, Data, Typeable, SafeCopy)
newtype Points = Points Int deriving (Read, Show, Ord, Eq, Data, Typeable, SafeCopy)

-- String newtypes: Keyword, card text

newtype Keyword = Keyword String deriving (Ord, Eq, Data, Typeable, SafeCopy)
newtype Text = Text String deriving (Ord, Eq, Data, Typeable, SafeCopy)

-- Datatypes: Color, card number, card set, rarity, card type


data Number = Regular Int | F Int | PF Int deriving (Eq, Ord, Data, Typeable)

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

readsN :: ReadS Number
readsN s = (`zip`[""]).(:[]) $ readN s

instance Read Number where
    readsPrec = const readsN
    

-- Abbreviated strings

data CSet = Premiere | CanterlotNights | RockNRave | CelestialSolstice | CrystalGames deriving (Enum, Eq, Ord, Data, Typeable)

data Rarity = Common | Uncommon | Fixed | Rare | UltraRare | Promotional deriving (Enum, Eq, Ord, Show, Read, Data, Typeable)

data CardType = TMane | TFriend | TResource | TEvent | TTroublemaker | TProblem deriving (Eq, Ord, Data, Typeable)

instance Show CardType where
    show TMane = "Mane"
    show TFriend = "Friend"
    show TResource = "Resource"
    show TEvent = "Event"
    show TTroublemaker = "Troublemaker"
    show TProblem = "Problem"

readsT :: ReadS CardType
readsT s = (`zip`[""]).(:[]) $ readT s

readT :: String -> CardType
readT s = case s of
    "Mane"          -> TMane
    "Friend"        -> TFriend
    "Resource"      -> TResource
    "Event"         -> TEvent
    "Troublemaker"  -> TTroublemaker
    "Problem"       -> TProblem

titleCase :: String -> String
titleCase (x:xs) = (toUpper x):(map toLower xs)

instance Read CardType where
    readsPrec = const readsT
