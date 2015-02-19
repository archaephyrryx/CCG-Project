{-# Language FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Cards.Common.Instances where

import Cards.Common.Types
import Data.Maybe
import Data.Char

-- Class declarations
class Hint a where
    val :: a -> Int
    unval :: Int -> a
    readH :: String -> a
    readH = unval.read 
    showH :: a -> String
    showH = show.val

class Abbrev a where
    short :: String -> a
    long :: String -> a
    brief :: a -> String
    verbose :: a -> String

class Stringe a where
    ravel :: String -> a
    unravel :: a -> String
    shew :: a -> String
    shew = show.unravel

-- Implicit Instances

instance Hint Int where
    val = id
    unval = id
    readH = read
    showH = show

instance Stringe String where
    ravel = id
    unravel = id
    shew = show

-- Derived functions

readMaybeH :: (Hint a) => String -> Maybe a
readMaybeH "" = Nothing
readMaybeH x = Just (readH x)

plus :: (Hint a) => a -> a -> a
plus = (unval.).(.val).(+).val

minus :: (Hint a) => a -> a -> a
minus = (unval.).(.val).(-).val

inc :: (Hint a) => Int -> a -> a
inc = (unval.).(.val).(+)

dec :: (Hint a) => Int -> a -> a
dec = (unval.).(.val).(+).negate


-- Implemented Instances

-- hints

instance Hint Power where
    val (Power x) = x
    unval x = (Power x)

instance Hint Cost where
    val (Cost x) = x
    unval x = (Cost x)

instance Hint Req where
    val (Req x) = x
    unval x = (Req x)

instance Hint Points where
    val (Points x) = x
    unval x = (Points x)

-- stringes

instance Stringe Keyword where
    ravel x = Keyword x
    unravel (Keyword x) = x

instance Stringe Text where
    ravel x = Text x
    unravel (Text x) = x

instance Show Keyword where
    show = shew
instance Show Text where
    show = shew

-- abbrevs

instance Abbrev CSet where
    short = readCS
    long = readCS.shorten
    brief = shorten.show
    verbose = show

instance Abbrev Rarity where
    short = readR
    long = read
    brief = (filter isUpper).show
    verbose = show

-- Helpers for instances of Abbrev

instance Read CSet where
    readsPrec = const (readsSet.(map toLower))

readsSet :: ReadS CSet
readsSet s = (`zip`[""]).(:[]) $ case s of
      ('p':_)     -> Premiere
      ('r':_)     -> RockNRave
      ('c':'r':_) -> CrystalGames
      ('c':'g':_) -> CrystalGames
      ('c':'a':_) -> CanterlotNights
      ('c':'n':_) -> CanterlotNights
      ('c':'s':_) -> CelestialSolstice
      ('c':'e':_) -> CelestialSolstice

instance Show CSet where
    show Premiere = "Premiere"
    show CanterlotNights = "Canterlot Nights"
    show RockNRave = "Rock and Rave"
    show CelestialSolstice = "Celestial Solstice"
    show CrystalGames = "Crystal Games"

readCS :: String -> CSet
readCS x = case x of
        "pr" -> Premiere
        "cn" -> CanterlotNights
        "rr" -> RockNRave
        "cs" -> CelestialSolstice
        "cg" -> CrystalGames
        _ -> error ("No cardset parse for '"++x++"'")

shorten :: String -> String
shorten x = let y = (filter (\(a:b) -> isUpper a) (words x))
    in if (length y) == 1
        then (map toLower (take 2 (head y)))
        else (map toLower (map (head) y))

readR :: String -> Rarity
readR x = case x of 
    "C"  -> Common
    "U"  -> Uncommon
    "F"  -> Fixed
    "R"  -> Rare
    "UR" -> UltraRare
    "P"  -> Promotional
    _    -> error ("No Rarity parse for '"++x++"'")
