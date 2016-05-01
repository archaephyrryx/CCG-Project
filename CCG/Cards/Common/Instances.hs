{-# Language FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module CCG.Cards.Common.Instances where

import CCG.Cards.Common.Types
import Data.Dimorph
import Data.Maybe
import Data.Char
import Util

-- * Typeclasses

-- | The "Hint" class represents wrapped "Int" values
class Hint a where
    val :: a -> Int -- ^ Extract an Int
    unval :: Int -> a -- ^ Encapsulate an Int
    readH :: String -> a -- ^ Read an Int, then encapsulate
    readH = unval.read
    showH :: a -> String -- ^ Extract, then Show an Int
    showH = show.val

-- | The "Abbrev" class represents Data-types that can be printed in a
-- verbose way or an abbreviated way
class Abbrev a where
    short :: String -> a -- ^ Read an abbreviated string
    long :: String -> a -- ^ Read an verbose string
    brief :: a -> String -- ^ Show as an abbreviated string
    verbose :: a -> String -- ^ Show as a verbose string

-- | The "Stringe" class represents wrapped "String" values
class Stringe a where
    ravel :: String -> a -- ^ Wrap a string
    unravel :: a -> String -- ^ Unwrap a string
    shew :: a -> String -- ^ Unwrap, then show (with inner quotes)
    shew = show.unravel

-- * Implicit Instances

-- | Trivial "Hint" instance for "Int"
instance Hint Int where
    val = id
    unval = id
    readH = read
    showH = show

-- | Trivial "Stringe" instance for "String"
instance Stringe String where
    ravel = id
    unravel = id
    shew = show

-- * Derived Functions

-- | readMaybeH converts an empty string to "Nothing", or an integral
-- string to a "Just" value of a "Hint"-encapsulated "Int"
readMaybeH :: (Hint a) => String -> Maybe a
readMaybeH "" = Nothing
readMaybeH x = Just (readH x)

-- | Adds two "Hint" values
plus :: (Hint a) => a -> a -> a
plus = (unval.).(.val).(+).val

-- | Subtract one "Hint" value from another
minus :: (Hint a) => a -> a -> a
minus = (unval.).(.val).(-).val

-- | Increment a "Hint" value by an Int value
inc :: (Hint a) => Int -> a -> a
inc = (unval.).(.val).(+)

-- | Decrement a "Hint" value by an Int value
dec :: (Hint a) => Int -> a -> a
dec = (unval.).(.val).(+).negate

-- * Implemented Instances

-- ** Hint Instances

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

-- ** Stringe Instances

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

-- ** Abbrev Instances

instance Abbrev Rarity where
    short = readR
    long = read
    brief = (filter isUpper).show
    verbose = show

-- *** Functions for Abbrev Instance Declarations

transCS :: Dimorph CSet String
transCS = let x = [ Premiere
                  , CanterlotNights
                  , RockNRave
                  , CelestialSolstice
                  , CrystalGames
                  ]
              y = [ "Premiere"
                  , "Canterlot Nights"
                  , "Rock and Rave"
                  , "Celestial Solstice"
                  , "Crystal Games"
                  ]
          in Dimorph x y

instance Read CSet where
    readsPrec = const $ rdr . from $ transCS

instance Show CSet where
    show = to transCS

instance Abbrev CSet where
    short = readCS
    long = readCS.shorten
    brief = shorten.show
    verbose = show

-- | 'readCS' is an Abbrev-based reader/parser for readCS
readCS :: String -> CSet
readCS x = case x of
        "pr" -> Premiere
        "cn" -> CanterlotNights
        "rr" -> RockNRave
        "cs" -> CelestialSolstice
        "cg" -> CrystalGames
        _ -> error ("No cardset parse for '"++x++"'")

-- | The 'shorten' function converts an unabbreviated CSet string to its
-- abbreviated version; this conversion filters out all words of a
-- string that begin with an uppercase letter, and takes the first two
-- characters if there is only one such word, or the initials otherwise.
shorten :: String -> String
shorten = map toLower . cond (length.=1) (take 2.head) (map head) . filter (isUpper.head) . words

readR :: String -> Rarity
readR x = case x of
    "C"  -> Common
    "U"  -> Uncommon
    "F"  -> Fixed
    "R"  -> Rare
    "UR" -> UltraRare
    "P"  -> Promotional
    _    -> error ("No Rarity parse for '"++x++"'")
