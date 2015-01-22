{-# LANGUAGE FlexibleInstances #-}

module Cards.Common.Abbrev where

import Cards.Common
import Data.Char

class Abbrev a where
    short :: String -> a
    long :: String -> a
    brief :: a -> String
    verbose :: a -> String

instance Abbrev CSet where
    short = readCS
    long = readCS.shorten
    brief = shorten.show
    verbose = show

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

instance Abbrev Rarity where
    short = readR
    long = read
    brief = (filter isUpper).show
    verbose = show
