{-# LANGUAGE DeriveDataTypeable #-}

module Cards.Common.Color where

import Cards.Common.Invotomorph
import Data.List ((\\))
import Data.Data (Data, Typeable)

data Color =    Blue |    Yellow |    Purple |    Pink |    White |    Orange
	       | NonBlue | NonYellow | NonPurple | NonPink | NonWhite | NonOrange
           | Wild deriving (Show, Read, Eq, Ord, Typeable)

instance Invotomorph Color where
    classX = spect
    classY = aspect

readC :: String -> Color
readC "" = Wild
readC x = read x :: Color

spect :: [Color]
spect = [Blue, Yellow, Purple, Pink, White, Orange]

aspect :: [Color]
aspect = [NonBlue, NonYellow, NonPurple, NonPink, NonWhite, NonOrange]

invert :: Color -> Color
invert = invoto

classify :: Color -> [Color]
classify x | spectral x = x:(aspect\\[invert x])
           | aspectral x = x:(spect\\[invert x])
           | otherwise = Wild:(spect++aspect)

spectral :: Color -> Bool
spectral = (`elem`spect)

aspectral :: Color -> Bool
aspectral = (`elem`aspect)

isWild :: Color -> Bool
isWild = (==Wild)
