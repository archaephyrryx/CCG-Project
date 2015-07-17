{-# LANGUAGE FlexibleInstances, DeriveDataTypeable,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    RecordWildCards #-}

module CCG.Cards.Common.Types (
    -- * Aliases

    -- | These aliases are all used to write type signatures that
    -- hide the choice of implementation of particular types and make
    -- modification of those definitions more convenient to change
    -- globally.

    -- | 'Keywords' is an alias for a Keyword-list that abstracts away the
    -- specific list-based implementation and allows for more convenient 
    -- reimplementation using some other collection
    Keywords,

    -- | 'Name' is a simple alias to distinguish between Strings used in
    -- general, and Card-Names currently implemented as Strings but with
    -- potential redefinition 
    Name,

    -- * 'ProblemReq' is a type-alias used instead of the unwieldy type that
    -- would characterise problem requirements if no alias were used, namely
    -- @([(Color,Power)],Power)@
    ProblemReq,

    -- * Newtypes

    -- | These newtypes are all used as wrappers around various
    -- primitive Haskell types, but in such a way that they can be
    -- distinguished from one another and from their primitive contents,
    -- for the primary purpose of creating IxSets of Cards with
    -- unambiguous key specification, as well as for allowing
    -- instantiation of various typeclasses.
    
    -- ** Integral Newtypes

    -- | The following newtypes are all wrapped integers, all of which
    -- implement the "Hint" typeclass.

    -- | A representation of a card's power, for confrontations and faceoffs
    Power(..),

    -- | A representation of a card's cost, for action accounting during the
    -- Mane phase.
    Cost(..),

    -- | A representation of a card's colored power-requirements, for
    -- playing cards during the Mane phase.
    Req(..),

    -- | A representation of the number of points a card is worth, for
    -- successful troublemaker faceoffs or problem confrontations
    Points(..),

    -- ** String Newtypes

    -- | The following newtypes are all wrapped strings, all of which
    -- implement the "Stringe" typeclass.

    -- | A type for representing the keywords of a card
    Keyword(..),

    -- | A type for representing a card's descriptive text
    Text(..),

    -- * Algebraic Data Types

    -- ** Color

    -- | The Color ADT consists of unary constructors representing each of
    -- the possible colors in the CCG, namely the six primary
    -- colors, their complementary specifications, and the "Wild" color
    -- specification.
    Color(..),

    -- | 'readC' is a simple function for converting strings into Colors,
    -- taking the empty string as "Wild" and 'read'ing any other string
    -- using the @Read Color@ instance.
    readC,

    -- | A list of all of the spectral colors, in ascending order; the
    -- spectral class of the invotomorphism on color
    spect,

    -- | 'aspect' is a list of all the aspectral colors, which are the
    -- inverted specifications of the spectral colors, in ascending order;
    -- the aspectral class of the invotomorphism on color
    aspect,

    -- | 'invert' is the invotomorphism on Colors defined by the
    -- @Invotomorph Color@ instance, which maps @X -> NotX@, @NotX -> X@,
    -- and @Wild -> Wild@. By the definition of the Invotomorph typeclass,
    -- this is guaranteed to be an involution, so @invert.invert == id@
    invert,

    -- | 'classify' takes any color and determines all of the colors
    -- that can coincide with it, namely itself and the inverses of the
    -- other members of its invotomorphism class. For Wild, a list of all
    -- the colors is returned
    --
    -- >>> classify Blue
    -- [Blue, NonYellow, NonPurple, NonPink, NonWhite, NonOrange]
    -- >>> classify NonOrange
    -- [NonOrange, Blue, Yellow, Purple, Pink, White]
    -- >>> classify Wild
    -- [Wild, Blue, Yellow, Purple, Pink, White, Orange, NonBlue, NonYellow, NonPink, NonWhite, NonOrange]
    classify,

    -- | Boolean test for whether a color is spectral (i.e. in the
    -- Blue-Orange range)
    spectral,

    -- | Boolean test for whether a color is aspectral (i.e. in the
    -- NonBlue-NonOrange range)
    aspectral,

    -- | Boolean test for whether a color is equal to "Wild"
    isWild,

    -- ** Abbrev Instances

    -- | CSet is an ADT for card sets, which makes it prone to changes
    -- over time if new sets are released.
    CSet(..),

    -- | A representation for the rarity of a card
    Rarity(..),

    -- ** CardType

    -- | A representation for card-types, used for reflection for Cards,
    -- records of GenCards, and query parameters in the API. The
    -- constructor names are qualified with leading \'T\'s in order to
    -- avoid namespace conflicts with the constructors of 'Card'
    CardType(..),

    -- | A Dimorph between CardType and String, as direct Read/Show
    -- derivations are not possible due to the qualification of the
    -- constructor names; this Dimorph is a conversion between the
    -- constructor names and their unqualified string representations.
    transCT,

    -- * Non-Algebraic Data Types

    -- ** Number

    -- | A representation of card enumeration within each card-set, with
    -- constructors for each mode of enumeration used in the CCG. 
    Number(..), 

    -- | A basic string-reader for Number
    readN
    )
    where

import CCG.Cards.Common.Invotomorph
import CCG.Cards.Common.Dimorph
import Data.Data (Data, Typeable)
import Data.Char
import Data.List
import Util.Helper

type Keywords = [Keyword]
type Name = String
type ProblemReq = ([(Color,Power)],Power)

newtype Power = Power Int deriving (Read, Show, Ord, Eq, Data, Typeable)
newtype Cost = Cost Int deriving (Read, Show, Ord, Eq, Data, Typeable)
newtype Req = Req Int deriving (Read, Show, Ord, Eq, Data, Typeable)
newtype Points = Points Int deriving (Read, Show, Ord, Eq, Data, Typeable)

newtype Keyword = Keyword String deriving (Ord, Eq, Data, Typeable)
newtype Text = Text String deriving (Ord, Eq, Data, Typeable)

data Color =    Blue |    Yellow |    Purple |    Pink |    White |    Orange
           | NonBlue | NonYellow | NonPurple | NonPink | NonWhite | NonOrange
           | Wild deriving (Show, Enum, Read, Eq, Ord, Data, Typeable)

instance Invotomorph Color where
    classX = spect
    classY = aspect

readC :: String -> Color
readC "" = Wild
readC x = read x :: Color

spect, aspect :: [Color]
spect = [Blue .. Orange]
aspect = [NonBlue .. NonOrange]

invert :: Color -> Color
invert = invoto

classify :: Color -> [Color]
classify x | spectral x = x:(aspect\\[invert x])
           | aspectral x = x:(spect\\[invert x])
           | otherwise = Wild:(spect++aspect)

spectral, aspectral, isWild :: Color -> Bool
spectral = (`elem`spect)
aspectral = (`elem`aspect)
isWild = (==Wild)

-- Abbreviated strings

data CSet = Premiere
          | CanterlotNights
          | RockNRave
          | CelestialSolstice
          | CrystalGames
          deriving (Enum, Eq, Ord, Data, Typeable)

data Rarity = Common
            | Uncommon
            | Fixed
            | Rare
            | UltraRare
            | Promotional
            deriving (Enum, Eq, Ord, Show, Read, Data, Typeable)

data CardType = TMane
              | TFriend
              | TResource
              | TEvent
              | TTroublemaker
              | TProblem
              deriving (Enum, Eq, Ord, Data, Typeable)

transCT :: Dimorph CardType String
transCT = let x = [ TMane  , TFriend  , TResource  , TEvent  , TTroublemaker  , TProblem  ]
              y = [ "Mane" , "Friend" , "Resource" , "Event" , "Troublemaker" , "Problem" ]
          in Dimorph x y

instance Show CardType where
    show = to transCT

readsT :: ReadS CardType
readsT = rdr $ from transCT

instance Read CardType where
    readsPrec = const readsT

-- Number

data Number = Regular Int
            | F Int
            | PF Int
            deriving (Eq, Ord, Data, Typeable)

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

instance Read Number where
    readsPrec = const $ rdr readN
