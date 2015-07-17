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

-- | Type alias for a function that displays a card as a string
type ShowCard = (Card -> String)

-- | Type alias for card collections, to avoid hard-coding any
-- particular container
type Cardlist = Set Card

{-|
  Multi-constructor \"Normal\" "Card" data-type with record fields
  appropriate to the card-type represented by each constructor. Note
  that the constructors themselves and their fields are all highly
  specialized for the MLP CCG in particular, and thus must be ported
  appropriately if ever used for a different CCG.
  -}
data Card = 
          -- | Constructor for \'Mane\' cards 
          Mane {
              name     :: Name
            , set      :: CSet
            , num      :: Number
            , rar      :: Rarity
            , keywords :: Keywords
            , color    :: Color
            , power    :: Power
            , boosted  :: Power
            , text     :: Text
            }
          | -- | Constructor for \'Friend\' cards 
            Friend {
              name     :: Name
            , set      :: CSet
            , num      :: Number
            , rar      :: Rarity
            , keywords :: Keywords
            , color    :: Color
            , cost     :: Cost
            , req      :: Req
            , power    :: Power
            , text     :: Text
            }
          | -- | Constructor for \'Resource\' cards
            Resource {
              name     :: Name
            , set      :: CSet
            , num      :: Number
            , rar      :: Rarity
            , keywords :: Keywords
            , color    :: Color
            , cost     :: Cost
            , req      :: Req
            , power    :: Power
            , text     :: Text
            }
          | -- | Constructor for \'Event\' cards
            Event {
              name     :: Name
            , set      :: CSet
            , num      :: Number
            , rar      :: Rarity
            , keywords :: Keywords
            , color    :: Color
            , cost     :: Cost
            , req      :: Req
            , power    :: Power
            , text     :: Text
            }
          | -- | Constructor for \'Troublemaker\' cards
            Troublemaker {
              name     :: Name
            , set      :: CSet
            , num      :: Number
            , rar      :: Rarity
            , keywords :: Keywords
            , power    :: Power
            , points   :: Points
            , text     :: Text
            }
          | -- | Constructor for \'Problem\' cards
            Problem {
              name     :: Name
            , set      :: CSet
            , num      :: Number
            , rar      :: Rarity
            , points   :: Points
            , keywords :: Keywords
            , preqs    :: ProblemReq
            , text     :: Text
            }
      deriving (Show, Data, Typeable)

-- | Reflection from "Card" to "CardType"
cardtype :: Card -> CardType
cardtype (Mane{..})     = TMane
cardtype (Friend{..})       = TFriend
cardtype (Resource{..}) = TResource
cardtype (Event{..})        = TEvent
cardtype (Troublemaker{..})= TTroublemaker
cardtype (Problem{..})      = TProblem

-- | "Eq" instance for "Card" testing for equality of names
instance Eq Card where
    (==) = (==)`on`name

-- | "Ord" instance for "Card" comparing card set and then card number
instance Ord Card where
    compare = (comparing set)`mappend`(comparing num)
