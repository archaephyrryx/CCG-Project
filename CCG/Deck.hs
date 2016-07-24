{-# LANGUAGE RecordWildCards #-}
module CCG.Deck where
-------------------------------------------------
import Prelude hiding (div)
import Data.Maybe
import Data.List.Split
import Data.List
--------------------------------------------------
import CCG.Cards
import CCG.Cards.Generic
import CCG.Cards.Common
---------------------------------------------------
import Util.Tuple
---------------------------------------------------

-- * List-Based Decks
-- The following implementations are all list-based, with a possible
-- Deck data-type in a later version. This List implementation is not
-- optimized or embellished, though it may be once the main App
-- components are fully implemented

-- | Type representing a "Deck", a specifically structured collection of
-- "Card"s
type Deck = [Card]

-- | Type representing a contiguous or cohesive Part of a "Deck"
type DeckP = [Card]

-- | Completely empty "Deck" primitive instance
emptyDeck :: Deck
emptyDeck = []

-- | Adds a "Card" to a "Deck" without validation
addCard :: Card -> Deck -> Deck
addCard c = (c:)

-- | Given a "Card", 'dLim' returns the maximum number of that type of
-- card that may be included in a normal, non-draft "Deck"
dLim :: Card -> Int
dLim c@Mane{..} = 1
dLim c@Problem{..} = 2
dLim _ = 3

-- | If a "Card" is present in a "Deck", 'incCard' adds another copy if
-- the limit has not been reached, or if draft-mode is on
incCard :: Bool -- ^ Draft-mode boolean
        -> Card -- ^ Card to add
        -> Deck -- ^ Deck to insert card into
        -> Deck -- ^ Deck, unchanged or with 1 more copy of the card
incCard dm c d
  | d`has`c && (dm || d`hasN`c < dLim c) = addCard c d
  | otherwise = d

-- | Removes one copy of a "Card" from a "Deck"
decCard :: Card -- ^ Card to remove
        -> Deck -- ^ Deck to remove card from
        -> Deck -- ^ Deck with one fewer copies of the card
decCard c d = delete c d

-- | Test for inclusion of a "Card" in a "Deck"
has :: Deck -> Card -> Bool
has d c = c`elem`d

-- | Test for exclusion of a "Card" from a "Deck"
lacks :: Deck -> Card -> Bool
lacks d c = c`notElem`d

-- | Counts number of copies of a "Card" in a "Deck"
hasN :: Deck -> Card -> Int
hasN d c
  | has d c = length (filter (==c) d)
  | otherwise = 0

-- | Given a Problem-"DeckP", determines whether there is any starting
-- problem (tested, rather than retrieved as metadata)
hasStarting :: DeckP -> Bool
hasStarting = any (isPrefixOf "Starting Problem" . unravel . text)

-- | Split a "Deck" into a "DeckP" 3-tuple
-- (Mane, Problem, Draw)
tpart :: Deck -> (DeckP, DeckP, DeckP)
tpart d = foldr tpartition ([],[],[]) d
  where
    tpartition :: Card -> (DeckP, DeckP, DeckP) -> (DeckP, DeckP, DeckP)
    tpartition c@Mane{..} = mhall ((c:),id,id)
    tpartition c@Problem{..} = mhall (id,(c:),id)
    tpartition c = mhall (id,id,(c:))

-- | Dismantles a "DeckP" into its underlying structure, of a "Card" count-set
-- (as "Card"-Int pairlist)
struct :: DeckP -> [(Card,Int)]
-- list-specific implementation
struct dp = let g = group . sort $ dp
                l = map length g
                u = map head g
            in zip u l
