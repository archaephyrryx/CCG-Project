{-# LANGUAGE RecordWildCards #-} 
module Deck where
-------------------------------------------------
import Prelude hiding (div)
import Data.Maybe
import Data.List.Split
import Data.List
--------------------------------------------------
import Cards
import Cards.Generic
import Cards.Common
import Cards.Differentiation
---------------------------------------------------
import App.Core.Helper
---------------------------------------------------

-- |List-based implementation, to be altered only when the App itself is
-- working
type Deck = [Card]
type DeckP = [Card]

emptyDeck :: Deck
emptyDeck = []

addCard :: Card -> Deck -> Deck
addCard c = (c:)

incCard :: Bool -> Card -> Deck -> Deck
incCard dm c d | d`has`c && (dm || (d`hasN`c < 3)) = addCard c d
               | otherwise = d

decCard :: Card -> Deck -> Deck
decCard c d = delete c d

has :: Deck -> Card -> Bool
has d c = c`elem`d

hasN :: Deck -> Card -> Int
hasN d c | has d c = length (filter (==c) d)
         | otherwise = 0

hasStarting :: DeckP -> Bool
hasStarting = any (isPrefixOf "Starting Problem" . unravel . ctext)

tpart :: Deck -> (DeckP, DeckP, DeckP)
tpart d = foldr tpartition ([],[],[]) d
  where
    tpartition :: Card -> (DeckP, DeckP, DeckP) -> (DeckP, DeckP, DeckP)
    tpartition c@Mane{..} = mhall ((c:),id,id)
    tpartition c@Problem{..} = mhall (id,(c:),id)
    tpartition c = mhall (id,id,(c:))

struct :: DeckP -> [(Card,Int)]
-- list-specific implementation
struct dp = let g = group . sort $ dp
                l = map length g
                u = map head g
            in zip u l
