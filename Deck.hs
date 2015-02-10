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
import Cards.Common.Color
import Cards.Common.Stringe
import Cards.Common.Hint
import Cards.Common.Abbrev
import Cards.Common.Values
import Cards.Differentiation
---------------------------------------------------
import App.Core.Helper
---------------------------------------------------

type Deck = [Card]
type DeckP = [Card]

emptyDeck :: Deck
emptyDeck = []

addCard :: Card -> Deck -> Deck
addCard c = (c:)

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
