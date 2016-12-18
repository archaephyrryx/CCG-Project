module App.Mode where

data Mode = Home
          | FilterCard
          | ShowCard
          | DeckBuilder
          deriving (Eq, Enum, Ord, Read, Show)

modes :: [Mode]
modes = [Home .. DeckBuilder]
