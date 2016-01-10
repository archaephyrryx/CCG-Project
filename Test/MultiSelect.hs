{-# LANGUAGE ScopedTypeVariables, RecursiveDo, NoMonomorphismRestriction #-}

module Test.MultiSelect where

import Widgets.MultiSelect
import Widgets.Links
import Widgets.Core
import Data.List

main :: IO ()
main = start test

anthology :: [String]
anthology =
  [ "A Midsummer Night's Dream"
  , "A Comedy of Errors"
  , "Much Ado About Nothing"
  , "King Lear"
  , "Richard III"
  , "Romeo and Juliet"
  , "Macbeth"
  , "Hamlet"
  , "The Tempest"
  , "Twelfth Night"
  , "Taming of the Shrew"
  , "Merchant of Venice"
  , "Othello"
  ]

test :: IO ()
test = do
    f <- frame [text := "Test"]
    clear <- button f [text := "clear"]
    choicer <- multiListBox f []
    choice <- staticText f []

    set f [layout := margin 10 $ row 5 $ [minsize (sz 200 300) $ widget choicer, widget choice, widget clear]]

    let networkDescription :: MomentIO ()
        networkDescription = mdo
                eClear <- eClick clear

                selector <- multiSelect choicer bAnthology bSelections bDisplay
                bSelections <- reSelections eSelections eClear

                let tSelections :: Tidings [String]
                    tSelections = tide selector

                    eSelections :: Event [String]
                    eSelections = rumors tSelections

                    bResults :: Behavior String
                    bResults = intercalate ", " <$> bSelections

                    bAnthology :: Behavior [String]
                    bAnthology = pure anthology

                    bDisplay :: Behavior (String -> String)
                    bDisplay = pure id

                sink choice [ text :== bResults ]

    network <- compile networkDescription
    actuate network

reSelections :: Event [a] -> Event () -> MomentIO (Behavior [a])
reSelections sel res = stepper [] $ unionWith triage sel reset
    where
        reset :: Event [a]
        reset = [] <$ res
        triage :: [a] -> [a] -> [a]
        triage _ [] = []
        triage x _ = x
