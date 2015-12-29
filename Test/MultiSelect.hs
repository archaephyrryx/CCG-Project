{-# LANGUAGE ScopedTypeVariables, RecursiveDo, NoMonomorphismRestriction #-}

module Test where

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

    counter <- staticText f []
    inc <- button f [ text := "+0" ]
    plus <- button f [ text := "+" ]


    set f [layout := margin 10 $ row 5 $ [minsize (sz 200 300) $ widget choicer, widget choice, widget counter, widget inc, widget plus] ]

    let networkDescription :: MomentMonad m => m ()
        networkDescription = mdo
                eClear <- event0 clear command

                tSelections <- multiSelect choicer bAnthology bSelections bDisplay

                bPlus <- softLink plus 1
                tInc <- liquidLink inc (pure (('+':).show)) (accumB 0 $ (+) <$> rumors bPlus)

                let eSelections :: Event [String]
                    eSelections = rumors tSelections

                    bSelections :: Behavior [String]
                    bSelections = stepper [] $ unions [ eSelections, [] <$ eClear ]

                    bResults :: Behavior String
                    bResults = intercalate ", " <$> bSelections

                    bAnthology :: Behavior [String]
                    bAnthology = pure anthology

                    bDisplay :: Behavior (String -> String)
                    bDisplay = pure id

                    eInc :: Event Int
                    eInc = rumors tInc

                    bCount :: Behavior Int
                    bCount = accumB 0 $ (+) <$> eInc

                sink choice [ text :== bResults ]
                sink counter [ text :== (show <$> bCount) ]

    network <- compile networkDescription
    actuate network
