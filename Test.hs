{-# LANGUAGE ScopedTypeVariables, RecursiveDo, NoMonomorphismRestriction #-}

module Test where

import Widgets.Core
import Widgets.Links
import Widgets.MultiSelect
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

    let networkDescription :: MomentIO ()
        networkDescription = mdo
                eClear <- event0 clear command

                tSelections <- multiSelect choicer bAnthology bSelections bDisplay

                bPlus <- softLink plus show (1 :: Int)
                bInc <- accumB 0 $ (+) <$> (rumors . tide $ bPlus)
                tInc <- liquidLink inc (pure (('+':).show)) bInc

                bSelections <- stepper [] $ priorityUnion [ eSelections, [] <$ eClear ]

                let eSelections :: Event [String]
                    eSelections = rumors . tide $ tSelections

                    bResults :: Behavior String
                    bResults = intercalate ", " <$> bSelections

                    bAnthology :: Behavior [String]
                    bAnthology = pure anthology

                    bDisplay :: Behavior (String -> String)
                    bDisplay = pure id

                    eInc :: Event Int
                    eInc = rumors . tide $ tInc

                bCount <- accumB 0 $ (+) <$> eInc

                sink choice [ text :== bResults ]
                sink counter [ text :== (show <$> bCount) ]

    network <- compile networkDescription
    actuate network
