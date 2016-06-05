{-# LANGUAGE ScopedTypeVariables, RecursiveDo, NoMonomorphismRestriction #-}

module Main where

import Widgets.MultiSelect
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

    set f [layout := margin 10 $ grid 10 5 $ [[minsize (sz 200 300) $ widget choicer, glue, widget clear], [widget choice]] ]

    let networkDescription :: MomentIO ()
        networkDescription = mdo
                eClear <- event0 clear command

                tSelections <- multiSelect choicer bAnthology bSelections bDisplay

                let eSelections :: Event [String]
                    eSelections = portents tSelections

                bSelections <- stepper [] $ priorityUnion [ eSelections, [] <$ eClear ]

                let
                    bResults :: Behavior String
                    bResults = intercalate ", " <$> bSelections

                    bAnthology :: Behavior [String]
                    bAnthology = pure anthology

                    bDisplay :: Behavior (String -> String)
                    bDisplay = pure id

                sink choice [ text :== bResults ]

    network <- compile networkDescription
    actuate network

{-
 -
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
import Data.Maybe

main :: IO ()
main
  = start arithmetic >> start counter

arithmetic :: IO ()
arithmetic = do
    f      <- frame   [text := "Arithmetic"]
    input1 <- entry f []
    input2 <- entry f []
    output <- staticText f []

    set f [layout := margin 10 $ row 10
            [ widget input1, label "+", widget input2
            , label "=", minsize (sz 40 20) $ widget output]]

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            binput1 <- behaviorText input1 ""
            binput2 <- behaviorText input2 ""

            let
                result :: Behavior t (Maybe Int)
                result = f <$> binput1 <*> binput2
                    where
                      f x y = liftA2 (+) (readNumber x) (readNumber y)
                readNumber s = listToMaybe [x | (x,"") <- reads s]
                showNumber   = maybe "--" show

            sink output [ text :== showNumber <$> result]
    network <- compile networkDescription
    actuate network

counter :: IO ()
counter = do
    f <- frame [text := "Counter"]
    bup <- button f [text := "Up"]
    bdown <- button f [text := "Down"]
    output <- staticText f []

    set f [layout := margin 10 $
            column 5 [widget bup, widget bdown, widget output]]

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do

            eup   <- event0 bup   command
            edown <- event0 bdown command

            let
                counter :: Behavior t Int
                counter = accumB 0 $ ((+1) <$ eup) `union` (subtract 1 <$ edown)

            sink output [text :== show <$> counter]
    network <- compile networkDescription
    actuate network

-}
