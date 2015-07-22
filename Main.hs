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
