{-# LANGUAGE ScopedTypeVariables, RecursiveDo, NoMonomorphismRestriction #-}

module Test.Obscura where

import Widgets.Core
import Widgets.Obscura
import Widgets.Links
import Widgets.Ranger
import Data.List
import Util (mono)

main :: IO ()
main = start test

test :: IO ()
test = do
    f <- frame [text := "Test"]
    d <- staticText f []
    o <- bitmapButton f []
    r <- range f

    set f [layout := margin 10 $ column 5 $ [margin 10 $ row 5 $ [ widget d, widget o ], widget r]]

    let networkDescription :: MomentIO ()
        networkDescription = mdo
                ranged <- ranger r cur (pure 'a') (pure 'z') (pure renderValue)

                let tRanger :: Tidings Char
                    tRanger = tide ranged

                    eLoc :: Event Char
                    eLoc = rumors $ tRanger

                cur <- stepper 'a' $ eLoc

                obscured <- obscura o (pure charToPath) cur

                let
                    tAction :: Tidings Char
                    tAction = tide obscured

                    eAct :: Event Char
                    eAct = rumors tAction

                act <- stepper 'a' $ eAct

                sink d [ text :== mono act ]

    network <- compile networkDescription
    actuate network

charToPath :: Char -> String
charToPath c = "/home/peter/ccg/CCG-Project/res/test/" ++ (c:".png")
