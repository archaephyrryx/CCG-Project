{-# LANGUAGE ScopedTypeVariables, RecursiveDo, NoMonomorphismRestriction #-}

module Test where


import Widgets.MultiSelect
import Widgets.Core
import Widgets.Links
import Data.List

main :: IO ()
main = start test

test :: IO ()
test = do
    f <- frame [text := "Test"]

    counter <- staticText f []
    inc <- button f [ text := "+0" ]
    plus <- button f [ text := "+" ]


    set f [layout := margin 10 $ row 5 $ [minsize (sz 200 300) $ widget counter, widget inc, widget plus] ]

    let networkDescription :: MomentMonad m => m ()
        networkDescription = mdo
                bPlus <- softLink plus 1
                tInc <- liquidLink inc (pure (('+':).show)) (accumB 0 $ (+) <$> rumors bPlus)

                let eInc :: Event Int
                    eInc = rumors tInc

                    bCount :: Behavior Int
                    bCount = accumB 0 $ (+) <$> eInc

                sink counter [ text :== (show <$> bCount) ]

    network <- compile networkDescription
    actuate network
