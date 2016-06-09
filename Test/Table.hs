module Test.Table where

import Widgets.Core
import Widgets.Table
import Util.List

main :: IO ()
main = start test

test :: IO ()
test = do
    f <- frame [text := "Test"]
    t <- table f []

    b1 <- button t [text := "foo"]
    b2 <- button t [text := "bar"]
    --set t [layout := grid 5 5 $ mono [widget b1, widget b2]]
    let networkDescription :: MomentIO ()
        networkDescription = mdo
          set b1 [ on command := (button t [text := "new"] >>= \x -> set t [layout :~ (\prev -> column 5 $ [prev, widget x])]) ]

    set f [layout := widget $ t]
    network <- compile networkDescription
    actuate network
