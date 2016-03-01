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
    set t [layout := grid 5 5 $ mono [widget b1, widget b2]]
    set b1 [ on command := ]
    set f [layout := widget $ t]
