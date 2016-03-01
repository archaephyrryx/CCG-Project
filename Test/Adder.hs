{-# LANGUAGE RecursiveDo #-}

module Test.Adder where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators
import Reactive.Banana.WX
import Graphics.UI.WX
import Graphics.UI.WXCore

main :: IO ()
main = start test

test :: IO ()
test = do
    f <- frame [text := "Test"]

    add <- button f [ text := "+" ]
    next <- button f [ text := ">" ]
    prev <- button f [ text := "<" ]
    pos <- staticText f [ text := "0" ]

    let networkDescription :: MomentIO ()
        networkDescription = mdo
                eAdd  <- ((length <$> bItems) <@) <$> event0 add command
                ePrev <- event0 prev command
                eNext <- event0 next command

                bNext <- accumB 0 $ succ <$ eAdd
                reAdd <- mapEventIO (\n -> staticText f [ text := replicate n '\t' ++ show n ]) eAdd

                let eVis = (\x -> sink x [ visible :== elem x <$> bThis ] >> return x) <$> reAdd
                reVis <- execute eVis

                let eAll = flip (:) <$> bItems <@> reVis
                bItems <- stepper [] eAll
                let eChange :: Reactive.Banana.Event (Int -> Int)
                    eChange = unions [whenE bNotFirst (pred <$ ePrev), whenE bNotLast (succ <$ eNext)]
                bCur <- accumB 0 eChange

                let bNotFirst = (>0) <$> bCur
                    bNotLast = (\x y -> x + 1 < y) <$> bCur <*> (length <$> bItems)
                    bThis = (\i xs -> if null xs then [] else [xs!!i]) <$> bCur <*> bItems

                sink pos [ text :== show <$> bCur ]
                sink f [ layout :== (grid 5 5 . ([widget add, widget prev, widget pos, widget next]:) . map (\x -> [widget x])) <$> bThis]
    network <- compile networkDescription
    actuate network
