{-# LANGUAGE RecursiveDo #-}

module Test.Adder where

import Reactive.Banana
import Reactive.Banana.WX
import Graphics.UI.WX.Attributes
import Graphics.UI.WX hiding (Event, newEvent, empty, Identity)
import Graphics.UI.WXCore hiding (Event, Timer, empty, Identity, newEvent)
import Graphics.UI.WXCore.Frame

-- | Combine Unit-Events
anyEvent :: [Event ()] -> Event ()
anyEvent = foldl1 (unionWith (\_ _ -> ()))

-- | Unsugared if-then-else function
if_ :: Bool -> a -> a -> a
if_ True x _ = x
if_ False _ y = y

-- | Apply a function to the value at an index, or return a default value
-- if the index is out of range
(!?) :: (a -> b) -> b -> Int -> ([a] -> b)
(f!? ~y) n xs
  | n < 0 = y
  | otherwise = case drop n xs of
                  x:_ -> f x
                  [] -> y

main :: IO ()
main = start test


create :: Window w -> Int -> Behavior Int -> Event Int -> Event () -> MomentIO (StaticText ())
create t i bi ei eRef = do
  let tx = replicate i '\t' ++ show i

  x <- liftIO $ staticText t [ text := tx ]

  let beq = (==i) <$> bi

  let eMe = filterE (==i) ei

  sink x [ visible :== beq ]
  reactimate (refresh x <$ anyEvent [ eRef, () <$ eMe ])
  return x

test :: IO ()
test = do
    f <- frame [text := "Test"]

    add <- button f [ text := "+" ]

    prv <- button f [ text := "<" ]
    cur <- staticText f []
    nxt <- button f [ text := ">" ]

    tab <- panel f [ clientSize := sz 200 300 ]
    deb <- staticText f []
    ref <- button f [ text := "refresh" ]


    let networkDescription :: MomentIO ()
        networkDescription = mdo
                eAdd <- event0 add command
                eRef <- event0 ref command

                let bNotFirst = (>0) <$> bCur
                    bNotLast  = (<) <$> bCur <*> bNext

                sink prv [ enabled :== bNotFirst ]
                sink cur [ text :== show <$> bCur ]
                sink nxt [ enabled :== bNotLast ]

                ePrev <- event0 prv command
                eNext <- event0 nxt command

                let eDelta :: Enum n => Event (n -> n)
                    eDelta = unions [ pred <$ whenE bNotFirst ePrev
                                    , succ <$ whenE bNotLast  eNext ]
                    eChange = flip ($) <$> bCur <@> eDelta

                bCur <- stepper 0 $ eChange

                (eIndex, bCount) <- mapAccum 0 ((\x -> (x, succ x)) <$ eAdd)

                let bView = (\n i -> if_ (n==0) (0) i) <$> bCount <*> bCur
                    bNext = pred <$> bCount
                    eCreate = (\n -> create tab n bView eChange $ anyEvent [eRef,eAdd]) <$> eIndex

                reCreate <- execute eCreate

                bItemer <- accumB id $ flip (.) . (:) <$> reCreate
                let bItems = ($[]) <$> bItemer
                    bThis = (widget!?(nullLayouts!!0)) <$> bCur <*> bItems

                sink tab [ layout :== bThis ]
                liftIO $ set f [ layout := column 5 [ margin 10 $ row 5 [ widget add
                                                                        , widget prv
                                                                        , widget cur
                                                                        , widget nxt
                                                                        , widget ref
                                                                        ]
                                                    , fill $ widget tab
                                                    ]
                               ]

    network <- compile networkDescription
    actuate network
