{-# LANGUAGE ScopedTypeVariables, RecursiveDo, NoMonomorphismRestriction #-}

module Test.Mode where

import           Data.List
import           Data.Stringent
import           Util                (for, mono, verity)
import           Widgets.Cast
import           Widgets.Core        hiding (Row, cast, label, wrap)
import qualified Widgets.Core        as Core (cast)
import           Widgets.Links
import           Widgets.MultiSelect
import           Widgets.Ranger
import           Widgets.Table
import           Widgets.Phantom
import           Widgets.Group


data Mode = ModeA | ModeB | ModeC

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
    rang <- range f
    tab <- table f []
    act <- staticText f []
    debug <- staticText f []
    inc <- button f [text := "+"]

    set f [layout := margin 10 $ column 5 $ [row 5 [widget rang, widget inc], widget tab, widget act]]

    let networkDescription :: MomentIO ()
        networkDescription = mdo
            let bAnthology = pure anthology

 {-
                caster = Cask { bContents = pure anthology
                              , pagesize = 4
 -}
                caster = Case { contents = anthology
                              , bPagesize = bPage
                              , current = tidings bCurrent $ portents ranged
                              , format = Format { label = Static stringify
                                                , wrap = Static (\s l -> Row [Item l])
                                                , collect = Static id
                                                }
                              }

            ranged <- ranger rang bCurrent (pure 0) (bFinal caster) (pure stringify)
            cast <- genCast caster tab

            bCurrent <- stepper 0 $ portents ranged
            incs <- eClick inc
            bPage <- restepper 4 (#) $ whenE ((<(length . contents $ caster)) <$> bCurrent) $ (+1) <$ incs

--          bClicked <- stepper (-1) $ portents cast
            bClicked <- stepper (-1) $ priorityUnion ([0 <$ incs, portents cast])
            reactimate (refit tab <$ portents ranged)
            sink act [ text :== stringify <$> bClicked ]
            let
                bDebug :: Behavior String
                bDebug = show . map ((map _crux).unpack) <$> bRows
                bRows :: Behavior [Row]
                bRows = _rows . _elem . _cast $ cast
                bShows :: [Behavior Bool]
                bShows = for [0..] (\x -> (==x) <$> bCurrent)
                unpack :: Row -> [Link Int]
                unpack = \r -> extract $ _items r
                  where
                    extract :: [Item] -> [Link Int]
                    extract [] = []
                    extract ((Item x):t) = case Core.cast x of
                                             Just (x :: Link Int) -> x:(extract t)
                                             Nothing -> extract t

                bUnpacked :: Behavior [Link Int]
                bUnpacked = concatMap unpack <$> bRows

            sink debug [ text :== bDebug ]

    network <- compile networkDescription
    actuate network


{-# LANGUAGE ScopedTypeVariables, RecursiveDo, NoMonomorphismRestriction #-}

module Test where


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


{-# LANGUAGE RecursiveDo #-}

module Test.Adder where

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
