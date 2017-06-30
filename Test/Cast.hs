{-# LANGUAGE ScopedTypeVariables, RecursiveDo, NoMonomorphismRestriction #-}

module Test.Cast where

import Widgets.Core hiding (label, wrap, Row, cast)
import qualified Widgets.Core as Core (cast)
import Widgets.Links
import Widgets.Cast
import Widgets.Table
import Widgets.Ranger
import Data.List
import Data.Stringent
import Util (verity, mono, for)

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
            reactimate (refit (_tab tab) <$ portents ranged)
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
