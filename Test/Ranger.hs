{-# LANGUAGE ScopedTypeVariables, RecursiveDo, NoMonomorphismRestriction #-}

module Test where

import Widgets.Core
import Widgets.Links
import Widgets.Ranger
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
    v <- staticText f []
    r <- range f

    set f [layout := grid 5 5 $ [[ widget v, widget r ]]]

    let networkDescription :: MomentIO ()
        networkDescription = mdo
                range <- ranger r bLoc bMin bMax (pure show)

                let tRanger :: Tidings Int
                    tRanger = tide range

                    bAnthology :: Behavior [String]
                    bAnthology = pure anthology

                    eLoc :: Event Int
                    eLoc = rumors $ tRanger

                    bMin :: Behavior Int
                    bMin = pure 0

                    bMax :: Behavior Int
                    bMax = pure (length anthology - 1)
                bLoc <- stepper 0 $ eLoc

                sink v [ text :== (!!) <$> bAnthology <*> bLoc ]

    network <- compile networkDescription
    actuate network
