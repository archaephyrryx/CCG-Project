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

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = mdo
                range <- ranger r bLoc bMin bMax (pure show)

                let tRanger :: Tidings t Int
                    tRanger = tide range

                    bAnthology :: Behavior t [String]
                    bAnthology = pure anthology

                    eLoc :: Event t Int
                    eLoc = rumors $ tRanger

                    bLoc :: Behavior t Int
                    bLoc = stepper 0 eLoc

                    bMin :: Behavior t Int
                    bMin = pure 0

                    bMax :: Behavior t Int
                    bMax = pure (length anthology - 1)

                sink v [ text :== (!!) <$> bAnthology <*> bLoc ]

    network <- compile networkDescription
    actuate network
