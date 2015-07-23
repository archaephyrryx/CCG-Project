{-# LANGUAGE ScopedTypeVariables, RecursiveDo, NoMonomorphismRestriction #-}

module Test where

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
import Widgets.MultiSelect
import Data.List
import Tidings

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
    clear <- button f [text := "clear"]
    choicer <- multiListBox f []
    choice <- staticText f []

    set f [layout := margin 10 $ row 5 $ [minsize (sz 200 300) $ widget choicer, widget choice] ]

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = mdo
                eClear <- event0 clear command

                tSelections <- multiSelect choicer bAnthology bSelections bDisplay

                let eSelections :: Event t [String]
                    eSelections = rumors tSelections
                
                    bSelections :: Behavior t [String]
                    bSelections = stepper [] $ unions [ eSelections, [] <$ eClear ]

                    bResults :: Behavior t String
                    bResults = intercalate ", " <$> bSelections

                    bAnthology :: Behavior t [String]
                    bAnthology = pure anthology

                    bDisplay :: Behavior t (String -> String)
                    bDisplay = pure id

                sink choice [ text :== bResults ]

    network <- compile networkDescription
    actuate network
