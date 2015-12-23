{-# LANGUAGE ScopedTypeVariables, RecursiveDo, NoMonomorphismRestriction #-}

module Test where

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
import Widgets.MultiSelect
import Widgets.Links
import Data.List
import Tidings

main :: IO ()
main = start test

test :: IO ()
test = do
    f <- frame [text := "Test"]

    counter <- staticText f []
    inc <- button f [ text := "+0" ]
    plus <- button f [ text := "+" ]


    set f [layout := margin 10 $ row 5 $ [minsize (sz 200 300) $ widget counter, widget inc, widget plus] ]

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = mdo
                bPlus <- softLink plus 1
                tInc <- liquidLink inc (pure (('+':).show)) (accumB 0 $ (+) <$> rumors bPlus)

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

                    eInc :: Event t Int
                    eInc = rumors tInc

                    bCount :: Behavior t Int
                    bCount = accumB 0 $ (+) <$> eInc

                sink choice [ text :== bResults ]
                sink counter [ text :== (show <$> bCount) ]

    network <- compile networkDescription
    actuate network
