{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Test.Hide where


import           Control.Monad       (void, forM)
import           Data.List
import           Reactive.ValText
import           Util                hiding (Visible, visible)
import           Widgets.Core        hiding (mask, Row)
import           Widgets.Links
import           Widgets.MultiSelect
import           Widgets.Ranger
import           Widgets.Table
import           Widgets.Text

lyrics :: [[String]]
lyrics = map words ["Senbonzakura yoru ni magire"
                   ,"Kimi no koe ga todokanaiyo"
                   ,"Kore wa utage hagane no ori"
                   ,"Sono dantoudai wo miomoshite"
                   ]


main :: IO ()
main = start test

test :: IO ()
test = do
    f <- frame [text := "Test (hiding)"]

    r <- range f


    let networkDescription :: MomentIO ()
        networkDescription = mdo
          ran <- ranger r verse (pure 0) (pure 3) (pure show)
          stanzas <- forM lyrics (\xs -> forM xs (\x -> (liftIO . preText) f >>= \e -> rText e (Static x) never))

          verse <- stepper 0 $ portents ran
          let
            lines :: [[Row]]
            lines = map (map (Row . one . Item)) stanzas
            allLines :: [Row]
            allLines = concat lines
            indices :: [[Int]]
            indices = count (map length lines)
            bLines :: Behavior [Row]
            bLines = (lines!!) <$> omens ran
            bShows :: Behavior ([Row] -> [Bool])
            bShows = verity <$> ((indices!!) <$> omens ran)

          redrawRows bShows (pure allLines)
          liftIO $ set f [layout := margin 10 $ column 10 $ widget r:(map widget allLines)]

    network <- compile networkDescription
    actuate network
