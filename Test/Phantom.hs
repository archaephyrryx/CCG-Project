{-# LANGUAGE RecursiveDo #-}

module Test.Phantom where


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
import           Widgets.Phantom

main :: IO ()
main = start test




test :: IO ()
test = do
  f <- frame [text := "Test"]
  b <- button f [ text := "Switch" ]

  counter <- staticText f [ ]

  inc <- button f []


  let networkDescription :: MomentIO ()
      networkDescription = mdo
          modeswap <- eClick b
          bMode <- restepper 0 (const.(1-)) $ modeswap


          lInc <- softLink inc (const "+1") ((+1) :: Int -> Int)
          bNum <- accumB 0 (blood lInc)
          sCounter <- rText' counter (mask bNum show) never

          ghost <- phantom [ aspect sCounter 0 Nothing
                           , aspect lInc 1 Nothing
                           ] bMode

          liftIO $ set f [ layout := (grid 5 5 $ [[widget b],[widget ghost]] )]
  network <- compile networkDescription
  actuate network
