{-# LANGUAGE RecordWildCards #-}

module Renderer.FilterCard where

import CCG
--------------------------
import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
-----------------------------
import qualified Graphics.UI.Threepenny            as UI
import qualified Graphics.UI.Threepenny.Core       as UI
import qualified Graphics.UI.Threepenny.Elements   as UI
import qualified Graphics.UI.Threepenny.Attributes as UI
import Graphics.UI.Threepenny.Core hiding (get, set)
-----------------------------
import Util
import Renderer.Cards
-----------------------------

render :: [GenCard] -> (GenCard -> UI Element) -> [UI Element]
render matches pronounce =
  let theader = UI.tr #+ (map (\x -> UI.th #+ [string x]) ["#", "Rarity", "Type", "Cost", "Req.", "Name", "Power"]);
      trows = for matches (\g@GenCard{..} ->
          UI.tr #+ (map (\x -> UI.td #+ [x]) $
            [ UI.string $ genset g
            , UI.string $ brief grar
            , iconic ctype
            , UI.string $ fromMaybe "" (show.val <$> mcost)
            , reqtify g
            , pronounce g
            , empower g
            ])
          );
  in [ UI.table #+ (theader:trows) ]
