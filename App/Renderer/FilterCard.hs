{-# LANGUAGE RecordWildCards #-}

module App.Renderer.FilterCard where

import Cards
import Cards.Common
import Cards.Common.Abbrev
import Cards.Common.Color
import Cards.Common.Hint
import Cards.Common.Stringe
import Cards.Common.Values
import Cards.Differentiation
import Cards.Generic
--------------------------
import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.IORef
-----------------------------
import qualified Graphics.UI.Threepenny          as UI
import qualified Graphics.UI.Threepenny.Core     as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import Graphics.UI.Threepenny.Core hiding (get, set)
-----------------------------
import App.Core.Helper
import App.Renderer.Cards
import App.Widgets
import App.Core.Modes
import App.Core
-----------------------------

render :: [GenCard] -> (GenCard -> UI Element) ->[UI Element]
render matches pronounce =
  let theader = UI.tr #+ (map (\x -> UI.th #+ [string x]) ["#", "Rarity", "Type", "Cost", "Req.", "Name", "Power"]);
      trows = for matches (\g@GenCard{..} ->
          UI.tr #+ (map (\x -> UI.td #+ [x]) $
            [ UI.string $ genset g
            , UI.string $ brief rar
            , iconic ctype
            , UI.string $ fromMaybe "" (show.val <$> mcost)
            , reqtify g
            , pronounce g
            , empower g
            ])
          );
  in [ UI.table #+ (theader:trows) ]
