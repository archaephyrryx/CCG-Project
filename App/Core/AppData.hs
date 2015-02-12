module App.Core.AppData where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Cards.Common
import Cards.Common.Color
import Cards.Generic
import Data.Maybe
import App.Widgets
import Data.IxSet
import App.MicroScope
import Graphics.UI.Threepenny.Core

-- | Shorthand type aliases

type BLT = Behavior ([CardType])
type BLS = Behavior ([CSet])
type BLR = Behavior ([Rarity])
type BLC = Behavior ([Color])
type BMR = Behavior (Maybe Req)
type BMP = Behavior (Maybe Power)
type BMC = Behavior (Maybe Cost)
type BBL = Behavior (Bool)
type BIG = Behavior (IxSet GenCard)
type BAM = Behavior (AppMode)

type BFL = Behavior Filter
type BGC = Behavior GenCard
type BDK = Behavior Deck
type BVM = Behavior ViewMode

-- | Universal Behavior List: used for app context passing
data UBL = UBL { bView :: BVM
               , abl :: ABL }

data ABL = FCBL { bTypSelect :: BLT
                , bSetSelect :: BLS
                , bRarSelect :: BLR
                , bColSelect :: BLC
                , bReqMin    :: BMR
                , bReqMax    :: BMR
                , bPowMin    :: BMP
                , bPowMax    :: BMP
                , bCostMin   :: BMC
                , bCostMax   :: BMC
                , bQFilter   :: BFL
                , bMatches   :: BIG
                }
         | SCBL { bSingle :: BGC }
         | DBBL { bTypSelect :: BLT
                , bSetSelect :: BLS
                , bRarSelect :: BLR
                , bColSelect :: BLC
                , bDraftCheck :: BBL
                , bQFilter   :: BFL
                , bMatches   :: BIG
                , bDeck      :: BDK
                }
