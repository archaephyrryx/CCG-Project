module App.Core.AppData where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Cards.Common
import Cards.Common.Color
import Cards.Generic
import Data.Maybe
import App.Widgets
import App.MicroScope
import Graphics.UI.Threepenny.Core

data AEL = FCEL { minCost :: Min Cost
                , maxCost :: Max Cost
                , minPow :: Min Power
                , maxPow :: Max Power
                , minReq :: Min Req
                , maxReq :: Max Req
                , selectCol :: MultiSelect Color
                , selectRar :: MultiSelect Rarity
                , selectSet :: MultiSelect CSet
                , selectTyp :: MultiSelect CardType
                , prevPage :: Element
                , pageNo   :: Element
                , nextPage :: Element
                }
         | SCEL {- prevCard :: LiquidLink GenCard
                , curCard :: Element
                , nextCard :: LiquidLink GenCard
                -}
         | DBEL { selectCol :: MultiSelect Color
                , selectRar :: MultiSelect Rarity
                , selectSet :: MultiSelect CSet
                , selectTyp :: MultiSelect CardType
                , prevPage :: Element
                , pageNo   :: Element
                , nextPage :: Element
                }

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
                }
         | SCBL {- bMicro :: Behavior (MicroScope GenCard)
                , bPrev  :: BGC
                , -} { bCur :: BGC } {-
                , bNext :: BGC
                -}
         | DBBL { bTypSelect :: BLT
                , bSetSelect :: BLS
                , bRarSelect :: BLR
                , bColSelect :: BLC
                }


type BLT = Behavior ([CardType])
type BLS = Behavior ([CSet])
type BLR = Behavior ([Rarity])
type BLC = Behavior ([Color])
type BMR = Behavior (Maybe Req)
type BMP = Behavior (Maybe Power)
type BMC = Behavior (Maybe Cost)

type BGC = Behavior GenCard
