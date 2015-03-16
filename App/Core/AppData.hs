{-# LANGUAGE RecordWildCards, DoRec #-}

module App.Core.AppData where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Cards.Common
import Cards.Generic
import Data.Maybe
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

type BGC = Behavior GenCard

-- | Universal Behavior List
data UBL = FCBL { bTypSelect :: BLT
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
         | SCBL { bSingle :: BGC }
         | DBBL { bTypSelect :: BLT
                , bSetSelect :: BLS
                , bRarSelect :: BLR
                , bColSelect :: BLC
                }
