{-# LANGUAGE TemplateHaskell #-}
module Cards.Safe where

import Cards
import Cards.Common
import Cards.Generic
import Cards.Common.Color
import Data.SafeCopy (base, deriveSafeCopy)

$(deriveSafeCopy 0 'base ''Color)
$(deriveSafeCopy 0 'base ''Rarity)
$(deriveSafeCopy 0 'base ''Number)
$(deriveSafeCopy 0 'base ''CSet)
$(deriveSafeCopy 0 'base ''CardType)
$(deriveSafeCopy 0 'base ''Card)
$(deriveSafeCopy 0 'base ''GenCard)
