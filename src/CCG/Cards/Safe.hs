{-# LANGUAGE TemplateHaskell #-}
module CCG.Cards.Safe where

import CCG.Cards
import CCG.Cards.Common
import CCG.Cards.Generic
import Data.SafeCopy (base, deriveSafeCopy)

$(deriveSafeCopy 0 'base ''Color)
$(deriveSafeCopy 0 'base ''Rarity)
$(deriveSafeCopy 0 'base ''Number)
$(deriveSafeCopy 0 'base ''CSet)
$(deriveSafeCopy 0 'base ''CardType)
$(deriveSafeCopy 0 'base ''Card)
$(deriveSafeCopy 0 'base ''GenCard)
