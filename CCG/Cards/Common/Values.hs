{-# LANGUAGE FlexibleInstances, UndecidableInstances #-} 

module CCG.Cards.Common.Values (setValues, rarityValues, typeValues, colorValues) where

import CCG.Cards.Common.Types

class Valuable a where
    values :: [a]

instance Enum a => Valuable a where
    values = [toEnum 0..]

setValues :: [CSet]
setValues = values
--setValues = [Premiere, CanterlotNights, RockNRave, CelestialSolstice, CrystalGames]

rarityValues :: [Rarity]
rarityValues = values
--rarityValues = [Fixed, Common, Uncommon, Rare, UltraRare, Promotional]

typeValues :: [CardType]
typeValues = values
--typeValues = [TMane, TFriend, TResource, TEvent, TTroublemaker, TProblem]

colorValues :: [Color]
colorValues = Wild:[Blue .. Orange]
