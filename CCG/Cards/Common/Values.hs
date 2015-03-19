module Cards.Common.Values (setValues, rarityValues, typeValues, colorValues) where

import Cards.Common.Types

setValues :: [CSet]
setValues = [Premiere, CanterlotNights, RockNRave, CelestialSolstice, CrystalGames]

rarityValues :: [Rarity]
rarityValues = [Fixed, Common, Uncommon, Rare, UltraRare, Promotional]

typeValues :: [CardType]
typeValues = [TMane, TFriend, TResource, TEvent, TTroublemaker, TProblem]

colorValues :: [Color]
colorValues = [Wild, Blue, Yellow, Purple, Pink, White, Orange]
