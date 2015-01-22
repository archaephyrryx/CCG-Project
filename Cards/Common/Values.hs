module Cards.Common.Values (setValues, rarityValues, typeValues, colorValues) where

import Cards.Common
import Cards.Common.Color

setValues :: [CSet]
setValues = [Premiere, CanterlotNights, RockNRave, CelestialSolstice, CrystalGames]

rarityValues :: [Rarity]
rarityValues = [Fixed, Common, Uncommon, Rare, UltraRare, Promotional]

typeValues :: [CardType]
typeValues = [TMane, TFriend, TResource, TEvent, TTroublemaker, TProblem]

colorValues :: [Color]
colorValues = [Wild, Blue, Yellow, Purple, Pink, White, Orange]
