module CCG.Cards.Universal where

import CCG.Cards
import CCG.Cards.Generic
import CCG.Cards.Common

class UniCard a where
    uname :: a -> Name
    uset  :: a -> CSet
    unum  :: a -> Number
    urar  :: a -> Rarity
    ukeywords :: a -> Keywords
    utext :: a -> Text

instance UniCard Card where
    uname = name
    uset = set
    unum = num
    urar = rar
    ukeywords = keywords
    utext = text

instance UniCard GenCard where
    uname = gname
    uset = gset
    unum = gnum
    urar = grar
    ukeywords = gkeywords
    utext = gtext
