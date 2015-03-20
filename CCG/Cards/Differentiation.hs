module CCG.Cards.Differentiation where

import qualified CCG.Cards as C
import qualified CCG.Cards.Generic as G
import CCG.Cards (Card)
import CCG.Cards.Generic (GenCard)
import CCG.Cards.Common

cname     :: Card    -> Name
gname     :: GenCard -> Name

cname = C.name
gname = G.name

cset      :: Card    -> CSet
gset      :: GenCard -> CSet

cset = C.set
gset = G.set

cnum      :: Card    -> Number
gnum      :: GenCard -> Number

cnum = C.num
gnum = G.num

crar      :: Card    -> Rarity
grar      :: GenCard -> Rarity

crar = C.rar
grar = G.rar

ckeywords :: Card    -> Keywords
gkeywords :: GenCard -> Keywords

ckeywords = C.keywords
gkeywords = G.keywords

ctext     :: Card    -> Text
gtext     :: GenCard -> Text

ctext = C.text
gtext = G.text
