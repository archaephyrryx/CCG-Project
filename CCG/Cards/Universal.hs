{-# LANGUAGE Rank2Types #-}
module CCG.Cards.Universal where

import CCG.Cards
import CCG.Cards.Generic
import CCG.Cards.Common
import Control.Applicative

type Unifier x = UniCard a => a -> x

class UniCard a where
    uname     :: a -> Name
    uset      :: a -> CSet
    unum      :: a -> Number
    urar      :: a -> Rarity
    ukeywords :: a -> Keywords
    utext     :: a -> Text
    utype     :: a -> CardType
    asGeneric :: a -> GenCard
    asNatural :: a -> Card
    ---
    ucolor    :: a -> Maybe Color
    ucost     :: a -> Maybe Cost
    ureq      :: a -> Maybe Req
    upower    :: a -> Maybe Power
    uboosted  :: a -> Maybe Power
    upoints   :: a -> Maybe Points
    upreqs    :: a -> Maybe ProblemReq
    --
    upower = mpower . asGeneric
    ucolor = mcolor . asGeneric
    ucost = mcost . asGeneric
    ureq = mreq . asGeneric
    uboosted = mboosted . asGeneric
    upoints = mpoints . asGeneric
    upreqs = mpreqs . asGeneric

instance UniCard Card where
    uname = name
    uset = set
    unum = num
    urar = rar
    ukeywords = keywords
    utext = text
    utype = cardtype
    asGeneric = toGeneric
    asNatural = id

instance UniCard GenCard where
    uname = gname
    uset = gset
    unum = gnum
    urar = grar
    ukeywords = gkeywords
    utext = gtext
    utype = ctype
    asGeneric = id
    asNatural = fromGeneric

typename :: UniCard c => c -> String
typename = (++) <$> show.utype <*> (": "++).uname

setnum :: UniCard c => c -> String
setnum = (++) <$> brief.uset <*> show.unum

instance Show GenCard where
    show = setnum
