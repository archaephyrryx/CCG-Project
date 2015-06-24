{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, NamedFieldPuns, DeriveDataTypeable #-}

module CCG.Cards.Generic where

import CCG.Cards
import CCG.Cards.Common
import Data.List
import Data.Char
import Data.Set (Set)
import Data.Function (on)
import Data.Maybe
import Data.Data (Data, Typeable)
import qualified Data.Set as Set

data GenCard = GenCard { ctype     :: CardType
                       , gname     :: Name
                       , gset      :: CSet
                       , gnum      :: Number
                       , grar      :: Rarity
                       , gkeywords :: Keywords
                       , mcolor    :: Maybe Color
                       , mcost     :: Maybe Cost
                       , mreq      :: Maybe Req
                       , mpower    :: Maybe Power
                       , mboosted  :: Maybe Power
                       , mpoints   :: Maybe Points
                       , mpreqs    :: Maybe ProblemReq
                       , gtext     :: Text
                       } deriving (Eq, Ord, Data, Typeable)

toGeneric :: Card -> GenCard
toGeneric c =
    let gset = set c
        gnum = num c
        gtext = text c
        gname = name c
        grar = rar c
        gkeywords = keywords c
    in case c of
        (Mane{..}) -> let ctype = TMane
                          mcolor = Just color
                          mcost = Nothing
                          mpower = Just power
                          mreq = Nothing
                          mboosted = Just boosted
                          mpoints = Nothing
                          mpreqs = Nothing
                      in GenCard {..}
        (Friend{..}) -> let ctype = TFriend
                            mcolor = Just color
                            mcost = Just cost
                            mreq = Just req
                            mpower = Just power
                            mboosted = Nothing
                            mpoints = Nothing
                            mpreqs = Nothing
                        in GenCard {..}
        (Event{..}) -> let ctype = TEvent
                           mcolor = Just color
                           mcost = Just cost
                           mreq = Just req
                           mpower = Just power
                           mboosted = Nothing
                           mpoints = Nothing
                           mpreqs = Nothing
                       in GenCard {..}
        (Resource{..}) -> let ctype = TResource
                              mcolor = Just color
                              mcost = Just cost
                              mreq = Just req
                              mpower = Just power
                              mboosted = Nothing
                              mpoints = Nothing
                              mpreqs = Nothing
                          in GenCard {..}
        (Troublemaker{..}) -> let ctype = TTroublemaker
                                  mcolor = Nothing
                                  mcost = Nothing
                                  mreq = Nothing
                                  mpower = Just power
                                  mboosted = Nothing
                                  mpoints = Just points
                                  mpreqs = Nothing
                              in GenCard {..}
        (Problem{..}) -> let ctype = TProblem
                             mcolor = Nothing
                             mcost = Nothing
                             mreq = Nothing
                             mpower = Nothing
                             mboosted = Nothing
                             mpoints = Just points
                             mpreqs = Just preqs
                         in GenCard {..}

fromGeneric :: GenCard -> Card
fromGeneric c@GenCard{..} =
    let set = gset
        rar = grar
        name = gname
        num = gnum
        text = gtext
        keywords = gkeywords
    in case ctype of
    TMane -> let color = fromJust mcolor
                 power = fromJust mpower
                 boosted = fromJust mboosted
             in Mane {..}
    TFriend -> let color = fromJust mcolor
                   cost = fromJust mcost
                   req = fromJust mreq
                   power = fromJust mpower
               in Friend {..}
    TEvent -> let color = fromJust mcolor
                  cost = fromJust mcost
                  req = fromJust mreq
                  power = fromJust mpower
              in Event {..}
    TResource -> let color = fromJust mcolor
                     cost = fromJust mcost
                     req = fromJust mreq
                     power = fromJust mpower
                 in Resource {..}
    TTroublemaker -> let power = fromJust mpower
                         points = fromJust mpoints
                     in Troublemaker {..}
    TProblem -> let points = fromJust mpoints
                    preqs = fromJust mpreqs
                in Problem {..}
