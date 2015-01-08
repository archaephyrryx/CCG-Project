{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, NamedFieldPuns, DeriveDataTypeable #-}

module Cards.Generic where

import Cards
import Cards.Common
import Cards.Pretty
import Data.List
import Data.Char
import Data.Set (Set)
import Data.Function (on)
import Data.Maybe
import Data.Data (Typeable)
import qualified Data.Set as Set

data GenCard = GenCard { ctype    :: CardType
                       , name     :: Name
                       , set      :: CSet
                       , num      :: Number
                       , rar      :: Rarity
                       , keywords :: Keywords
                       , mcolor    :: Maybe Color
                       , mcost     :: Maybe Cost
                       , mreq      :: Maybe Req
                       , mpower    :: Maybe Power
                       , mboosted  :: Maybe Power
                       , mpoints   :: Maybe Points
                       , mpreqs    :: Maybe ProblemReq
                       , text     :: Text
                       } deriving (Eq, Ord, Typeable)

howshow = fst (show, prettyShow)

genset = setnum.fromGeneric

instance Show GenCard where
    show = howshow.fromGeneric

toGeneric :: Card -> GenCard
toGeneric c@Mane{..} = let ctype = TMane
                           mcolor = Just color
                           mcost = Nothing
                           mpower = Just power
                           mreq = Nothing
                           mboosted = Just boosted
                           mpoints = Nothing
                           mpreqs = Nothing
                       in GenCard {..}
toGeneric c@Friend{..} = let ctype = TFriend
                             mcolor = Just color
                             mcost = Just cost
                             mreq = Just req
                             mpower = Just power
                             mboosted = Nothing
                             mpoints = Nothing
                             mpreqs = Nothing
                          in GenCard {..}
toGeneric c@Event{..} = let ctype = TEvent
                            mcolor = Just color
                            mcost = Just cost
                            mreq = Just req
                            mpower = Just power
                            mboosted = Nothing
                            mpoints = Nothing
                            mpreqs = Nothing
                        in GenCard {..}
toGeneric c@Resource{..} = let ctype = TResource
                               mcolor = Just color
                               mcost = Just cost
                               mreq = Just req
                               mpower = Just power
                               mboosted = Nothing
                               mpoints = Nothing
                               mpreqs = Nothing
                           in GenCard {..}
toGeneric c@Troublemaker{..} = let ctype = TTroublemaker
                                   mcolor = Nothing
                                   mcost = Nothing
                                   mreq = Nothing
                                   mpower = Just power
                                   mboosted = Nothing
                                   mpoints = Just points
                                   mpreqs = Nothing
                               in GenCard {..}
toGeneric c@Problem{..} = let ctype = TProblem
                              mcolor = Nothing
                              mcost = Nothing
                              mreq = Nothing
                              mpower = Nothing
                              mboosted = Nothing
                              mpoints = Just points
                              mpreqs = Just preqs
                          in GenCard {..}

fromGeneric :: GenCard -> Card
fromGeneric c@GenCard{..} = case ctype of
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
