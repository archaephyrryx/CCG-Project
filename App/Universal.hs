{-# LANGUAGE RecordWildCards, RecursiveDo #-}

module App.Universal where

import CCG
import Util
--------------------------
import Control.Applicative
import Control.Monad
import Data.List
-----------------------------
import qualified Graphics.UI.Threepenny          as UI
import qualified Graphics.UI.Threepenny.Core     as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import Graphics.UI.Threepenny.Core hiding (get, set)
-----------------------------
import App.Core
import App.Widgets
-----------------------------

powerless :: Maybe Power
powerless = Nothing

priceless :: Maybe Cost
priceless = Nothing

boundless :: Maybe Req
boundless = Nothing

selectAll :: [UI Element] -> UI Element
selectAll xs = row (map (\x -> column [ x ]) xs)

-- | Applet multi/mono select
data AMS = AMS { uSelectTyp :: MultiSelect CardType
               , oSelectTyp :: MonoSelect  CardType
               , uSelectCol :: MultiSelect Color
               , oSelectCol :: MonoSelect  Color
               , uSelectSet :: MultiSelect CSet
               , oSelectSet :: MonoSelect  CSet
               , uSelectRar :: MultiSelect Rarity
               , oSelectRar :: MonoSelect  Rarity
               , clearsTyp  :: Element
               , clearsCol  :: Element
               , clearsSet  :: Element
               , clearsRar  :: Element
               , minPow     :: Min Power
               , maxPow     :: Max Power
               , minCost    :: Min Cost
               , maxCost    :: Max Cost
               , minReq     :: Min Req
               , maxReq     :: Max Req
               }

selectors :: Behavior Bool -> Event AppMode -> UI (UBL, UBL, AMS)
selectors bMulti eModeChange = do
    let bColorValues = pure colorValues
        bSetValues = pure setValues
        bRarityValues = pure rarityValues
        bTypeValues = pure typeValues

    rec 
        (uSelectTyp, clearsTyp) <- multiSelect   bTypeValues   bTypSelect (plss)
        (uSelectCol, clearsCol) <- multiSelect   bColorValues  bColSelect (plss)
        (uSelectSet, clearsSet) <- multiSelect   bSetValues    bSetSelect (plss)
        (uSelectRar, clearsRar) <- multiSelect   bRarityValues bRarSelect (plss)
        (oSelectTyp, _) <- monoSelectVDC bTypeValues   bTypSelect pss (istring "Type")
        (oSelectCol, _) <- monoSelectVDC bColorValues  bColSelect pss (istring "Color")
        (oSelectSet, _) <- monoSelectVDC bSetValues    bSetSelect pss (istring "Set")
        (oSelectRar, _) <- monoSelectVDC bRarityValues bRarSelect pss (istring "Rarity")

        let
            rus = rumors . userSelections
            steph :: [Event [a]] -> UI (Behavior [a])
            steph es = stepper [] $ head <$> unions es
            

        buTypSelect <- steph [rus uSelectTyp, [] <$ UI.click clearsTyp]
        boTypSelect <- steph [rus oSelectTyp, [] <$ UI.click clearsTyp]

        boColSelect <- steph [rus oSelectCol, [] <$ UI.click clearsCol]
        buColSelect <- steph [rus uSelectCol, [] <$ UI.click clearsCol]

        buSetSelect <- steph [rus uSelectSet, [] <$ UI.click clearsSet]
        boSetSelect <- steph [rus oSelectSet, [] <$ UI.click clearsSet]

        buRarSelect <- steph [rus uSelectRar, [] <$ UI.click clearsRar]
        boRarSelect <- steph [rus oSelectRar, [] <$ UI.click clearsRar]

        let 
            bTypSelect = if_ <$> bMulti <*> buTypSelect <*> boTypSelect
            bColSelect = if_ <$> bMulti <*> buColSelect <*> boColSelect
            bSetSelect = if_ <$> bMulti <*> buSetSelect <*> boSetSelect
            bRarSelect = if_ <$> bMulti <*> buRarSelect <*> boRarSelect

    rec
        (minPow, maxPow) <- minmax bPowMin bPowMax (pure (show.val))
        (minCost, maxCost) <- minmax bCostMin bCostMax (pure (show.val))
        (minReq, maxReq) <- minmax bReqMin  bReqMax (pure (show.val))

        let
            stepr :: (Maybe a) -> (s0 -> Tidings (Maybe a)) -> s0 -> UI (Behavior (Maybe a))
            stepr n f x = stepper n $ head <$> unions [ rumors . f $ x, Nothing <$ eModeChange ]
        bPowMin  <- stepr powerless userMin minPow
        bPowMax  <- stepr powerless userMax maxPow
        bCostMin <- stepr priceless userMin minCost
        bCostMax <- stepr priceless userMax maxCost
        bReqMin  <- stepr boundless userMin minReq
        bReqMax  <- stepr boundless userMax maxReq
    return (FCBL{..}, DBBL{..}, AMS{..})
