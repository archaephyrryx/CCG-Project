{-# LANGUAGE RecordWildCards, DoRec #-}

module App.FilterCard where

import Cards
import Cards.Common
import Cards.Common.Abbrev
import Cards.Common.Color
import Cards.Common.Hint
import Cards.Common.Stringe
import Cards.Common.Values
import Cards.Differentiation
import Cards.Generic
--------------------------
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Database
import Data.Data (Data, Typeable)
import Data.IORef
import Data.IxSet hiding (null)
import qualified Data.IxSet as IxSet
import TagState
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
-----------------------------
import qualified Graphics.UI.Threepenny          as UI
import qualified Graphics.UI.Threepenny.Core     as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import Graphics.UI.Threepenny.Core hiding (get, set)
-----------------------------
import App.Core
import App.Core.Modes
import App.Core.AppData
import App.Core.Helper
import App.Widgets
import App.Filtering
-----------------------------
import App.Renderer.FilterCard
-----------------------------

powerless :: Maybe Power
powerless = Nothing

priceless :: Maybe Cost
priceless = Nothing

boundless :: Maybe Req
boundless = Nothing

appletCards :: Applet
appletCards = Applet FilterCard initCardState cardRun

initCardState :: UI AppState
initCardState = do
    rec selectTyp <- multiSelect "Type" True (pure   typeValues) bTypSelect (pure (string . show))
        bTypSelect <- stepper [] . rumors $ userSelections selectTyp
    rec selectCol <- multiSelect "Color" True (pure  colorValues) bColSelect (pure (string . show))
        bColSelect <- stepper [] . rumors $ userSelections selectCol
    rec selectRar <- multiSelect "Rarity" True (pure rarityValues) bRarSelect (pure (string . show))
        bRarSelect <- stepper [] . rumors $ userSelections selectRar
    rec selectSet <- multiSelect "Set" True (pure    setValues) bSetSelect (pure (string . show))
        bSetSelect <- stepper [] . rumors $ userSelections selectSet
    rec (minPow,   maxPow) <- minmax bPowMin  bPowMax  (pure (show . val))
        bPowMin  <- stepper powerless . rumors $ userMin minPow
        bPowMax  <- stepper powerless . rumors $ userMax maxPow
    rec (minCost, maxCost) <- minmax bCostMin bCostMax (pure (show . val))
        bCostMin <- stepper priceless . rumors $ userMin minCost
        bCostMax <- stepper priceless . rumors $ userMax maxCost
    rec (minReq, maxReq)   <- minmax bReqMin  bReqMax  (pure (show . val))
        bReqMin  <- stepper boundless . rumors $ userMin minReq
        bReqMax  <- stepper boundless . rumors $ userMax maxReq

    prevPage <- UI.button #. "page-btn" # settext "<"
    pageNo   <- UI.span   #. "page-num" # settext "1"
    nextPage <- UI.button #. "page-btn" # settext ">"

    let qfilter = blankCardFilter
        cardView = ListView 100 0
        appMode = FilterCardMode qfilter cardView
        appElems = FCEL{..}
        appRules = FCBL{..}
    return AppState{..}


updateS :: Filter -> AppState -> AppState
updateS nf as@AppState{appMode=(FilterCardMode _ v), ..} =
    let appMode = FilterCardMode nf v
    in AppState{..}



cardRun :: Window -> SigStateT
cardRun window (sig, stat) = do
    iSig <- liftIO $ newIORef sig
    iMatches <- liftIO $ newIORef cardDB
    content <- head <$> getElementsByClassName window "main-content"
    nav <- head <$> getElementsByClassName window "nav"
    dbg <- head <$> getElementsByClassName window "debugger"
    let
        sigHandle :: SigStateT
        sigHandle (g,t) = case g of 
            Continue -> return (g,t)
            Change Initialize -> initialize t
            Change Rerender -> rerender t
            Change Update -> update t
            Change Next -> goNextPage t
            Change Prev -> goPrevPage t

        update :: AppState -> UI (AppSignal, AppState)
        update stat@AppState{..} = do
            newFilter <- filterFromElem appElems
            let newMatches = applyFilter newFilter
            liftIO $ writeIORef iMatches (newMatches)
            return (Change Rerender, (updateS newFilter $ stat))

        pronounce :: GenCard -> UI Element
        pronounce g = do
            let setCardMode = \g -> liftIO $ writeIORef iSig (Switch (ShowCardMode g))
            sl <- softLink (gname g) g
            sl`linksTo`setCardMode
            --on UI.click (getElement sl) $ setCardMode g
            element sl
        
        rerender :: AppState -> UI (AppSignal, AppState)
        rerender st = do
            let (FilterCardMode _ (ListView npp pn)) = appMode st
            matches <- (take npp . drop (npp*pn) . toList) <$> liftIO (readIORef iMatches)
            element content # UI.set children [] #+ (render matches pronounce)
            d <- debug st
            element dbg # UI.set children d
            return (Continue, st)

        initialize :: AppState -> UI (AppSignal, AppState)
        initialize st = do let a@FCEL{..} = appElems st
                           dbg <- debug st
                           layout <- mkLayout dbg a
                           on UI.click prevPage $ \_ -> liftIO $ writeIORef iSig (Change Prev)
                           on UI.click nextPage $ \_ -> liftIO $ writeIORef iSig (Change Next)
                           getBody window # UI.set children [layout]
                           firstPage =<< snd <$> rerender st

        mkLayout :: [Element] -> AEL -> UI Element
        mkLayout xs ael = UI.div #. "page" #+ [UI.div #. "nav-row" #+ [element nav], UI.div #. "content" #+ outlay ael, UI.div #. "debug" #+ [element dbg]]

        mkLine :: [Element] -> [UI Element]
        mkLine xs = map element xs

        outlay :: AEL -> [UI Element]
        outlay a@FCEL{..} =
                [ UI.div #. "card-filter" #+ [
                     UI.div #. "min-max" #+ [
                         UI.div #. "power-min-max" #+ [element minPow , UI.bold #+ [string "to"], element maxPow ]
                       , UI.div #. "cost-min-max"  #+ [element minCost, UI.bold #+ [string "to"], element maxCost]
                       , UI.div #. "req-min-max"   #+ [element minReq , UI.bold #+ [string "to"], element maxReq ]
                       ]
                     , UI.div #. "select-filter" #+ [
                         UI.div #. "type-select" #+ [element selectTyp]
                       , UI.div #. "color-select" #+  [element selectCol]
                       , UI.div #. "rarity-select" #+ [element selectRar]
                       , UI.div #. "set-select" #+  [element selectSet]
                       ]
                   ]
                   , UI.div #. "filtered-cards" #+ [element content]
                   , UI.div #. "page-nav" #+ [element prevPage, element pageNo, element nextPage]
                 ]

        debug :: AppState -> UI [Element]
        debug st@AppState{..} = do
            let bs@FCBL{..} = appRules
                f x = show <$> currentValue x
            mapM (\x -> liftIO x >>= string) ((show <$> readIORef iSig):[f bColSelect,f bRarSelect,f bSetSelect,f bPowMin,f bPowMax,f bCostMin,f bCostMax,f bReqMin,f bReqMax])
 

        firstPage :: AppState -> UI (AppSignal, AppState)
        firstPage st@AppState{..} = do
            matches <- liftIO $ readIORef iMatches
            let FilterCardMode f l@ListView{..} = appMode
                a@FCEL{..} = appElems
                l' = setpage 0 l
                ub = Data.IxSet.size matches  
                mpn = ub`cdiv`(npp l)
            prevPage <- element prevPage # UI.set UI.enabled False
            pageNo <- element pageNo # settext "1"
            nextPage <- element nextPage # UI.set UI.enabled (mpn > 1)
            let appMode = FilterCardMode f l'
                appElems = FCEL{..}
                st' = AppState{..}
            return (Change Rerender, st')

        goNextPage :: AppState -> UI (AppSignal, AppState)
        goNextPage st@AppState{..} = do
            matches <- liftIO $ readIORef iMatches
            let FilterCardMode f l@ListView{..} = appMode
                ub = Data.IxSet.size matches  
                a@FCEL{..} = appElems
                mpn = ub`cdiv`(npp l)
            if (pn < mpn)
              then do
                let l' = incpage l
                prevPage <- element prevPage # UI.set UI.enabled True
                pageNo <- element pageNo # settext (show (pn + 2))
                nextPage <- element nextPage # UI.set UI.enabled ((pn+1) < mpn)
                let appMode = FilterCardMode f l'
                    appElems = FCEL{..}
                    st' = AppState{..}
                return (Change Rerender, st')
              else do
                return (Continue, st)

        goPrevPage :: AppState -> UI (AppSignal, AppState)
        goPrevPage st@AppState{..} = do 
            let FilterCardMode f l@ListView{..} = appMode
                a@FCEL{..} = appElems
            if (pn > 0)
              then do
                let l' = decpage l
                nextPage <- element nextPage # UI.set UI.enabled True
                pageNo <- element pageNo # settext (show pn)
                prevPage <- element prevPage # UI.set UI.enabled (pn > 1)
                let appMode = FilterCardMode f l'
                    appElems = FCEL{..}
                    st' = AppState{..}
                return (Change Rerender, st')
              else do
                return (Continue, st)

        persist :: AppState -> UI (AppSignal, AppState)
        persist stat = do
            sig <- liftIO $ readIORef iSig
            if (remain sig)
              then do
                (sig',stat') <- sigHandle (sig,stat)
                sig'' <- liftIO $ readIORef iSig
                if (sig'' == sig)
                  then do
                    liftIO $ writeIORef iSig (sig')
                    persist stat'
                  else do
                    persist stat'
              else
                return (sig, stat)

    persist stat
