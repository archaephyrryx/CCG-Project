{-# LANGUAGE RecordWildCards, DoRec #-} 
module App.Deck where
-------------------------------------------------
import Prelude hiding (div)
import Data.Maybe
import Data.List.Split
import Data.List
import Data.IORef
--------------------------------------------------
import Database
import Data.IxSet
--------------------------------------------------
import Deck
import Cards
import Cards.Generic
import Cards.Common
import Cards.Differentiation
import MLPCCG
-------------------------------------------------
import App.Core
import App.Core.Helper
import App.Filtering
import App.Widgets
import App.Core.Modes
import App.Core.AppData
import App.Renderer.Deck
--------------------------------------------------
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Elements hiding (map)
import Graphics.UI.Threepenny.Core
---------------------------------------------------

appletDeck :: Applet
appletDeck = Applet DeckBuilder initDeckState deckRun

initDeckState :: UI AppState
initDeckState = do
    rec selectTyp <- multiSelect "Type" False (pure   typeValues) bTypSelect (pure (string . show))
        bTypSelect <- stepper [] . rumors $ userSelections selectTyp
    rec selectCol <- multiSelect "Color" False (pure  colorValues) bColSelect (pure (string . show))
        bColSelect <- stepper [] . rumors $ userSelections selectCol
    rec selectRar <- multiSelect "Rarity" False (pure rarityValues) bRarSelect (pure (string . show))
        bRarSelect <- stepper [] . rumors $ userSelections selectRar
    rec selectSet <- multiSelect "Set" False (pure    setValues) bSetSelect (pure (string . show))
        bSetSelect <- stepper [] . rumors $ userSelections selectSet

    prevPage <- UI.button #. "page-btn" # settext "<"
    pageNo   <- UI.span   #. "page-num" # settext "1"
    nextPage <- UI.button #. "page-btn" # settext ">"

    let qfilter = blankDeckFilter
        deck = emptyDeck
        view = (GridView 10 5 0)
        appMode = DeckBuilderMode qfilter view deck
        appElems = DBEL{..}
        appRules = DBBL{..}
    return AppState{..}

updateS :: Filter -> Maybe GenCard -> AppState -> AppState
updateS nf ac as@AppState{appMode=(DeckBuilderMode _ v d), ..} =
    let appMode = DeckBuilderMode nf v (((addCard.fromGeneric)?ac)$d)
    in AppState{..}

deckRun :: Window -> SigStateT
deckRun window (sig, stat) = do
    iSig <- liftIO $ newIORef sig
    iMatches <- liftIO $ newIORef cardDB
    iAddCard <- liftIO $ newIORef Nothing
    content <- head <$> getElementsByClassName window "main-content"
    nav <- head <$> getElementsByClassName window "nav"
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
            madCard <- liftIO $ readIORef iAddCard
            let stat' = updateS newFilter madCard stat
            return (Change Rerender, stat')
        
        rerender :: AppState -> UI (AppSignal, AppState)
        rerender st = do
            liftIO $ writeIORef iAddCard (Nothing)
            let (DeckBuilderMode _ g@GridView{..} deck) = appMode st
            matches <- toList <$> liftIO (readIORef iMatches)
            element content # UI.set children [] #+ (render g matches)
            return (Continue, st)

        initialize :: AppState -> UI (AppSignal, AppState)
        initialize st = do let a@DBEL{..} = appElems st
                               (DeckBuilderMode _ _ deck) = appMode st
                           layout <- mkLayout [] a deck
                           on UI.click prevPage $ \_ -> liftIO $ writeIORef iSig (Change Prev)
                           on UI.click nextPage $ \_ -> liftIO $ writeIORef iSig (Change Next)
                           getBody window # UI.set children [layout]
                           firstPage =<< snd <$> rerender st

        mkLayout :: [Element] -> AEL -> Deck -> UI Element
        mkLayout xs ael d = UI.div #. "page" #+ [UI.div #. "nav-row" #+ [element nav], UI.div #. "content" #+ outlay ael d]


        outlay :: AEL -> Deck -> [UI Element]
        outlay a@DBEL{..} d = [ UI.div #. "add-cards" #+ [
                              UI.div #. "card-filter" #+ [
                                UI.div #. "select-filter" #+ [
                                  UI.div #. "type-select" #+ [element selectTyp]
                                , UI.div #. "color-select" #+  [element selectCol]
                                , UI.div #. "rarity-select" #+ [element selectRar]
                                , UI.div #. "set-select" #+  [element selectSet]
                                ]
                              ]
                            , UI.div #. "filtered-cards" #+ [element content]
                            ]
                          , UI.div #. "deck-sidebar" #+ [construct d]
                          ]

        firstPage :: AppState -> UI (AppSignal, AppState)
        firstPage st@AppState{..} = do
            matches <- liftIO (readIORef iMatches)
            let DeckBuilderMode f l@GridView{..} d = appMode
                a@DBEL{..} = appElems
                l' = setpage 0 l
                ub = Data.IxSet.size matches
                mpn = ub`cdiv`(npp l)
            prevPage <- element prevPage # UI.set UI.enabled False
            pageNo <- element pageNo # settext "1"
            nextPage <- element nextPage # UI.set UI.enabled (mpn > 1)
            let appMode = DeckBuilderMode f l' d
                appElems = DBEL{..}
                st' = AppState{..}
            return (Change Rerender, st')

        goNextPage :: AppState -> UI (AppSignal, AppState)
        goNextPage st@AppState{..} = do
            matches <- liftIO (readIORef iMatches)
            let DeckBuilderMode f l@GridView{..} d = appMode
                ub = Data.IxSet.size matches
                a@DBEL{..} = appElems
                mpn = ub`cdiv`(npp l)
            if (pn < mpn)
              then do
                let l' = incpage l
                prevPage <- element prevPage # UI.set UI.enabled True
                pageNo <- element pageNo # settext (show (pn + 2))
                nextPage <- element nextPage # UI.set UI.enabled ((pn+1) < mpn)
                let appMode = DeckBuilderMode f l' d
                    appElems = DBEL{..}
                    st' = AppState{..}
                return (Change Rerender, st')
              else do
                return (Continue, st)

        goPrevPage :: AppState -> UI (AppSignal, AppState)
        goPrevPage st@AppState{..} = do 
            let DeckBuilderMode f l@GridView{..} d = appMode
                a@DBEL{..} = appElems
            if (pn > 0)
              then do
                let l' = decpage l
                nextPage <- element nextPage # UI.set UI.enabled True
                pageNo <- element pageNo # settext (show pn)
                prevPage <- element prevPage # UI.set UI.enabled (pn > 1)
                let appMode = DeckBuilderMode f l' d
                    appElems = DBEL{..}
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
