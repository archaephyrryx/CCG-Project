{-# LANGUAGE RecordWildCards, OverloadedStrings, DoRec #-}
module App where
------------------------------
import Cards
import Cards.Common 
import Cards.Common.Abbrev
import Cards.Common.Color
import Cards.Common.Hint
import Cards.Common.Stringe
import Cards.Common.Values
import Cards.Differentiation
import Cards.Generic
------------------------------
import Control.Applicative
import Control.Monad
------------------------------
import Data.Data (Data, Typeable)
import Data.IORef
import Data.IxSet hiding (null)
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
------------------------------
import Database
import TagState
------------------------------
import App.FilterCard
import App.SingleCard
import App.Core
import App.Core.Helper
import App.Core.Modes
import App.Deck
import App.Filtering
import App.Widgets
------------------------------
import qualified Graphics.UI.Threepenny          as UI
import qualified Graphics.UI.Threepenny.Core     as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Elements (addStyleSheet)

appname = "ThreePony"
appdesc = "An Advanced MLP:CCG Toolkit"
appfname = (appname ++ ": " ++ appdesc)

setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title appfname
    addStyleSheet window "style.css"
    appmode <- liftIO $ newIORef Home

    butHome <- softLink "Home" Home
    butCFil <- softLink "Cards" FilterCard
    butCDec <- softLink "Deck Builder" DeckBuilder

    butHome`linksTo`(liftIO . writeIORef appmode)
    butCFil`linksTo`(liftIO . writeIORef appmode)
    butCDec`linksTo`(liftIO . writeIORef appmode)

    content <- UI.div #. "main-content"
    debug <- UI.div #. "debugger"

    rec elSearch <- searchBar (pure cardDB) (bFilterString) (pure (string . show . gname))
        bFilterString <- stepper "" . rumors $ userSearch elSearch

    nav <- UI.li #. "nav" #+ [element butHome, element butCFil, element butCDec, element elSearch]

    let
        redoLayout :: UI ()
        redoLayout = void $ do
            layout <- mkLayout []
            getBody window # UI.set children [layout]

        mkLayout :: [Element] -> UI Element
        mkLayout xs = UI.div #. "page" #+ [UI.div #. "nav-row" #+ [element nav], UI.div #. "content" #+ [element content], UI.div #. "debug" #+ [element debug]]

    redoLayout
    let fc@Applet{..} = appletCards
    i <- initState
    appRun window (Change Initialize, i)

{-
setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title appfname
    addStyleSheet window "style.css"
    appmode <- liftIO $ newIORef HomeMode

    butHome <- UI.button #. "nav-btn" # settext "Home"
    butCFil <- UI.button #. "nav-btn" # settext "Cards"
    butCDec <- UI.button #. "nav-btn" # settext "Deck Builder"
    content <- UI.div

    rec elSearch <- searchBar (pure cardDB) (bFilterString) (pure (string . show . gname))
        bFilterString <- stepper "" . rumors $ userSearch elSearch


    nav <- UI.li #. "nav" #+ [element butHome, element butCFil, element butCDec, element elSearch]
    -- FilterCard and DeckBuilder only
    rec selectTyp <- multiSelect (pure   typeValues) bTypSelect (pure (string . show))
        bTypSelect <- stepper [] . rumors $ userSelections selectTyp
    rec selectCol <- multiSelect (pure  colorValues) bColSelect (pure (string . show))
        bColSelect <- stepper [] . rumors $ userSelections selectCol
    rec selectRar <- multiSelect (pure rarityValues) bRarSelect (pure (string . show))
        bRarSelect <- stepper [] . rumors $ userSelections selectRar
    rec selectSet <- multiSelect (pure    setValues) bSetSelect (pure (string . show))
        bSetSelect <- stepper [] . rumors $ userSelections selectSet

    clearsTyp <- UI.button #. "clear-btn" # settext "clear"
    clearsCol <- UI.button #. "clear-btn" # settext "clear"
    clearsRar <- UI.button #. "clear-btn" # settext "clear"
    clearsSet <- UI.button #. "clear-btn" # settext "clear"

    cdFilter  <- liftIO $ newIORef (blankCardFilter)
    cdMatches <- liftIO $ newIORef (Data.IxSet.empty)

    -- FilterCard only
    rec (minPow,   maxPow) <- minmax bPowMin  bPowMax  (pure (show . val))
        bPowMin  <- stepper powerless . rumors $ userMin minPow
        bPowMax  <- stepper powerless . rumors $ userMax maxPow 
    rec (minCost, maxCost) <- minmax bCostMin bCostMax (pure (show . val))
        bCostMin <- stepper priceless . rumors $ userMin minCost
        bCostMax <- stepper priceless . rumors $ userMax maxCost
    rec (minReq, maxReq)   <- minmax bReqMin  bReqMax  (pure (show . val))
        bReqMin  <- stepper boundless . rumors $ userMin minReq
        bReqMax  <- stepper boundless . rumors $ userMax maxReq

    cardView <- liftIO $ newIORef (ListView 100 0)

    prevPage <- UI.button #. "page-btn" # settext "<"
    pageNo   <- UI.span   #. "page-num" # settext "1"
    nextPage <- UI.button #. "page-btn" # settext ">"

    -- DeckBuilder Only
    
    deck <- liftIO $ newIORef []

    -- ShowCard/CardFilter

    crender <- liftIO $ newIORef (Map.empty)

        
    -- Tagging Only
    elPlus    <- UI.button # settext "+"
    elMinu    <- UI.button # settext "-"
    elSave    <- UI.button # settext "Save Changes"
    elDrop    <- UI.button # settext "Discard Changes"
    elMode    <- UI.button # settext "View"
    elBlank   <- UI.input  # settext  ""
    result    <- UI.span

    inputs <- liftIO $ newIORef []
    mode   <- liftIO $ newIORef View
    edited <- liftIO $ newIORef False
    dats   <- liftIO $ newIORef example

    -- events and behaviors
    
    -- GUI Layout
    let
        refilter :: UI ()
        refilter = void $ do
            updateFilter
            updateCardMatches
            redoLayout

        displayAll :: UI ()
        displayAll = void $ do
            amode <- liftIO (readIORef appmode)
            case amode of
                HomeMode -> do displayHome
                FilterCardMode f -> do liftIO $ modifyIORef cdFilter (const f)
                                       updateFilter
                                       displayFilterCard
                ShowCardMode g ->  do displayCard
                DeckBuilderMode f cs -> do liftIO $ modifyIORef cdFilter (const f)
                                           updateFilter
                                           displayDeckBuilder
        
        displayHome :: UI ()
        displayHome = void $ do
            element content # UI.set children []
            element content #+ [
                row [UI.h1 #+ [string appname]]
              , row [UI.p #+ [UI.string ("Welcome to "++appname++", an in-development project for the MLP:CCG")]]
              , row [UI.p #+ [UI.string ("This site was inspired by, and modelled on, ")
                        ,UI.a # UI.set UI.href "http://ponyhead.com" # UI.set UI.text "PonyHead"
                        ]]
              , row [UI.p #+ [UI.string ("This is merely one branch of a wider project. Be sure to check out the others.")]
              ]]

        displayFilterCard :: UI ()
        displayFilterCard = void $ do
            decree
            tabulate

        displayCard :: UI ()
        displayCard = void $ do
            amode <- liftIO (readIORef appmode)
            when (modes amode == ShowCard) $ void $ do
                let (ShowCardMode g) = amode
                element content # UI.set children []
                element content #+ [renderCard g]

        displayDeckBuilder :: UI ()
        displayDeckBuilder = void $ do
            builddeck

        builddeck :: UI ()
        builddeck = void $ do
            element content # UI.set children []
            element content #+ [string "Deck builder not written yet."]

        updateFilter :: UI ()
        updateFilter = void $ do
            amode <- liftIO (readIORef appmode)
            case amode of
                FilterCardMode _ -> updateCardFilter
                DeckBuilderMode _ _ -> updateDeckFilter
                _ -> return ()

        updateDeckFilter :: UI ()
        updateDeckFilter = void $ do
            ftyp <- map   (typeValues!!) <$> get selections (getElement selectTyp)
            fcol <- map  (colorValues!!) <$> get selections (getElement selectCol)
            frar <- map (rarityValues!!) <$> get selections (getElement selectRar)
            fset <- map    (setValues!!) <$> get selections (getElement selectSet)
            let newFilter = DeckFilter fcol fset ftyp frar
            liftIO $ modifyIORef cdFilter (const newFilter)
            updateCardMatches

        updateCardFilter :: UI ()
        updateCardFilter = void $ do
            pmin <- readMaybeH <$> get value (getElement minPow)
            pmax <- readMaybeH <$> get value (getElement maxPow)
            cmin <- readMaybeH <$> get value (getElement minCost) 
            cmax <- readMaybeH <$> get value (getElement maxCost)
            rmin <- readMaybeH <$> get value (getElement minReq)
            rmax <- readMaybeH <$> get value (getElement maxReq)
            ftyp <- map   (typeValues!!) <$> get selections (getElement selectTyp)
            fcol <- map  (colorValues!!) <$> get selections (getElement selectCol)
            frar <- map (rarityValues!!) <$> get selections (getElement selectRar)
            fset <- map    (setValues!!) <$> get selections (getElement selectSet)
            let newFilter = CardFilter pmin pmax cmin cmax rmin rmax fcol fset ftyp frar
            liftIO $ modifyIORef cdFilter (const newFilter)
            updateCardMatches

        updateCardMatches :: UI ()
        updateCardMatches = void $ do
            xfilter <- liftIO $ readIORef cdFilter
            liftIO $ modifyIORef cdMatches (const (applyFilter xfilter))

        pronounce :: GenCard -> UI Element
        pronounce g = do
            rends <- liftIO $ readIORef crender
            return (fromJust (Map.lookup g rends))
    
        decree :: UI ()
        decree = do
            amode <- liftIO (readIORef appmode)
            when (modes amode == FilterCard) $ do
                cview <- liftIO (readIORef cardView)
                let (ListView npp pn) = cview
                matches <- (take npp . drop (npp*pn) . toList) <$> liftIO (readIORef cdMatches)
                forM matches (\g -> softLink (gname g) g (setCardMode) >>= \sl -> liftIO $ modifyIORef crender (Map.insert g (getElement sl)) >> return (slHandle sl))
                return ()



        tabulate :: UI ()
        tabulate = void $ do
            cview <- liftIO (readIORef cardView)
            let (ListView npp pn) = cview
            matches <- (take npp . drop (npp*pn) . toList) <$> liftIO (readIORef cdMatches)
            let {
              theader =
                UI.tr #+ (map (\x -> UI.th #+ [string x]) ["#", "Rarity", "Type", "Cost", "Req.", "Name", "Power"]);
              trows =
                for matches (\g@GenCard{..} ->
                    UI.tr #+ (map (\x -> UI.td #+ [x]) $
                      [ UI.string $ genset g
                      , UI.string $ brief rar
                      , iconic ctype
                      , UI.string $ fromMaybe "" (show.val <$> mcost)
                      , reqtify g
                      , pronounce g
                      , empower g
                      ])
                    );
            }
            element content # UI.set children []
            element content #+ [ UI.table #+ (theader:trows) ]
            
    
        redoLayout :: UI ()
        redoLayout = void $ do
            amode <- liftIO (readIORef appmode)
            cview <- liftIO (readIORef cardView)
            layout <- mkLayout amode []
            getBody window # UI.set children [layout]
            displayAll

        mkLayout :: AppMode -> [Element] -> UI Element
        mkLayout m xs = UI.div #. "page" #+ [UI.div #. "nav-row" #+ [element nav], UI.div #. "content" #+ (outlay m)]

        outlay :: AppMode -> [UI Element]
        outlay mod = case mod of
            HomeMode -> [element content]
            FilterCardMode f -> outlayFC
            ShowCardMode g -> outlaySC
            DeckBuilderMode f d -> outlayDB d
          
        outlayFC :: [UI Element]
        outlayFC = [ UI.div #. "card-filter" #+ [
                       UI.div #. "min-max" #+ [
                         UI.div #. "power-min-max" #+ [element minPow, UI.bold #+ [string "to"], element maxPow]
                       , UI.div #. "cost-min-max" #+ [element minCost, UI.bold #+ [string "to"], element maxCost]
                       , UI.div #. "req-min-max" #+ [element minReq, UI.bold #+ [string "to"], element maxReq]
                       ]
                     , UI.div #. "select-filter" #+ [
                         UI.div #. "type-select" #+ [element selectTyp, element clearsTyp]
                       , UI.div #. "color-select" #+  [element selectCol, element clearsCol]
                       , UI.div #. "rarity-select" #+ [element selectRar, element clearsRar]
                       , UI.div #. "set-select" #+  [element selectSet, element clearsSet]
                       ]
                     ]
                   , UI.div #. "filtered-cards" #+ [element content]
                   , UI.div #. "page-nav" #+ [element prevPage, element pageNo, element nextPage]
                   ]

        outlaySC :: [UI Element]
        outlaySC = [] 
        outlayDB :: Deck -> [UI Element]
        outlayDB d = [ UI.div #. "add-cards" #+ [
                         UI.div #. "card-filter" #+ [
                           UI.div #. "select-filter" #+ [
                             UI.div #. "type-select" #+ [element selectTyp, element clearsTyp]
                           , UI.div #. "color-select" #+  [element selectCol, element clearsCol]
                           , UI.div #. "rarity-select" #+ [element selectRar, element clearsRar]
                           , UI.div #. "set-select" #+  [element selectSet, element clearsSet]
                           ]
                         ]
                         , UI.div #. "filtered-cards" #+ [element content]
                       ]
                       , UI.div #. "deck-sidebar" #+ [construct d]
                     ]

        mkLine :: Bool -> [Element] -> String -> [UI Element]
        mkLine e xs s = [row [UI.span # UI.set UI.text s]] ++ rebutrows
            where
                rebutrows = if e
                                then concatMap (\x -> [row x]) (reverse xs')
                                else [row [element result]]
                xs' = (ebut (head xs)):(map (\x -> [element x]) (tail xs))
                ebut y = (element y):(if e then [element elPlus, element elMinu] else [])

    -- Appmode switching
        setMode :: AppMode -> UI ()
        setMode am = do
            old <- liftIO $ readIORef appmode
            when ((modes old) /= (modes am)) $ do
                liftIO $ modifyIORef appmode (const am)
                redoLayout

        setModeHome = do
            setMode (HomeMode)
        setModeFilter = do
            filt <- liftIO $ readIORef cdFilter
            setMode (FilterCardMode filt)
        setCardMode g = do
            setMode (ShowCardMode g)
        setDeckMode = do
            filt <- liftIO $ readIORef cdFilter
            pdeck <- liftIO $ readIORef deck
            setMode (DeckBuilderMode filt pdeck)
            

    -- Page navigation
        firstPage :: UI ()
        firstPage = do
            cv <- liftIO $ readIORef cardView
            amode <- liftIO $ readIORef appmode
            when ((npp cv) > 0 && modes amode == FilterCard) $ do
                liftIO $ modifyIORef cardView (setpage 0)
                element pageNo # settext "1"
                element prevPage # UI.set UI.enabled False
                redoLayout

        goNextPage :: UI ()
        goNextPage = do
            cv@ListView{..} <- liftIO $ readIORef cardView
            amode <- liftIO $ readIORef appmode
            let ub = Data.IxSet.size cardDB  
            if (ub > (npp * (pn + 1)) && modes amode == FilterCard)
              then do
                liftIO $ modifyIORef cardView (incpage)
                element prevPage # UI.set UI.enabled True
                element pageNo # settext (show (pn + 2))
                redoLayout
              else do
                element nextPage # UI.set UI.enabled False
                redoLayout

        goPrevPage :: UI ()
        goPrevPage = do
            cv@ListView{..} <- liftIO $ readIORef cardView
            amode <- liftIO $ readIORef appmode
            if (pn > 0 && modes amode == FilterCard)
              then do
                liftIO $ modifyIORef cardView (incpage)
                element pageNo # settext (show (pn + 2))
                element nextPage # UI.set UI.enabled True
                redoLayout
              else do
                element prevPage # UI.set UI.enabled False
                redoLayout

    -- Filter clearing

        clearTyp :: UI ()
        clearTyp = do
            element selectTyp # UI.set selections []
            updateFilter >> updateCardMatches >> redoLayout

        clearSet :: UI ()
        clearSet = do
            element selectSet # UI.set selections []
            updateFilter >> updateCardMatches >> redoLayout

        clearRar :: UI ()
        clearRar = do
            element selectRar # UI.set selections []
            updateFilter >> updateCardMatches >> redoLayout

        clearCol :: UI ()
        clearCol = do
            element selectCol # UI.set selections []
            updateFilter >> updateCardMatches >> redoLayout

    -- Tagging-mode switching
        toggleMode :: UI ()
        toggleMode = do
            liftIO $ modifyIORef mode (toggle)

        toggler = void $ do
            emode <- liftIO (readIORef mode)
            element elMode # UI.set UI.text (show.toggle$emode)

    -- Save/Discard modifications

        saveChanges :: UI ()
        saveChanges = do
            tags <- mapM (get value) =<< liftIO (readIORef inputs)
            liftIO $ modifyIORef dats (\(y,x) -> (y, tags))

        discardChanges :: UI ()
        discardChanges = do
            liftIO $ modifyIORef inputs (const [])
            vdat <- liftIO (readIORef dats)
            mapM_ addInputVal (snd vdat)
            when (null (snd vdat)) (addInput)

    -- Add/Remove Fields
        addInputVal :: String -> UI ()
        addInputVal s = do
            elInput <- UI.input # UI.set value s
            on (domEvent "livechange") elInput $ \_ -> (liftIO $ modifyIORef edited (const True)) >> displayAll
            liftIO $ modifyIORef inputs (elInput:)

        addInput :: UI ()
        addInput = addInputVal ""

        dropInput :: UI ()
        dropInput = do
            elInput <- UI.input # UI.set value ""
            on (domEvent "livechange") elInput $ \_ -> (liftIO $ modifyIORef edited (const True)) >> displayAll
            liftIO $ modifyIORef inputs ((cond null (elInput:) id).(drop 1))

    -- Click behaviors
    on UI.click butHome $ const $ do
        setModeHome
    on UI.click butCFil $ const $ do
        setModeFilter
    on UI.click butCDec $ const $ do
        setDeckMode
    on UI.click prevPage $ const $ do
        goPrevPage
    on UI.click nextPage $ const $ do
        goNextPage
    on UI.click clearsTyp $ const $ do
        clearTyp
    on UI.click clearsCol $ const $ do
        clearCol
    on UI.click clearsRar $ const $ do
        clearRar
    on UI.click clearsSet $ const $ do
        clearSet
    on UI.click elPlus $ const $ do
        liftIO $ modifyIORef edited (const True)
        addInput
        redoLayout
    on UI.click elMinu $ const $ do
        liftIO $ modifyIORef edited (const True)
        dropInput
        redoLayout
    on UI.click elMode $ const $ do
        toggleMode
        toggler
        redoLayout
    on UI.click elSave $ const $ do
        saveChanges
        liftIO $ modifyIORef edited (const False)
        redoLayout
    on UI.click elDrop $ const $ do
        discardChanges
        liftIO $ modifyIORef edited (const False)
        redoLayout
    toggleMode >> toggler >> discardChanges >> redoLayout
    
    on (domEvent "livechange") (getElement minPow) $ \_ -> refilter
    on (domEvent "livechange") (getElement minCost) $ \_ -> refilter
    on (domEvent "livechange") (getElement minReq) $ \_ -> refilter
    on (domEvent "livechange") (getElement maxPow) $ \_ -> refilter
    on (domEvent "livechange") (getElement maxCost) $ \_ -> refilter
    on (domEvent "livechange") (getElement maxReq) $ \_ -> refilter
    on (domEvent "livechange") (getElement selectTyp) $ \_ -> refilter
    on (domEvent "livechange") (getElement selectRar) $ \_ -> refilter
    on (domEvent "livechange") (getElement selectCol) $ \_ -> refilter
    on (domEvent "livechange") (getElement selectSet) $ \_ -> refilter
    singles <- getElementsByClassName window "single-card"
    forM singles (\x -> UI.get value x >>= \code -> on UI.click x $ const $ setCardMode (fromJust.getOne$(cardDB @= (ravel code :: SetNum))))
    redoLayout
-}

-- conditional operator: performs function on value depending on test of value

example = ("Cat",[])

