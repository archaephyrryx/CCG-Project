{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeSynonymInstances, FlexibleInstances, DoRec #-} 

module App.SingleCard where
-------------------------------------------------
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Data (Data, Typeable)
import Data.IORef
import Data.IxSet
import Data.Map (Map)
import qualified Data.Map as Map
-------------------------------------------------
import Control.Applicative
import Control.Monad
--------------------------------------------------
import Database
import TagState
--------------------------------------------------
import Cards
import Cards.Common 
import Cards.Differentiation
import Cards.Generic
--------------------------------------------------
import App.MicroScope
import App.Core
import App.Core.AppData
import App.Core.Helper
import App.Core.Modes
-------------------------------------------------
import App.Renderer.SingleCard
--------------------------------------------------
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny          as UI
import qualified Graphics.UI.Threepenny.Core     as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import Graphics.UI.Threepenny.Core

appletSingleCard :: GenCard -> Applet
appletSingleCard g = Applet ShowCard (initSingleState g) singleRun

initSingleState :: GenCard -> UI AppState
initSingleState g = do
    {-
    rec let ePrev = UI.click prevCard
            eNext = UI.click nextCard
        prevCard <- liquidLink (unravel.gname) bPrev
        curCard <- liquidLink (unravel.gname) bCur
        nextCard <- liquidLink (unravel.gname) bNext
        bMicro <- accumB (reset (toSet cardDB) g) $ concatenate <$> unions
            [ goPrev <$ ePrev
            , goNext <$ eNext
            ]
        bPrev <- focus.goPrev <$> bMicro
    -}
    let bCur = pure g -- focus <$> bMicro
    {-
        pNext <- focus.goNext <$> bMicro
        element prevCard # sink UI.enabled (not . isFirst <$> bMicro)
        element nextCard # sink UI.enabled (not . isLast <$> bMicro)
    -}
    let appMode = ShowCardMode g
        appElems = SCEL
        appRules = SCBL{..}
    return AppState{..}


singleRun :: Window -> SigStateT
singleRun window (sig, stat) = do
    iSig <- liftIO $ newIORef sig
    content <- head <$> getElementsByClassName window "main-content"
    nav <- head <$> getElementsByClassName window "nav"
    let
        sigHandle :: SigStateT
        sigHandle (g,t) = case g of 
            Continue -> return (g,t)
            Change Initialize -> initialize t
            Change Rerender -> rerender t
            Change Update -> updateR t
            --Change Next -> goNextPage t
            --Change Prev -> goPrevPage t

        updateR :: AppState -> UI (AppSignal, AppState)
        updateR stat@AppState{..} = do
            return (Change Rerender, stat)
        
        rerender :: AppState -> UI (AppSignal, AppState)
        rerender st = do
            return (Continue, st)
            
        initialize :: AppState -> UI (AppSignal, AppState)
        initialize st = do let a@SCBL{..} = appRules st
                           layout <- mkLayout [] a
                           --prevCard`sinkTo`(on UI.click prevPage $ \_ -> liftIO $ writeIORef iSig (Change PrevPage)
                           --on UI.click nextPage $ \_ -> liftIO $ writeIORef iSig (Change NextPage)
                           getBody window # UI.set children [layout]
                           rerender st
                           {-firstPage =<< snd <$> rerender st -}

        mkLayout :: [Element] -> ABL -> UI Element
        mkLayout xs abl = do
            g <- currentValue {-getFlux-} (bCur abl)
            UI.div #. "page" #+ [UI.div #. "nav-row" #+ [element nav], UI.div #. "content" #+ outlay g]

        outlay :: GenCard -> [UI Element]
        outlay g {-a@SCEL{..}-} = 
                [{- UI.div #. "card-nav" #+ [element prevCard, element curCard, element nextCard]
                ,-} UI.div #. "card-display" #+ renderCard g 
                , UI.div #. "card-tags" #+ [cardTags g]
                ]
        
        cardTags :: GenCard -> UI Element
        cardTags g = UI.string ("This should show the tags for "++(gname g)++", but it doesn't yet.")


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

    -- GUI Layout
    let
        displayAll :: UI ()
        displayAll = void $ do
            amode <- liftIO (readIORef appmode)
            case amode of
                ShowCardMode g ->  do displayCard
        
        displayCard :: UI ()
        displayCard = void $ do
            amode <- liftIO (readIORef appmode)
            when (modes amode == ShowCard) $ void $ do
                let (ShowCardMode g) = amode
                element content # UI.set children []
                element content #+ [renderCard g]

        builddeck :: UI ()
        builddeck = void $ do
            element content # UI.set children []
            element content #+ [string "Deck builder not written yet."]

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
          
        outlaySC :: [UI Element]
        outlaySC = [] 

        mkLine :: Bool -> [Element] -> String -> [UI Element]
        mkLine e xs s = [row [UI.span # UI.set UI.text s]] ++ rebutrows
            where
                rebutrows = if e
                                then concatMap (\x -> [row x]) (reverse xs')
                                else [row [element result]]
                xs' = (ebut (head xs)):(map (\x -> [element x]) (tail xs))
                ebut y = (element y):(if e then [element elPlus, element elMinu] else [])

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
    
    redoLayout


setup :: Window -> UI ()
setup window = do
	-- GUI elements
	return window # set UI.title "Hello World!"

	elPlus	<- UI.button # set UI.text "+"
	elMinu	<- UI.button # set UI.text "-"
	elSave	<- UI.button # set UI.text "Save Changes"
	elDrop	<- UI.button # set UI.text "Discard Changes"
	elMode	<- UI.button # set UI.text "View"
	elBlank	<- UI.input  # set UI.text  ""
	result	<- UI.span

	inputs <- liftIO $ newIORef []
	mode   <- liftIO $ newIORef View
	edited <- liftIO $ newIORef False
	dats   <- liftIO $ newIORef example

	-- GUI Layout
	
	let key = fst example


	-- Layout and display
	let
		displayAll = void $ do
			xs <- mapM (get value) =<< liftIO (readIORef inputs)
			element result # set text (":\t"++((intercalate ", ").reverse $ xs))
		
		redoLayout :: UI ()
		redoLayout = void $ do
			emode <- liftIO (readIORef mode)
			edits <- liftIO (readIORef edited)
			let canedit = canEdit emode
			layout <- mkLayout canedit =<< liftIO (readIORef inputs)
			getBody window # set children [layout]
			displayAll
		
		mkLayout :: Bool -> [Element] -> UI Element
		mkLayout e xs = column $
			([row [element elMode], UI.hr]++
			mkLine e xs key ++
			[UI.hr, row [element elSave, element elDrop]])
			
		mkLine :: Bool -> [Element] -> String -> [UI Element]
		mkLine e xs s = [row [UI.span # set text s]] ++ rebutrows
			where
				rebutrows = if e
								then concatMap (\x -> [row x]) (reverse xs')
								else [row [element result]]
				xs' = (ebut (head xs)):(map (\x -> [element x]) (tail xs))
				ebut y = (element y):(if e then [element elPlus, element elMinu] else [])

	-- Mode switching
	let 
		toggleMode :: UI ()
		toggleMode = do
			liftIO $ modifyIORef mode (toggle)

		toggler = void $ do
			emode <- liftIO (readIORef mode)
			element elMode # set text (show.toggle$emode)

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
			elInput <- UI.input # set value s
			on (domEvent "livechange") elInput $ \_ -> (liftIO $ modifyIORef edited (const True)) >> displayAll
			liftIO $ modifyIORef inputs (elInput:)
			
		addInput :: UI ()
		addInput = addInputVal ""

		dropInput :: UI ()
		dropInput = do
			elInput <- UI.input # set value ""
			on (domEvent "livechange") elInput $ \_ -> (liftIO $ modifyIORef edited (const True)) >> displayAll
			liftIO $ modifyIORef inputs ((cond null (elInput:) id).(drop 1))

	-- Click behaviors

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

-- Mode toggle

data Mode = View | Edit
	deriving (Read, Show, Eq)

toggle :: Mode -> Mode
toggle View = Edit
toggle Edit = View

canEdit :: Mode -> Bool
canEdit View = False
canEdit Edit = True

-- conditional operator: performs function on value depending on test of value

cond :: (a -> Bool) -> (a -> b) -> (a -> b) -> (a -> b)
cond p f g = \x -> if p x then f x else g x

example = ("Cat",[])
-}
