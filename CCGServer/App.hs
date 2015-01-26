{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
    DeriveDataTypeable, GeneralizedNewtypeDeriving,
	RecordWildCards, TemplateHaskell, TypeFamilies,
	OverloadedStrings #-}

module CCGServer.App where

import Cards
import Cards.Common
import Cards.Common.Abbrev
import Cards.Common.Color
import Cards.Common.Hint
import Cards.Common.Stringe
import Cards.Common.Values
import Cards.Generic
import Control.Applicative
import Control.Monad
import Database
import Data.Data (Data, Typeable)
import Data.IORef
import Data.IxSet
import TagState
import Data.List
import Data.Maybe
import Data.Map (Map)
import CCGServer.Widgets
import CCGServer.Filter
import CCGServer.Card
import CCGServer.Deck
import qualified Data.Map as Map
import				Graphics.UI.Threepenny.Core
import qualified	Graphics.UI.Threepenny 			as UI

appname = "ThreePony"
appdesc = "An Advanced MLP:CCG Toolkit"
appfname = (appname ++ ": " ++ appdesc)

for :: [a] -> (a -> b) -> [b]
for [] _ = []
for (x:xs) f = f x : for xs f




data AppMode = HomeMode
             | FilterCardMode Filter
             | ShowCardMode GenCard
             | DeckBuilderMode Filter [Card]
     deriving (Read, Show, Eq)

data ViewMode = ListView Int Int
                deriving (Read, Show, Eq)

data TagMode = View
             | Edit
             deriving (Read, Show, Eq)

toggle :: TagMode -> TagMode
toggle View = Edit
toggle Edit = View

canEdit :: TagMode -> Bool
canEdit View = False
canEdit Edit = True

setup :: Window -> UI ()
setup window = do
	-- GUI elements
	return window # set title appfname
    UI.addStyleSheet window "style.css"

    -- Universal
    appmode <- liftIO $ newIORef HomeMode

    butHome <- UI.button #. "nav-btn" #+ [string "Home"]
    butCFil <- UI.button #. "nav-btn" #+ [string "Cards"]
    butCDec <- UI.button #. "nav-btn" #+ [string "Deck Builder"]
    elSearch <- searchBar cardDB "" (string . show . gname)

	content <- UI.div

    -- FilterCard and DeckBuilder only
	selectTyp <- multiSelect typeValues [] (string . show)
    clearsTyp <- UI.button #. "clear-btn" #+ [string "clear"]
	selectCol <- multiSelect colorValues [] (string . show)
    clearsCol <- UI.button #. "clear-btn" #+ [string "clear"]
	selectRar <- multiSelect rarityValues [] (string . show)
    clearsRar <- UI.button #. "clear-btn" #+ [string "clear"]
	selectSet <- multiSelect setValues [] (string . show)
    clearsSet <- UI.button #. "clear-btn" #+ [string "clear"]

    cdFilter <- liftIO $ newIORef (blankCardFilter)
    cdMatches <- liftIO $ newIORef (IxSet.empty)

    -- FilterCard only
    (minPow, maxPow) <- minmax Nothing Nothing (show . val)
    (minCost, maxCost) <- minmax Nothing Nothing (show . val)
    (minReq, maxReq) <- minmax Nothing Nothing (show . val)

    cardView <- liftIO $ newIORef (ListView 100 0)

    prevPage <- UI.button #. "page-btn" #+ [string "<"]
    pageNo   <- string "1"
    nextPage <- UI.button #. "page-btn" #+ [string ">"]

    -- DeckBuilder Only
    
    deck <- liftIO $ new IORef []
    
    -- Tagging Only
	elPlus	<- UI.button #+ [string "+"]
	elMinu	<- UI.button #+ [string "-"]
	elSave	<- UI.button #+ [string "Save Changes"]
	elDrop	<- UI.button #+ [string "Discard Changes"]
	elMode	<- UI.button #+ [string "View"]
	elBlank	<- UI.input  #+ [string  ""]

	inputs <- liftIO $ newIORef []
	mode   <- liftIO $ newIORef View
	edited <- liftIO $ newIORef False
	dats   <- liftIO $ newIORef example

	-- GUI Layout
	let
		displayAll = void $ do
            amode <- liftIO (readIORef appmode)
            case amode of
                HomeMode -> displayHome
                FilterCardMode f -> liftIO $ modifyIORef cdFilter (const f) >> updateFilter >> displayFilterCard
                ShowCardMode g -> displayCard g
                DeckBuilderMode f cs -> liftIO $ modifyIORef cdFilter (const f) >> updateFilter >> displayDeckBuilder
        
        displayHome = void $ do
            element content # set children [
              UI.h1 projectName
            , UI.p #+ [UI.string ("Welcome to "++projectName++", an in-development project for the MLP:CCG")]
            , UI.p #+ [UI.string ("This site was inspired by, and modelled on, ")
                      ,UI.a # set UI.href "http://ponyhead.com" # set UI.text "PonyHead"
                      ]
            , UI.p #+ [UI.string ("This is merely one branch of a wider project. Be sure to check out the others.")]
            ]

        displayFilterCard = void $ do
            tabulate

        displayCard = void $ do
            amode <- liftIO (readIORef appmode)
            let (ShowCardMode g) = amode
            element content # set children [renderCard g]

        displayDeckBuilder = void $ do
            builddeck

        builddeck = void $ do
            element content # set children [string "Deck builder not written yet."]

		updateFilter = void $ do
            amode <- liftIO (readIORef appmode)
            case amode in
                FilterCardMode _ -> updateCardFilter
                DeckBuilderMode _ _ -> updateDeckFilter
                _ -> return ()

        updateDeckFilter = void $ do
			ftyp <- map   (typeValues!!) <$> get selections selectTyp
			fcol <- map  (colorValues!!) <$> get selections selectCol
			frar <- map (rarityValues!!) <$> get selections selectRar
			fset <- map    (setValues!!) <$> get selections selectSet
            let newFilter = DeckFilter fcol fset ftyp frar
            liftIO $ modifyIORef cdFilter (const newFilter)
            updateCardMatches

        updateCardFilter = void $ do
            pmin <- get value minPow
            pmax <- get value maxPow
            cmin <- get value minCost 
            cmax <- get value maxCost 
            rmin <- get value minReq 
            rmax <- get value maxReq 
			ftyp <- map   (typeValues!!) <$> get selections selectTyp
			fcol <- map  (colorValues!!) <$> get selections selectCol
			frar <- map (rarityValues!!) <$> get selections selectRar
			fset <- map    (setValues!!) <$> get selections selectSet
            let newFilter = CardFilter pmin pmax cmin cmax rmin rmax fcol fset ftyp frar
            liftIO $ modifyIORef cdFilter (const newFilter)
            updateCardMatches

        updateCardMatches = void $ do
            xfilter <- liftIO $ readIORef cdFilter
            liftIO $ modifyIORef cdMatches (const (applyFilter xfilter))

        tabulate = void $ do
            cview <- liftIO (readIORef cardView)
            let (ListView npp pn) = cview
            matches <- (take npp . drop (npp*pn) . toList) <$> liftIO (readIORef cdMatches)
            element content # set children $
              [UI.table #+ $
                [ UI.tr #+ $ map (UI.th) $ [ "#" , "Rarity" , "Type" , "Cost" , "Req." , "Name" , "Power" ]
                , for matches (\g@GenCard{..} ->
                    UI.tr #+ $
                      map (\x -> UI.td #+ [x]) $
                        [ UI.string $ genset g
                        , UI.string $ brief rar
                        , iconic ctype
                        , UI.string $ fromMaybe "" (show.val <$> mcost)
                        , reqtify g
                        , pronounce (unravel name, genset $ g)
                        , empower g
                        ]
                    )
                ]
              ]

		redoLayout :: UI ()
		redoLayout = void $ do
			amode <- liftIO (readIORef appmode)
            cview <- liftIO (readIORef cardView)
			layout <- mkLayout amode []
			getBody window # set children [layout]
			displayAll

		mkLayout :: AppMode -> [Element] -> UI Element
		mkLayout m xs = column [row nav, row (outlay m)]

        nav :: Element
        nav = UI.li #. "nav" #+ [element butHome, element butCFil, element butCDec, element elSearch]

        outlay :: AppMode -> [Element]
        outlay mod = case mod of
            HomeMode -> [element content]
            FilterCardMode f -> outlayFC
            ShowCardMode g -> 
            DeckBuilderMode f d -> outlayDB d
          
        outlayFC = [ row
                      [ column
                          [ row [element minPow, UI.b #+ [string "to"], element maxPow]
                          , row [element minCost, UI.b #+ [string "to"], element maxCost]
                          , row [element minReq, UI.b #+ [string "to"], element maxReq]
                          ]
                      , column [element selectTyp, element clearsTyp]
                      , column [element selectCol, element clearsCol]
                      , column [element selectRar, element clearsRar]
                      , column [element selectSet, element clearsSet]
                      ]
                   , row [element content]
                   , row [element prevPage, element pageNo, element nextPage]
                   ]

        outlaySC = [] 
        outlayDB d = [ column
                      [ row
                        [ column [element selectTyp, element clearsTyp]
                        , column [element selectCol, element clearsCol]
                        , column [element selectRar, element clearsRar]
                        , column [element selectSet, element clearsSet]
                        ]
                      , row [element content]
                      , row [element prevPage, element pageNo, element nextPage]
                      ]
                   , column [construct d]
                   ]


        mkLine :: Bool -> [Element] -> String -> [UI Element]
		mkLine e xs s = [row [UI.span # set text s]] ++ rebutrows
			where
				rebutrows = if e
								then concatMap (\x -> [row x]) (reverse xs')
								else [row [element result]]
				xs' = (ebut (head xs)):(map (\x -> [element x]) (tail xs))
				ebut y = (element y):(if e then [element elPlus, element elMinu] else [])

		clearAll :: UI ()
		clearAll = do
			element elType # set value ""
			element elColo # set value ""
			redoLayout

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
	on (domEvent "livechange") elType $ \_ -> displayAll
	on (domEvent "livechange") elColo $ \_ -> displayAll
	on UI.click elCler $ const $ do
		clearAll
		redoLayout
	redoLayout

-- conditional operator: performs function on value depending on test of value

cond :: (a -> Bool) -> (a -> b) -> (a -> b) -> (a -> b)
cond p f g = \x -> if p x then f x else g x
