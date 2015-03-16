{-# LANGUAGE RecordWildCards, OverloadedStrings, DoRec #-}
module App where
------------------------------
import Deck
import Cards
import Cards.Common 
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
import Data.List.Split
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
------------------------------
import Database
import TagState
------------------------------
import App.FilterCard
--import App.SingleCard
import App.Home
import App.Universal
import App.Deck
------------------------------
import App.Renderer.FilterCard
import App.Renderer.SingleCard
import App.Renderer.Deck
------------------------------
import App.Filtering
import App.Widgets
import App.Core
------------------------------
import qualified Graphics.UI.Threepenny          as UI
import qualified Graphics.UI.Threepenny.Core     as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Elements (addStyleSheet)

setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title appname
    UI.addStyleSheet window "style.css"

  --- Modes!
    butAHome <- UI.button # settext "Home"
    butQCard <- UI.button # settext "Cards"
    butBDeck <- UI.button # settext "Deck Builder"

    let eAHome = UI.click butAHome
        eQCard = UI.click butQCard
        eBDeck = UI.click butBDeck

        navigator :: [UI Element]
        navigator = [element butAHome, element butQCard, element butBDeck]

    let eModeChange = head <$> unions
          [ Home <$ eAHome
          , FilterCard <$ eQCard
          , DeckBuilder <$ eBDeck
          --, ShowCard <$ eSCard
          ]

    bMulti <- stepper True $ head <$> unions [ True <$ eQCard, False <$ eBDeck ]

    bMode <- stepper Home eModeChange
    
  --- Mode specific things (FilterCard and DeckBuilder)

    (fcbl@FCBL{..}, dbbl, ams@AMS{..}) <- selectors bMulti eModeChange

    rec
        draftCheck <- UI.input # UI.set UI.type_ "checkbox"
        element draftCheck # sink UI.checked bDMode
        bDMode <- stepper False $ UI.checkedChange draftCheck

    let uiDraft = row [ UI.bold #+ [ UI.string "Draft Mode:" ], element draftCheck ]

    let fcHeader = fcAmsHeader ams
        dbHeader = dbAmsHeader uiDraft ams

    let bQCFilter  = behaveBFilter fcbl 
        bQDFilter  = behaveBFilter dbbl 

        bQFilter = if_ <$> bMulti <*> bQCFilter <*> bQDFilter
        bQMatches = toList.applyFilter <$> bQFilter

        bNoMatches = length <$> bQMatches

    let pageSize = 100
        rowSize = 25
        colSize = 4

    rec
        let bcView = ListView <$> (pure pageSize) <*> bCur
            bdView = GridView <$> (pure rowSize) <*> (pure colSize) <*> bCur

        stRanger <- ranger bCur bFirst bLast (psss)
        let tRanger = userLoc stRanger
            eRanger = rumors   tRanger
            bRanger = facts    tRanger
            bFirst = pure 0
            bLast = (pred).(`cdiv`pageSize) <$> bNoMatches

        bCur <- stepper 0 $ head <$> unions [ eRanger, 0 <$ eModeChange ]

    let 
        bcLabel = pure gname
        bLinker = pure (head.curls)
        
        bLabel = if_ <$> bMulti <*> bcLabel <*> bLinker

        bcRower = pure tabulate
        bdRower = pure (\x y -> element y)

        bRower :: Behavior (GenCard -> LiquidLink Int -> UI Element)
        bRower = if_ <$> bMulti <*> bcRower <*> bdRower

        bcAggra = pure (theader:)
        bdAggra = pure (map (UI.tr #+) . chunksOf colSize . map (\x -> UI.td #+ [x]))
        
        bAggra = if_ <$> bMulti <*> bcAggra <*> bdAggra
        
    (qList,(qGrid,obscurae)) <- mpair $ knock6 (derangedCask,oculus) bQMatches pageSize stRanger bLabel bRower bAggra

    let tResults = if_  <$> (tidings bMulti never) <*> (userActive qList) <*> (userActive qGrid)
        eResults = rumors tResults
    bResults <- stepper (-1) $ eResults

    let eObscure = (map (rumors.ebbLink) obscurae)
        eOccult = head <$> unions (eObscure++[ (-1) <$ eRanger])
    bOccult <- stepper (-1) $ eOccult

    rec
        let ePutGenCard = whenE ((&&) <$> (not <$> bMulti) <*> ((>=0) <$> bResults)) ((!!) <$> bQMatches <@> eResults)
            eDecGenCard = whenE ((&&) <$> (not <$> bMulti) <*> ((>=0) <$> bOccult)) ((!!) <$> bQMatches <@> eOccult)
            ePutCard = fromGeneric <$> ePutGenCard
            eAddCard = filterApply (lacks <$> bDeck) ePutCard
            eIncCard = filterApply (has <$> bDeck) ePutCard
            eDecCard = fromGeneric <$> eDecGenCard

        bDeck <- accumB emptyDeck $ concatenate <$> unions
            [ addCard <$> eAddCard
            , incCard <$> bDMode <@> eIncCard
            , decCard <$> eDecCard
            ]

        {-let eSCard = whenE ((&&) <$> bMulti <*> ((>=0) <$> bResults)) ((!!) <$> bQMatches <@> eResults)
        
        bSingle <- stepper Nothing $ head <$> unions
            [ Just <$> eSCard
            , Nothing <$ (head <$> unions [ eQCard, eAHome, eBDeck ])
            ]

        let [buiCardImgs, buiCardText, buiCardInfo] = blTranspose 3 noop $ (maybe [] renderCard <$> bSingle)
        -}

        

    scSelect <- UI.span
    element scSelect # sink UI.text ((gname!?"Nothing selected") <$> bResults <*> bQMatches)

    scIndex <- UI.span
    element scIndex # sink UI.text (show <$> bResults)

    {-
    cardSide <- UI.div
    element cardSide # sink schildren ((:[]) <$> buiCardInfo)

    scCenter <- UI.div
    element scCenter # sink schildren ((\x y -> [ column [ x ], column [ y ]]) <$> buiCardImgs <*> buiCardText)
    -}

    deckSide <- UI.div
    element deckSide # sink schildren (construct <$> bDeck)

    let
        fcContent :: UI Element
        dbContent :: UI Element
        --scContent :: UI Element

        fcFooter :: UI Element
        dbFooter :: UI Element
        --scFooter :: UI Element

        fcSideBar :: UI Element
        dbSideBar :: UI Element
        --scSideBar :: UI Element
        
        fcDebugger :: UI Element
        dbDebugger :: UI Element
        --scDebugger :: UI Element

        fcContent = element qList
        dbContent = element qGrid
        --scContent = element scCenter

        --scHeader = noop

        fcFooter = element stRanger
        dbFooter = element stRanger
        --scFooter = noop

        fcSideBar = noop
        dbSideBar = element deckSide
        --scSideBar = element cardSide

        fcDebugger = row [element scSelect, element scIndex]
        dbDebugger = row [element scSelect, element scIndex]
        --scDebugger = row [element scSelect, element scIndex]

    let
        displayHeader   = (:[]) . hfdsCase hmHeader fcHeader dbHeader noop -- scHeader
        displayContent  = (:[]) . hfdsCase hmContent fcContent dbContent noop -- scContent
        displayFooter   = (:[]) . hfdsCase hmFooter fcFooter dbFooter noop -- scFooter
        displaySideBar  = (:[]) . hfdsCase hmSideBar fcSideBar dbSideBar noop -- scSideBar
        displayDebugger = (:[]) . hfdsCase hmDebugger fcDebugger dbDebugger noop -- scDebugger

    content <- UI.div
    header <- UI.div
    footer <- UI.span
    sidebar <- UI.div
    dbg <- UI.span
    element content # sink schildren (displayContent <$> bMode)
    element header # sink schildren (displayHeader <$> bMode)
    element footer # sink schildren (displayFooter <$> bMode)
    element sidebar # sink schildren (displaySideBar <$> bMode)
    element dbg # sink schildren (displayDebugger <$> bMode)

    getBody window # UI.set schildren ([column [ row navigator, row [element header], row [column [ element content ], column [ element sidebar ]], row [element footer], row [element dbg]]])
