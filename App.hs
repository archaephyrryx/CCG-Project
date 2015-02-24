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
import App.Renderer.FilterCard
import App.Renderer.SingleCard
import App.Renderer.Deck
import App.Core.AppData
import App.Core.Helper
import App.Core.Modes
--import App.Deck
import App.Filtering
import App.Widgets
------------------------------
import qualified Graphics.UI.Threepenny          as UI
import qualified Graphics.UI.Threepenny.Core     as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Elements (addStyleSheet)

powerless :: Maybe Power
powerless = Nothing

priceless :: Maybe Cost
priceless = Nothing

boundless :: Maybe Req
boundless = Nothing

appname = "ThreePony"
appdesc = "An Advanced MLP:CCG Toolkit"
appfname = (appname ++ ": " ++ appdesc)

welcomeText =
    [ "Welcome to ThreePony! This is an in-development browser-based GUI for the MLP:CCG, borrowing somewhat from PonyHead."
    , "This is very much in-progress, so don't expect fully functional or reliable performance, but thanks for helping test this!"
    , "Any comments, bug reports, questions, feature requests, or other feedback should go to the GitHub page for this project."
    , "A number of other implementations of this app are being developed as well, though they go by different names."
    ]

setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title appname

  --- General things

    let bColorValues = pure colorValues
        bSetValues = pure setValues
        bRarityValues = pure rarityValues
        bTypeValues = pure typeValues

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
          ]

    bMulti <- stepper True $ head <$> unions [ True <$ eQCard, False <$ eBDeck ]

    bMode <- stepper Home eModeChange


  --- Mode specific things (FilterCard and DeckBuilder)

    rec
        (selectTyp, clearsTyp) <- multiSelect bMulti bTypeValues   bTypSelect (pure ((UI.li #+).(:[]).string.show))
        (selectCol, clearsCol) <- multiSelect bMulti bColorValues  bColSelect (pure ((UI.li #+).(:[]).string.show))
        (selectSet, clearsSet) <- multiSelect bMulti bSetValues    bSetSelect (pure ((UI.li #+).(:[]).string.show))
        (selectRar, clearsRar) <- multiSelect bMulti bRarityValues bRarSelect (pure ((UI.li #+).(:[]).string.show))

        let tSelectType   = userSelections selectTyp
            tSelectColor  = userSelections selectCol
            tSelectSet    = userSelections selectSet
            tSelectRarity = userSelections selectRar

            eSelectType   = rumors tSelectType
            eSelectColor  = rumors tSelectColor
            eSelectSet    = rumors tSelectSet
            eSelectRarity = rumors tSelectRarity

            bSelectType   = facts tSelectType
            bSelectColor  = facts tSelectColor
            bSelectSet    = facts tSelectSet
            bSelectRarity = facts tSelectRarity

            eClearTyp = UI.click clearsTyp
            eClearCol = UI.click clearsCol
            eClearSet = UI.click clearsSet
            eClearRar = UI.click clearsRar
        
        bTypSelect <- stepper [] $ head <$> unions [eSelectType,   [] <$ eClearTyp]
        bColSelect <- stepper [] $ head <$> unions [eSelectColor,  [] <$ eClearCol]
        bSetSelect <- stepper [] $ head <$> unions [eSelectSet,    [] <$ eClearSet]
        bRarSelect <- stepper [] $ head <$> unions [eSelectRarity, [] <$ eClearRar]

        let 
            namedMultiSelect :: String -> Element -> MultiSelect a -> UI Element
            namedMultiSelect s cler sel = column [ row [ UI.bold #+ [ string s ], element cler ], row [ element sel ] ]

            uiSelectTyp = namedMultiSelect "Type"   clearsTyp selectTyp
            uiSelectCol = namedMultiSelect "Color"  clearsCol selectCol
            uiSelectSet = namedMultiSelect "Set"    clearsSet selectSet
            uiSelectRar = namedMultiSelect "Rarity" clearsRar selectRar

            selectAll :: [UI Element] -> UI Element
            selectAll xs = row (map (\x -> column [ x ]) xs)

            uiSelects = selectAll [uiSelectTyp, uiSelectCol, uiSelectSet, uiSelectRar]

  --- Mode specific things (FilterCard and DeckBuilder)

    rec
        (minPow, maxPow) <- minmax bPowMin bPowMax (pure (show.val))
        (minCost, maxCost) <- minmax bCostMin bCostMax (pure (show.val))
        (minReq, maxReq) <- minmax bReqMin  bReqMax (pure (show.val))

        bPowMax  <- stepper powerless $ head <$> unions [ rumors . userMin $ minPow , Nothing <$ eModeChange ]
        bPowMin  <- stepper powerless $ head <$> unions [ rumors . userMax $ maxPow , Nothing <$ eModeChange ]
        bCostMax <- stepper priceless $ head <$> unions [ rumors . userMin $ minCost, Nothing <$ eModeChange ]
        bCostMin <- stepper priceless $ head <$> unions [ rumors . userMax $ maxCost, Nothing <$ eModeChange ]
        bReqMax  <- stepper boundless $ head <$> unions [ rumors . userMin $ minReq , Nothing <$ eModeChange ]
        bReqMin  <- stepper boundless $ head <$> unions [ rumors . userMax $ maxReq , Nothing <$ eModeChange ]
        
        let
            namedMinMax :: String -> Min a -> Max a -> UI Element
            namedMinMax s mmin mmax = column [ row [ UI.bold #+ [ string s ] ], row [ element mmin, string "to", element mmax ] ]

            uiPowRange = namedMinMax "Power" minPow maxPow
            uiCostRange = namedMinMax "Cost" minCost maxCost
            uiReqRange = namedMinMax "Requirement" minReq maxReq

            freeRange :: [UI Element] -> UI Element
            freeRange xs = column (map (\x -> row [ x ]) xs)

            uiRanges = freeRange [uiPowRange, uiCostRange, uiReqRange]


        let fcbl = FCBL{..}
            dbbl = DBBL{..}
            bQCFilter  = behaveBFilter fcbl 
            bQDFilter  = behaveBFilter dbbl 

            bQFilter = if_ <$> bMulti <*> bQCFilter <*> bQDFilter
            bQMatches = toList.applyFilter <$> bQFilter

            bNoMatches = length <$> bQMatches

        let pageSize = 100
            rowSize = 25
            colSize = 4
            bcView = ListView <$> (pure pageSize) <*> bCur
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
            
        (qList,qGrid) <- knock6 (derangedCask,oculus) bQMatches pageSize stRanger bLabel bRower bAggra

        let tResults = if_  <$> (tidings bMulti never) <*> (userActive qList) <*> (userActive qGrid)
            eResults = rumors     tResults
        bResults <- stepper (-1) $ eResults

        rec
            draftCheck <- UI.input # UI.set type_ "checkbox"
            element draftCheck # sink checked bDMode
            bDMode <- stepper False $ checkedChange draftCheck

        let uiDraft = row [ UI.bold #+ [ UI.string "Draft Mode:" ], element bDMode ]

        let ePutCard = whenE ((&&) <$> (not <$> bMulti) <*> ((>=0) <$> bResults)) ((!!) <$> bQMatches <@> eResults)
            eAddCard = filterApply (lacks <$> bDeck <*>) ePutCard
            eIncCard = filterApply (has <$> bDeck <*>) ePutCard
            eDecCard = never

        bDeck <- accumB emptyDeck $ concatenate <$> unions
            [ addCard.fromGeneric <$> eAddCard
            , incCard <$> bDMode <@> eIncCard
            , decCard.fromGeneric <$> eDecCard
            ]
        
    scSelect <- UI.span
    element scSelect # sink UI.text ((gname!?"Nothing selected") <$> bResults <*> bQMatches)

    scIndex <- UI.span
    element scIndex # sink UI.text (show <$> bResults)

    deckSide <- UI.div
    element deckSide # sink schildren (construct <$> bDeck)

    scIO <- UI.div

    let
        noop :: UI Element
        noop = UI.a

        fcHeader :: UI Element
        dbHeader :: UI Element
        hmHeader :: UI Element

        fcContent :: UI Element
        dbContent :: UI Element
        hmContent :: UI Element

        fcFooter :: UI Element
        dbFooter :: UI Element
        hmFooter :: UI Element

        fcSideBar :: UI Element
        dbSideBar :: UI Element
        hmSideBar :: UI Element
        
        fcDebugger :: UI Element
        dbDebugger :: UI Element
        hmDebugger :: UI Element

        fcHeader = row [ column [ uiRanges ], column [ uiSelects ] ]
        dbHeader = row [ column [ uiDraft ],  column [ uiSelects ] ]
        hmHeader = noop

        fcContent = element qResults
        dbContent = element qResults
        hmContent = column ([ UI.h1 #+ [string appfname] ]++(map ((UI.p #+).(:[]).string) welcomeText))

        fcFooter = element stRanger
        dbFooter = element stRanger
        hmFooter = element hooves

        fcSideBar = noop
        dbSideBar = element deckSide
        hmSideBar = noop

        fcDebugger = row [element scSelect, element scIndex]
        dbDebugger = row [element scSelect, element scIndex]
        hmDebugger = row [string "This is the debugger! Anything you see here is special information used to debug the app."]
  
    let
        displayHeader   = (:[]) . hfdsCase hmHeader fcHeader dbHeader noop
        displayContent  = (:[]) . hfdsCase hmContent fcContent dbContent noop
        displayFooter   = (:[]) . hfdsCase hmFooter fcFooter dbFooter noop
        displaySideBar  = (:[]) . hfdsCase hmFooter fcFooter dbFooter noop
        displayDebugger = (:[]) . hfdsCase hmDebugger fcDebugger dbDebugger noop

    content <- UI.div
    header <- UI.div
    footer <- UI.span
    sideBar <- UI.div
    element content # sink schildren (displayContent <$> bMode)
    element header # sink schildren (displayHeader <$> bMode)
    element footer # sink schildren (displayFooter <$> bMode)
    element sideBar # sink schildren (displaySideBar <$> bMode)
    dbg <- UI.span
    element dbg # sink schildren (debugMode <$> bMode)

    getBody window # UI.set schildren ([column [ row navigator, row [element header], row [column [ element content, element sidebar ]], row [element footer], row [element debugger]]])
