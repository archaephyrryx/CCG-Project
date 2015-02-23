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

  --- Mode specific things (Home)
    let welcomeText :: [UI Element]
        welcomeText = [ UI.h1 #+ [string appfname] , UI.p #+ [string "Something will be written here eventually."] ]

    butFoo <- UI.button # settext "Once More into the Breach"

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

        let bLabel = pure gname
            bcRower = pure tabulate
            bdRower = pure (\x y -> element y)

            bRower :: Behavior (GenCard -> LiquidLink Int -> UI Element)
            bRower = if_ <$> bMulti <*> bcRower <*> bdRower

            bcAggra = pure (theader:)
            bdAggra = pure (map (UI.tr #+) . chunksOf colSize . map (\x -> UI.td #+ [x]))
            
            bAggra = if_ <$> bMulti <*> bcAggra <*> bdAggra
            
        (qResults,qLinks) <- derangedCask bQMatches pageSize stRanger bLabel bRower bAggra

        let tResults = userActive qResults
            eResults = rumors     tResults
        bResults <- stepper (-1) $ eResults

        let eAddCard = whenE ((&&) <$> (not <$> bMulti) <*> ((>=0) <$> bResults)) ((!!) <$> bQMatches <@> eResults)
            eIncCard = never
            eDecCard = never

        bDeck <- accumB emptyDeck $ concatenate <$> unions
            [ addCard.fromGeneric <$> eAddCard
            , incCard <$> (pure True) <@> eIncCard
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
        fcHeader :: UI Element
        fcHeader = row [ column [ uiRanges ], column [ uiSelects ] ]

        dbHeader :: UI Element
        dbHeader = row [ column [ uiSelects ] ]

        fcContent :: UI Element
        fcContent = element qResults

        dbContent :: UI Element
        dbContent = element qResults

        fcFooter :: UI Element
        fcFooter = element stRanger

        dbFooter :: UI Element
        dbFooter = element stRanger

        dbSideBar :: UI Element
        dbSideBar = element deckSide

        fcDebugger :: UI Element
        fcDebugger = row [element scSelect, element scIndex]

        dbDebugger :: UI Element
        dbDebugger = row [element scSelect, element scIndex]
  
    let
        displayMode :: AppMode -> [UI Element]
        displayMode mod = case mod of
            Home -> displayHome
            FilterCard -> displayFC
            DeckBuilder -> displayDB
        
        debugMode :: AppMode -> [UI Element]
        debugMode mod = case mod of
            Home -> []
            FilterCard -> [ fcDebugger ]
            DeckBuilder -> [ dbDebugger ]
        
        displayHome = welcomeText
        displayFC = [ column [ row [ fcHeader ], row [ fcContent ], row [ fcFooter ]]]
        displayDB = [ column [ row [ dbHeader ], row [ column [ dbContent ] , column [ dbSideBar ] ]]]


    content <- UI.div
    element content # sink schildren (displayMode <$> bMode)
    dbg <- UI.span
    element dbg # sink schildren (debugMode <$> bMode)

    getBody window # UI.set schildren ([column [ row navigator, row [element dbg], row [ element content ]]])
