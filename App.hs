{-# LANGUAGE RecordWildCards, OverloadedStrings, RecursiveDo #-}
module App where
------------------------------
import CCG
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
import API.Database
import API.TagState
import API.IxMap
import API.Filter
------------------------------
import App.FilterCard
--import App.SingleCard
import App.Home
import App.Universal
import App.Deck
------------------------------
import Renderer.FilterCard
import Renderer.SingleCard
import Renderer.Cards
import Renderer.Deck
import qualified Renderer.Core as R
------------------------------
import App.Filtering
import App.Widgets
import App.Core
----------------------------
import Util
-----------------------------
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

    bMulti <- stepper True $ head <$> unions [ True <$ eQCard, False <$ eBDeck ]

    bHome <- stepper True $ head <$> unions [ True <$ eAHome, False <$ eBDeck, False <$ eQCard ]
    let eModeChange = head <$> unions [ Home <$ eAHome, FilterCard <$ eQCard, DeckBuilder <$ eBDeck ]

    bMode <- stepper Home eModeChange

  --- Mode elements/behaviors for FilterCard and DeckBuilder

    (fcbl@FCBL{..}, dbbl, ams@AMS{..}) <- selectors bMulti eModeChange

    rec
        draftCheck <- UI.input # UI.set UI.type_ "checkbox"
        element draftCheck # sink UI.checked bDMode
        bDMode <- stepper False $ UI.checkedChange draftCheck

    let bQCFilter  = behaveBFilter fcbl
        bQDFilter  = behaveBFilter dbbl

        bQFilter = if_ <$> bMulti <*> bQCFilter <*> bQDFilter
        bQMatches = toList.applyFilter <$> bQFilter

        bNoMatches = length <$> bQMatches

    let pageSize = 20
        rowSize = 5
        colSize = 4

        bNpp = if_ <$> bMulti <*> (pure pageSize) <*> (pure (rowSize*colSize))


    rec stRanger <- ranger bCur bFirst bLast (psss)
        let tRanger = userLoc stRanger
            eRanger = rumors   tRanger
            bRanger = facts    tRanger
            bFirst = pure 0
            bLast = (pred) <$> (cdiv <$> bNoMatches <*> bNpp)
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
        let ePutGenCard = whenE ((&&) <$> (not <$> bMulti) <*> ((>=0) <$> bResults)) ((!!) <$> bQMatches <@> (filterE (>=0) eResults))
            eDecGenCard = whenE ((&&) <$> (not <$> bMulti) <*> ((>=0) <$> bOccult)) ((!!) <$> bQMatches <@> (filterE (>=0) eOccult))
            ePutCard = fromGeneric <$> ePutGenCard
            eAddCard = filterApply (lacks <$> bDeck) ePutCard
            eIncCard = filterApply (has <$> bDeck) ePutCard
            eDecCard = fromGeneric <$> eDecGenCard

        bDeck <- accumB emptyDeck $ concatenate <$> unions
            [ addCard <$> eAddCard
            , incCard <$> bDMode <@> eIncCard
            , decCard <$> eDecCard
            ]

  --- Mode elements/behaviors/handling for ShowCard
    let eSCard = whenE ((&&) <$> (bMulti) <*> ((>=0) <$> bResults)) ((!!) <$> bQMatches <@> (filterE (>=0) eResults))

    let eShowMode = head <$> unions [ False <$ eQCard, False <$ eAHome, False <$ eBDeck, True <$ eSCard ]
    bShow <- stepper False eShowMode

    bSingle <- stepper Nothing $ head <$> unions
        [ Just <$> eSCard
        , Nothing <$ eQCard
        , Nothing <$ eBDeck
        , Nothing <$ eAHome
        ]

  --- Multiselect clear handles
    on UI.click clearsTyp $ \_ -> (element uSelectTyp # UI.set clearSels ())
    on UI.click clearsCol $ \_ -> (element uSelectCol # UI.set clearSels ())
    on UI.click clearsSet $ \_ -> (element uSelectSet # UI.set clearSels ())
    on UI.click clearsRar $ \_ -> (element uSelectRar # UI.set clearSels ())

  --- UI Elements and layout
    scSelect <- UI.span
    element scSelect # sink UI.text ((uname!?"Nothing selected") <$> bResults <*> bQMatches)

    scIndex <- UI.span
    element scIndex # sink UI.text (show <$> bResults)

    cardSide <- UI.div
    element cardSide # sink (kinder (maybe [bstring "Show Card"] (once cardInfo))) bSingle

    scCenter <- UI.div
    element scCenter # sink (kinder (maybe [bstring "Show Card"] renderCard)) bSingle

    deckSide <- UI.div
    element deckSide # sink (kinder construct) bDeck

  --- UI section elements
    hmContent <- column ((R.h1 R.#$ R.string appfname):(map ((R.p R.#$).R.string) welcomeText))
    fcContent <- element qList
    dbContent <- element qGrid
    scContent <- element scCenter

    hmHeader <- noop
    fcHeader <- fcAmsHeader ams
    dbHeader <- let uiDraft = row [ bstring "Draft Mode:", element draftCheck ] in dbAmsHeader uiDraft ams
    scHeader <- estring UI.p "Show Card"

    hmFooter <-  homeFoot
    fcFooter <- element stRanger
    dbFooter <- element stRanger
    scFooter <- estring UI.p "Show Card"


    hmSideBar <- noop
    fcSideBar <- noop
    dbSideBar <- element deckSide
    scSideBar <- element cardSide

    hmDebugger <- row [string "This is the debugger! Anything you see here is special information used to debug the app."]
    fcDebugger <- row [element scSelect, element scIndex]
    dbDebugger <- row [element scSelect, element scIndex]
    scDebugger <- row [element scSelect, element scIndex]

    let
        displayHeader   = ((:[]).) . hfdsCase hmHeader fcHeader dbHeader scHeader
        displayContent  = ((:[]).) . hfdsCase hmContent fcContent dbContent scContent
        displayFooter   = ((:[]).) . hfdsCase hmFooter fcFooter dbFooter scFooter
        displaySideBar  = ((:[]).) . hfdsCase hmSideBar fcSideBar dbSideBar scSideBar
        displayDebugger = ((:[]).) . hfdsCase hmDebugger fcDebugger dbDebugger scDebugger

    nav <- UI.div
    content <- UI.div
    header <- UI.div
    footer <- UI.span
    sidebar <- UI.div
    dbg <- UI.span

    element content # sink children (displayContent <$> bMode <*> bShow)
    element header # sink children (displayHeader <$> bMode <*> bShow)
    element footer # sink children (displayFooter <$> bMode <*> bShow)
    element sidebar # sink children (displaySideBar <$> bMode <*> bShow)
    element dbg # sink children (displayDebugger <$> bMode <*> bShow)

    getBody window #+ [column [ row navigator, row [element header], row [column [ element content ], column [ element sidebar ]], row [element footer], row [element dbg]]]
