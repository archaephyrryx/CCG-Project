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

        let dbbl = DBBL{..}

        let rowSize = 25
            colSize = 4
            bdView = GridView <$> (pure rowSize) <*> (pure colSize) <*> bCur

        stRanger <- ranger bCur bFirst bLast (psss)
        let tRanger = userLoc stRanger
            eRanger = rumors   tRanger
            bRanger = facts    tRanger
            bFirst = pure 0
            bLast = (pred).(`cdiv`pageSize) <$> bNoMatches
        bCur <- stepper 0 $ head <$> unions [ eRanger, 0 <$ eModeChange ]

        let 
            bLinker = pure (head.curls)

            bdRower = pure (\x y -> element y)

            bdAggra = pure (map (UI.tr #+) . chunksOf colSize . map (\x -> UI.td #+ [x]))
            
        (qGrid,obscurae) <- oculus bQMatches pageSize stRanger bLabel bRower bAggra

        let tResults = if_  <$> (tidings bMulti never) <*> (userActive qList) <*> (userActive qGrid)
            eResults = rumors     tResults
        bResults <- stepper (-1) $ eResults

        rec
            draftCheck <- UI.input # UI.set UI.type_ "checkbox"
            element draftCheck # sink UI.checked bDMode
            bDMode <- stepper False $ UI.checkedChange draftCheck

        let uiDraft = row [ UI.bold #+ [ UI.string "Draft Mode:" ], element draftCheck ]

        let eObscure = (map (rumors.ebbLink) obscurae)
            eOccult = head <$> unions (eObscure++[ (-1) <$ eRanger])
        bOccult <- stepper (-1) $ eOccult

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
        
    scSelect <- UI.span
    element scSelect # sink UI.text ((gname!?"Nothing selected") <$> bResults <*> bQMatches)

    scIndex <- UI.span
    element scIndex # sink UI.text (show <$> bResults)

    deckSide <- UI.div
    element deckSide # sink schildren (construct <$> bDeck)

        dbHeader :: UI Element
        dbHeader = row [ column [ uiDraft ],  column [ uiSelects ] ]

        dbContent :: UI Element
        dbContent = element qGrid

        dbFooter :: UI Element
        dbFooter = element stRanger

        dbSideBar :: UI Element
        dbSideBar = element deckSide
        
        dbDebugger :: UI Element
        dbDebugger = row [element scSelect, element scIndex]
