{-# LANGUAGE RecordWildCards, DoRec #-} 
module App.Deck where
-------------------------------------------------
import Prelude hiding (div)
import Data.Maybe
import Data.List.Split
import Data.List
import Data.IORef
--------------------------------------------------
import API
import Data.IxSet
--------------------------------------------------
import CCG
-------------------------------------------------
import App.Core
import App.Filtering
import App.Widgets
import Renderer.Deck
import App.Universal
--------------------------------------------------
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Elements hiding (map)
import Graphics.UI.Threepenny.Core
---------------------------------------------------


unnamedMonoSelect :: Element -> MonoSelect a -> UI Element
unnamedMonoSelect cler sel = column [ row [ element cler ], row [ element sel ] ]

dbAmsHeader :: UI Element -> AMS -> UI Element
dbAmsHeader uiDraft a@AMS{..} = do
        uoSelectTyp <- unnamedMonoSelect clearsTyp oSelectTyp
        uoSelectCol <- unnamedMonoSelect clearsCol oSelectCol
        uoSelectSet <- unnamedMonoSelect clearsSet oSelectSet
        uoSelectRar <- unnamedMonoSelect clearsRar oSelectRar
        uoSelects <- selectAll [ element uoSelectTyp, element uoSelectCol, element uoSelectSet, element uoSelectRar]
        dbHeader <- row [ column [ uiDraft ],  column [ element uoSelects ] ]
        return dbHeader


{-
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
    let
        dbHeader = row [ column [ uiDraft ],  column [ uiSelects ] ]
        dbContent = element qGrid
        dbFooter = element stRanger
        dbSideBar = element deckSide
        dbDebugger = row [element scSelect, element scIndex]
    return DBCL{..}
-}
