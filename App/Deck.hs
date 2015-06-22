{-# LANGUAGE RecordWildCards #-} 
module App.Deck where
-------------------------------------------------
import Prelude hiding (div)
import Data.Maybe
import Data.List.Split
import Data.List
import Data.IORef
--------------------------------------------------
import API.Database
import API.TagState
import API.IxMap
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
