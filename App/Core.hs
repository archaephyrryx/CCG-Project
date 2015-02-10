{-# LANGUAGE RecordWildCards #-}
module App.Core where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import App.Core.Modes
import App.Core.AppData
import App.Core.Helper
import qualified Graphics.UI.Threepenny          as UI
import qualified Graphics.UI.Threepenny.Core     as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import Graphics.UI.Threepenny.Core

data Applet = Applet {
    appMode_ :: AppModeS,
    initState :: UI AppState,
    appRun :: Window -> SigStateT
    --applet  :: AppProcs
}

type SigState = (AppSignal, AppState)
type SigStateT = SigState -> UI SigState
type AppStateT = AppState -> UI SigState

data AppSignal = Continue | Change AppChange | Switch_ AppModeS | Switch AppMode deriving (Eq, Show)

data AppChange = Initialize | Rerender | Update | Next | Prev deriving (Eq, Show)

remain :: AppSignal -> Bool
remain Continue = True
remain (Change _) = True
remain _ = False

data AppState = AppState {
    appElems :: AEL, -- Applet Element List
    appRules :: ABL, -- Applet Behavior List
    appMode :: AppMode
}

data AppProcs = AppProcs { update :: AppStateT
                         , rerender :: AppStateT
                         , initialize :: AppStateT
                         , goNext :: AppStateT
                         , goPrev :: AppStateT
                         }
