{-# LANGUAGE TemplateHaskell #-}

module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Data.IORef
import qualified Data.ByteString.Char8 as B
import Snaplets

------------------------------------------------------------------------------
data App = App
    { _heist    :: Snaplet (Heist App)
    , _foo      :: Snaplet Foo
    , _bar      :: Snaplet Bar
    , _sitename :: IORef B.ByteString
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
type AppHandler = Handler App App
