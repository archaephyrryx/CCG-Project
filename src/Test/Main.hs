{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Test.Main where

import              Control.Lens.TH
import              Control.Lens
import              Data.IORef
import              Control.Applicative
import              Data.Maybe
---------------------------------------------
import              Data.ByteString (ByteString)
import              Control.Monad.IO.Class
import qualified    Data.ByteString.Char8 as B
import qualified    Data.Text as T
import              Snap.Core
import              Snap.Snaplet
import              Snap.Snaplet.Heist
import              Snap.Util.FileServe
import              Heist
import qualified    Heist.Interpreted as I
import              Snap.Http.Server
import              Snap.Snaplet.Config
import              System.IO

------------------------------------------------------------------------------
data App = App
    { _heist    :: Snaplet (Heist App)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
type AppHandler = Handler App App

main :: IO ()
main = serveSnaplet defaultConfig app

app :: SnapletInit App App
app = makeSnaplet "snapplejack" "The SnappleJack Web Server" Nothing $ do
    hs <- nestSnaplet "heist" heist $ heistInit "test/templates"
    addRoutes routes
    return $ App hs
    where
        routes :: [(ByteString, Handler App App ())]
        routes = [ ("", heistServe') ]

heistServe' :: Handler App App ()
heistServe' = renderWithSplices "test" listSplice

getItems = return ["one", "two", "many"]

listSplice :: Splices (SnapletISplice App)
listSplice = do
    items <- getItems
    "splicer" ## (I.mapSplices (I.runChildrenWithText . itemSplice) items)

itemSplice i = "spliced" ## i
