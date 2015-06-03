{-# LANGUAGE OverloadedStrings #-}

module Test where

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
import              CCG hiding (render)
import              API.Filter
import              Renderer.FilterCard
import              Application

app :: SnapletInit App App
app = makeSnaplet "snapplejack" "The SnappleJack Web Server" Nothing $ do
    hs <- nestSnaplet "heist" heist $ heistInit ""
    addRoutes routes
    return $ App hs
  where
        routes :: [(ByteString, Handler App App ())]
        routes = [ {-("/card", cardHandler)
                 , ("/deck", deckHandler)
                 ,-} ("/res", serveDirectory "res")
                 , ("", heistServe')
                 ]

heistServe' :: Handler App App ()
heistServe' = renderWithSplices "card" filterSplice

filterSplice :: Splices (SnapletISplice App)
filterSplice = resultSplice blankCardFilter
