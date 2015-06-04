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
----------------------------------------
import              Renderer.FilterCard
import              Renderer.Deck
import              Handler.FilterCard
import              Handler.Deck
-----------------------------------
import              Application

app :: SnapletInit App App
app = makeSnaplet "snapplejack" "The SnappleJack Web Server" Nothing $ do
    hs <- nestSnaplet "heist" heist $ heistInit ""
    addRoutes routes
    return $ App hs
  where
        routes :: [(ByteString, Handler App App ())]
        routes = [ ("/card", cardHandler)
                 , ("/deck", deckHandler)
                 , ("/res", serveDirectory "res")
                 , ("", cardServe blankCardFilter)
                 ]

cardHandler :: Handler App App ()
cardHandler = do
    params <- getParams
    let filter = parseCardFilter params
    cardServe filter

deckHandler :: Handler App App ()
deckHandler = do
    params <- getParams
    let filter = parseDeckFilter params
    deckServe filter

cardServe :: Filter -> Handler App App ()
cardServe = renderWithSplices "card" . cfSplice

deckServe :: Filter -> Handler App App ()
deckServe = renderWithSplices "deck" . dfSplice
