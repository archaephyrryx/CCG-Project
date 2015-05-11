{-# LANGUAGE OverloadedStrings #-}

module Site ( app ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.IORef
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
import           Filter
------------------------------------------------------------------------------
import           Application
import           Snaplets


------------------------------------------------------------------------------
-- | The application's routes.

app :: SnapletInit App App
app = makeSnaplet "snapplejack" "The SnappleJack Web Server" Nothing $ do
    hs <- nestSnaplet "heist" heist $ heistInit ""
    fs <- nestSnaplet "foo" foo $ fooInit
    bs <- nestSnaplet "bar" bar $ nameSnaplet "newname" $ barInit foo
    addRoutes routes
    ref <- liftIO $ newIORef "snapplejack"
    return $ App hs fs bs ref
    where
        routes :: [(ByteString, Handler App App ())]
        routes = [ ("/card", cardHandler)
                 , ("/deck", deckHandler)
                 , ("/res", serveDirectory "../res")
                 , ("", heistServe)
                 ]
