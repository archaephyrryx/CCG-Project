{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import              Control.Lens.TH
import              Data.IORef
import              Control.Applicative
import              Data.Maybe
import              Control.Monad
import              Control.Monad.State.Class
import              Control.Monad.IO.Class
---------------------------------------------
import qualified    Data.ByteString.Char8 as B
import qualified    Data.Text as T
import              Snap.Core
import              Snap.Snaplet
import              Snap.Snaplet.Heist
import              Snap.Util.FileServe
import              Snap.Http.Server.Config
import              Heist
import qualified    Heist.Interpreted as I


data Foo = Foo
data Bar = Bar

fooInit :: SnapletInit b Foo
fooInit = makeSnaplet "foo" "Foo snaplet" Nothing $ do
    return Foo

barInit :: SnapletLens b Foo -> SnapletInit b Bar
barInit h = makeSnaplet "bar" "Bar snaplet" Nothing $ do
    return Bar

data App = App
    { _heist    :: Snaplet (Heist App)
    , _foo      :: Snaplet Foo
    , _bar      :: Snaplet Bar
    , _sitename :: IORef B.ByteString
    }

makeLenses ''App

instance HasHeist App where heistLens = subSnaplet heist

appInit :: SnapletInit App App
appInit = makeSnaplet "snapplejack" "The SnappleJack Web Server" Nothing $ do
    hs <- nestSnaplet "" heist $ heistInit "."
    fs <- nestSnaplet "foo" foo $ fooInit
    bs <- nestSnaplet "bar" bar $ nameSnaplet "newname" $ barInit foo
    addRoutes [ ("/hello", writeText "hello world")
              , ("/fooname", with foo namePage)
              , ("/barname", with bar namePage)
              , ("/company", siteHandler)
              , ("/res", serveDirectory "res")
              ]
    wrapSite (<|> heistServe)
    ref <- liftIO $ newIORef "snapplejack"
    return $ App hs fs bs ref

namePage :: Handler b v ()
namePage = do
    mname <- getSnapletName
    writeText $ fromMaybe "This shouldn't happen" mname


siteHandler :: Handler App App ()
siteHandler = method GET getter <|> method POST setter
    where
        getter = do
            nameRef <- gets _sitename
            name <- liftIO $ readIORef nameRef
            writeBS name
        setter = do
            mname <- getParam "name"
            nameRef <- gets _sitename
            liftIO $ maybe (return ()) (writeIORef nameRef) mname
            getter


main :: IO ()
main = serveSnaplet defaultConfig appInit


{-
main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
           writeBS param
    heistState <- eitherT (putStrLn . unlines) return $ do
        heist <- initHeist mempty
            { hcTemplateLocations = [ loadTemplates "heists" ]
            , hcInterpretedSplices = defaultInterpretedSplices
            }
        Just (output, _) <- renderTemplate heist "card"
        liftIO . BS.putStrLn . toByteString $ output
    simpleHTTP nullConf $ msum
        [ dir "heists" $ heistServe heistState
        , nullDir >>
          seeOther "/heists/card" (toResponse "/heists/card")
        ]
-}
