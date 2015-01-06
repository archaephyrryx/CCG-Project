{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Main where

import              Control.Lens.TH
import              Data.IORef
import              Control.Applicative
import              Data.Maybe
---------------------------------------------
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
import              Site
import              Application
import              Snaplets

main :: IO ()
main = serveSnaplet defaultConfig app
