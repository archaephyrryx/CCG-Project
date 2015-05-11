{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
    DeriveDataTypeable, GeneralizedNewtypeDeriving,
	RecordWildCards, TemplateHaskell, TypeFamilies,
	OverloadedStrings #-}

module Filter where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.IORef
import Data.List
import Data.IxSet
import Data.Data (Data, Typeable)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Map (Map)
import Text.Digestive
import Text.Digestive.Heist
import Text.Digestive.Snap
import           Snap.Core (writeText)
import           Snap.Snaplet
import           Snap.Snaplet.Heist (heistLocal, render)
import           Application
---------------------------------------------

data Query = Query { color :: Color } deriving (Show)

isNotEmpty :: T.Text -> Bool
isNotEmpty = not . T.null

--newtype Type = Type !String
type Color = T.Text
--newtype Name = Name !String

colErrMsg :: T.Text
colErrMsg = "Color cannot be empty"

queryForm :: (Monad m) => Form T.Text m Query
queryForm = Query
    <$> "color" .: check colErrMsg isNotEmpty (text Nothing)

queryFormHandler :: Handler App App ()
queryFormHandler = do
  (view, result) <- runForm "query" queryForm
  case result of
    Just x  -> writeText $ T.pack $ show x
    Nothing -> heistLocal (bindDigestiveSplices view) $ render "queryForm"
