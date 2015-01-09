{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances, ViewPatterns, QuasiQuotes, OverloadedStrings #-}

module Reformation (inputMin, inputMax) where

import HSP
import qualified Text.Reform.Generalized as G
import Text.Reform.Backend
import Text.Reform
import Data.Text.Lazy             (Text)
import Language.Haskell.HSX.QQ    (hsx)
import Cards.Common.Hint
import Control.Applicative


customInput :: (Monad m, StringType x ~ Text, FormError error, FormInput input, XMLGenerator x, ErrorInputType error ~ input, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text y)) => (FormId -> y -> [XMLGenT x (XMLType x)]) -> (input -> Either error y) -> y -> Form m input error [XMLGenT x (XMLType x)] () y
customInput f getField initialValue = G.input getField f initialValue

inputNumber :: (Monad m
               , FormError error, FormInput input
               , ErrorInputType error ~ input
               , XMLGenerator x, StringType x ~ Text
               , EmbedAsAttr x (Attr Text FormId)
               , EmbedAsAttr x (Attr Text number)
               , Hint number) => number
                              -> Form m input error [XMLGenT x (XMLType x)] () number

inputNumber = customInput inputField readHError
  where
    inputField :: (XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text number), Hint number) => (FormId -> number -> [XMLGenT x (XMLType x)])
    inputField i a = [ [hsx| <input type="number" min="0" step="1" id=i name=i pattern="[0-9]+" value=a /> |] ]

inputMin :: (Monad m
            , FormError error, FormInput input
            , ErrorInputType error ~ input
            , XMLGenerator x, StringType x ~ Text
            , EmbedAsAttr x (Attr Text FormId)
            , EmbedAsAttr x (Attr Text number)
            , Hint number) => number
                           -> Form m input error [XMLGenT x (XMLType x)] () number

inputMin = customInput inputField readHError
  where
    inputField :: (XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text number), Hint number) => (FormId -> number -> [XMLGenT x (XMLType x)])
    inputField i a = [ [hsx| <input type="number" placeholder="Min" min="0" step="1" id=i name=i pattern="[0-9]+" value=a />|] ]

inputMax :: (Monad m
            , FormError error, FormInput input
            , ErrorInputType error ~ input
            , XMLGenerator x, StringType x ~ Text
            , EmbedAsAttr x (Attr Text FormId)
            , EmbedAsAttr x (Attr Text number)
            , Hint number) => number
                           -> Form m input error [XMLGenT x (XMLType x)] () number

inputMax = customInput inputField readHError
  where
    inputField :: (XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text number), Hint number) => (FormId -> number -> [XMLGenT x (XMLType x)])
    inputField i a = [ [hsx|<input type="number" placeholer="Max" min="0" step="1" id=i name=i pattern="[0-9]+" value=a />|] ]

readHError :: (FormError error, FormInput input, ErrorInputType error ~ input, Hint number) => input -> Either error number
readHError i = readH <$> (getInputString i)
