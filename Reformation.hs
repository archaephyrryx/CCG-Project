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

-- |A highly general Reform input element that can be used for
-- templating arbitrary types of Html input fields
customInput :: ( Monad m, StringType x ~ Text
                , FormError error, FormInput input
                , XMLGenerator x, ErrorInputType error ~ input
                , EmbedAsAttr x (Attr Text FormId)
                , EmbedAsAttr x (Attr Text y))
            => (FormId -> y -> [XMLGenT x (XMLType x)]) -- ^ A function that, given a form id and a value of type y, produces a list of XML/HTML content
            -> (input -> Either error y) -- ^ A function that parses input into a Right y or a FormError
            -> y -- ^ A default initial value for the input field to hold
            -> Form m input error [XMLGenT x (XMLType x)] () y -- ^ A Formlet that is compatible with the Reform-native formlets (e.g. inputText)
customInput f getField initialValue = G.input getField f initialValue

inputNumber :: (Monad m
               , FormError error, FormInput input
               , ErrorInputType error ~ input
               , XMLGenerator x, StringType x ~ Text
               , EmbedAsAttr x (Attr Text FormId)
               , EmbedAsAttr x (Attr Text number)
               , Hint number) => number -- ^ A default initial numeric value that has an instance of the Hint typeclass
                              -> Form m input error [XMLGenT x (XMLType x)] () number -- ^ A Form returning a value of that type

-- ^A generic but concrete application of 'customInput' that permits
-- non-negative numbers as input and yields a Hint instance as output
inputNumber = customInput inputField readHError
  where
    inputField :: (XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text number), Hint number) => (FormId -> number -> [XMLGenT x (XMLType x)])
    inputField i a = [ [hsx| <input type="number" min="0" step="1" id=i name=i pattern="[0-9]+" value=a /> |] ]

-- | A modified special case of inputNumber that is given a placeholer
-- of 'Min'
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

-- | A modified special case of inputNumber that is given a placeholder
-- of 'Max'
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
    inputField i a = [ [hsx|<input type="number" placeholder="Max" min="0" step="1" id=i name=i pattern="[0-9]+" value=a />|] ]

-- | 'readHError' takes FormInput that is supposed to be converted into
-- a Hint instance, returning a Left error if no conversion is possible,
-- and a Right Hint-value if successful
readHError :: (FormError error, FormInput input, ErrorInputType error ~ input, Hint number) => input -> Either error number
readHError i = readH <$> (getInputString i)
