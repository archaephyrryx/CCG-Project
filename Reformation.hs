{-# LANGUAGE FlexibleContexts, FlexibleInstances,
   MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies,
   UndecidableInstances, ViewPatterns, OverloadedStrings #-}

module Reformation (inputMin, inputMax) where

import qualified Text.Reform.Generalized as G
import Text.Reform.Backend
import Text.Reform
import CCG.Cards.Common.Instances (Hint)
import Control.Applicative
import Data.Maybe
import Text.Blaze ((!))
import Text.Blaze.Html5 (Html, toValue, ToValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

instance ToValue FormId where
    toValue fid = toValue (show fid)

customInput :: (Monad m, FormError error, FormInput input , ToValue y, ErrorInputType error ~ input)
            => (FormId -> (Maybe y) -> Html)
            -> (input -> Either error (Maybe y))
            -> (Maybe y) -> Form m input error Html () (Maybe y)
customInput f getField initialValue = G.input getField f initialValue


inputNumber :: (Monad m , FormError error, FormInput input , ErrorInputType error ~ input , ToValue number , Hint number) => (Maybe number) -> Form m input error Html () (Maybe number)
-- ^A generic but concrete application of 'customInput' that permits
-- non-negative numbers as input and yields a Hint instance as output
inputNumber = customInput inputField readMaybeHintError
  where
    inputField :: (ToValue number, Hint number) => (FormId -> (Maybe number) -> Html)
    inputField i (Just a) = H.input ! A.type_ "number" ! A.min "0"
                                    ! A.step "1" ! A.pattern "[0-9]+"
                                    ! A.id (toValue i) ! A.name (toValue i) ! A.value (toValue a)
    inputField i Nothing = H.input ! A.type_ "number" ! A.min "0"
                                   ! A.step "1" ! A.pattern "[0-9]+"
                                   ! A.id (toValue i) ! A.name (toValue i)

inputMin = customInput inputField readMaybeHintError
  where
    inputField :: (ToValue number, Hint number) => (FormId -> (Maybe number) -> Html)
    inputField i (Just a) = H.input ! A.type_ "number" ! A.min "0"
                                    ! A.step "1" ! A.pattern "[0-9]+"
                                    ! A.id (toValue i) ! A.name (toValue i) ! A.value (toValue a)
                                    ! A.placeholder "Min"
    inputField i Nothing = H.input ! A.type_ "number" ! A.min "0"
                                   ! A.step "1" ! A.pattern "[0-9]+"
                                   ! A.id (toValue i) ! A.name (toValue i)
                                   ! A.placeholder "Min"

inputMax :: (Monad m , FormError error, FormInput input , ErrorInputType error ~ input , ToValue number , Hint number) => (Maybe number) -> Form m input error Html () (Maybe number)
inputMax = customInput inputField readMaybeHintError
  where
    inputField :: (ToValue number, Hint number) => (FormId -> (Maybe number) -> Html)
    inputField i (Just a) = H.input ! A.type_ "number" ! A.min "0"
                                    ! A.step "1" ! A.pattern "[0-9]+"
                                    ! A.id (toValue i) ! A.name (toValue i) ! A.value (toValue a)
                                    ! A.placeholder "Max"
    inputField i Nothing = H.input ! A.type_ "number" ! A.min "0"
                                   ! A.step "1" ! A.pattern "[0-9]+"
                                   ! A.id (toValue i) ! A.name (toValue i)
                                   ! A.placeholder "Max"

--
-- | 'readMaybeHintError' takes FormInput that is supposed to be converted into
-- a Hint instance, returning a Left error if no conversion is possible,
-- and a Right Maybe-Hint-value if successful
readMaybeHintError :: (FormError error, FormInput input, ErrorInputType error ~ input, Hint number) => input -> Either error (Maybe number)
readMaybeHintError i = readMaybeH <$> (getInputString i)
