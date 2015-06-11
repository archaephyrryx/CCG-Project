{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, ViewPatterns #-}

module Widgets.Minmax where

import CCG hiding (name, set)
import Control.Applicative
import Data.Maybe
import Formal
import Util
import Renderer.Core
import Text.Reform
import Text.Reform.Backend
import qualified Text.Reform.Generalized as G
import Prelude hiding (span, div, min)

data MinMax = MinMax
  String -- ^ label
  String -- ^ title
  String -- ^ minname
  String -- ^ maxname

minmax :: Renderer' MinMax
minmax (MinMax l t n x) = collect
  [ label # set for_ l #$ string t
  , div # set id_ l #+
    [ input #
        set name n #
        set placeholder "Min" #
        set type_ "number" #
        set min "0" #
        set step "1" #
        set pattern "[0-9]+" #
        zap
    , span #: string "to"
    , input #
        set name x #
        set placeholder "Max" #
        set type_ "number" #
        set min "0" #
        set step "1" #
        set pattern "[0-9]+" #
        zap
    ]
  ]

customInput :: (Monad m, FormError error, FormInput input, ErrorInputType error ~ input)
            => (FormId -> (Maybe y) -> Rendered) -> (input -> Either error (Maybe y)) -> (Maybe y)
            -> Form m input error Rendered () (Maybe y)
customInput f getField initialValue = G.input getField f initialValue

-- |A generic but concrete application of 'customInput' that permits
-- non-negative numbers as input and yields a Hint instance as output
inputNumber, inputMin, inputMax ::
       (Monad m, FormError error, FormInput input, ErrorInputType error ~ input, Hint number)
    => (Maybe number) -> Form m input error Rendered () (Maybe number)


inputNumber = customInput ((zap.).inputField) readMaybeHintError
inputMin = customInput (((zap.(set placeholder "Min")).).inputField) readMaybeHintError
inputMax = customInput (((zap.(set placeholder "Max")).).inputField) readMaybeHintError


inputField :: (Hint number) => FormId -> (Maybe number) -> Vacuum
inputField i m = inputField' # set id_ (show i) # set name (show i) # (set value?(val<$>m))

inputField' :: Vacuum
inputField' = input # set type_ "number" # set min "0" # set step "1" # set pattern "[0-9]+"

readMaybeHintError :: (FormError error, FormInput input, ErrorInputType error ~ input, Hint number) => input -> Either error (Maybe number)
readMaybeHintError i = readMaybeH <$> (getInputString i)
