{-# LANGUAGE RecordWildCards #-}

module CCG.Cards.Parser.Fields (
        OrderedField(..)
      , wrapFields
      , FieldWrapper(..)
      , getField
  )
  where
import Data.List
import Util.List

-- | Data type used for column-order book-keeping based on a pre-split header
-- string
newtype OrderedField = OrderedField { flds :: [String] }

-- | Key- and OrderedField-based generator of all functions from a
-- field-split list to the values in the corresponding column, presented
-- in the same order as the columns of that name
getField :: String -> OrderedField -> [([String] -> String)]
getField x y = map (flip (!!)) (elemIndices x (flds y))

-- |"FieldWrapper" is a datatype highly specific to the MLP CCG card
-- metadata, to a lesser extent dependent on the format of the card-text OCR used
-- in this project.
data FieldWrapper =
     FieldWrapper
     { nam   :: String -- ^ Card name (all cards)
     , set'  :: String -- ^ Card set (all cards)
     , num'  :: String -- ^ Card number (all cards)
     , rar'  :: String -- ^ Card rarity (all cards)
     , typ   :: String -- ^ Card type (all cards)
     , key   :: String -- ^ Card keyword-string (all cards)
     , col   :: String -- ^ Card color (manes/friends/resources/events only)
     , cos   :: String -- ^ Card cost (friends/resources/events only)
     , req   :: String -- ^ Card play requirement (friends/resources/events only)
     , pow   :: String -- ^ Card power (non-problems only)
     , boo   :: String -- ^ Card boosted power (Mane only)
     , poi   :: String -- ^ Card points (troublemakers/problems only)
     , preq1 :: String -- ^ Problem Requirement power 1 (problems only)
     , col1  :: String -- ^ Problem Requirement color 1 (problems only)
     , preq2 :: String -- ^ Problem Requirement power 2 (problems only)
     , col2  :: String -- ^ Problem Requirement color 2 (problems only)
     , opreq :: String -- ^ Opponent problem requirement power (problems only)
     , text' :: String -- ^ Card text (all cards)
     }
     deriving (Show)

-- |Given an "OrderedField" generated from a header line, matches up keys
-- to values to wrap card fields in a general and user-friendly
-- "FieldWrapper"
wrapFields :: OrderedField -> [String] -> FieldWrapper
wrapFields ordfs fs =
  let getf h x = (h $ getField x ordfs) $ fs
      nam   = head`getf`"Name"
      set'  = head`getf`"Set"
      num'  = head`getf`"Number"
      rar'  = head`getf`"Rarity"
      typ   = head`getf`"Type"
      key   = head`getf`"Keywords"
      col   = head`getf`"Color"
      cos   = head`getf`"Cost"
      req   = head`getf`"Req"
      pow   = head`getf`"Power"
      boo   = head`getf`"Boosted"
      poi   = head`getf`"Points"
      preq1 = head`getf`"Problem Req"
      col1  = neck`getf`"Color"
      preq2 = neck`getf`"Problem Req"
      col2  = body`getf`"Color"
      opreq = head`getf`"Opponent Req"
      text' = head`getf`"Text"
  in FieldWrapper{..}
