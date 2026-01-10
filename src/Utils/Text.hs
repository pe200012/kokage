{-# LANGUAGE OverloadedStrings #-}

-- | Text utility functions for parsing and processing.
module Utils.Text
  ( readBoolOr
  , readMaybeBool
  , clean
  , cleanStr
  , readIntOr
  , readMaybeInt
  ) where

import           Data.Char  ( isSpace )
import qualified Data.Text  as T

-- | Trim leading and trailing whitespace (String version for charset detection).
cleanStr :: String -> String
cleanStr = dropWhile isSpace . dropWhileEnd isSpace
  where
    dropWhileEnd p = foldr (\x acc -> if p x && null acc
                              then []
                              else x : acc) []

-- | Trim leading and trailing whitespace (Text version).
clean :: Text -> Text
clean = T.strip

-- | Read an Int with a default value if parsing fails.
readIntOr :: Int -> Text -> Int
readIntOr def val = fromMaybe def (readMaybe (T.unpack val))

-- | Read a Maybe Int from Text.
readMaybeInt :: Text -> Maybe Int
readMaybeInt = readMaybe . T.unpack

-- | Read a Bool with a default value if parsing fails.
readBoolOr :: Bool -> Text -> Bool
readBoolOr def val = case T.toLower val of
  "true"  -> True
  "1"     -> True
  "false" -> False
  "0"     -> False
  _       -> def

-- | Read a Maybe Bool from Text.
readMaybeBool :: Text -> Maybe Bool
readMaybeBool val = case T.toLower val of
  "true"  -> Just True
  "1"     -> Just True
  "false" -> Just False
  "0"     -> Just False
  _       -> Nothing
