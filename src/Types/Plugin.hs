{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Plugin descript.txt types and parsing
-- Reference: https://ssp.shillest.net/ukadoc/manual/descript_plugin.html
module Types.Plugin
  ( -- * Data Types
    PluginDescript(..)
  , OtherGhostTalkOption(..)
    -- * Defaults
  , emptyPluginDescript
    -- * Parsing
  , readPluginDescript
  ) where

import           Data.Text                  ( Text )
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.ByteString.Lazy       as BL

import           Types.Ghost                ( detectCharsetFromBytes
                                            , convertToUtf8
                                            , clean
                                            , readIntOr
                                            )

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

-- | OnOtherGhostTalk notification option
data OtherGhostTalkOption
  = OtherGhostTalkDisabled    -- ^ false/0 - Don't notify
  | OtherGhostTalkAfter       -- ^ true/1/after - Notify immediately after speaking
  | OtherGhostTalkBefore      -- ^ before - Notify just before speaking
  deriving ( Show, Eq )

-- | Plugin descriptor containing metadata and settings
data PluginDescript
  = PluginDescript
  { -- Basic info (required fields)
    pdCharset :: Text                     -- ^ Character encoding
  , pdName :: Text                     -- ^ Plugin name (required)
  , pdId :: Text                     -- ^ Plugin ID/GUID (required, max 63 bytes ASCII)
  , pdFilename :: Text                     -- ^ DLL filename (required)
    -- Optional metadata
  , pdType :: Text                     -- ^ File set type ("plugin")
  , pdCraftman :: Maybe Text               -- ^ Author name (ASCII only)
  , pdCraftmanw :: Maybe Text               -- ^ Author name (unicode allowed)
  , pdCraftmanUrl :: Maybe Text               -- ^ Author's URL
  , pdHomeUrl :: Maybe Text               -- ^ Network update URL
  , pdReadme :: Text                     -- ^ Readme filename (default: readme.txt)
  , pdReadmeCharset :: Maybe Text               -- ^ Readme charset
    -- Plugin behavior settings
  , pdSecondChangeInterval :: Int        -- ^ OnSecondChange interval in seconds (default: 1, 0 = disabled)
  , pdOtherGhostTalk :: OtherGhostTalkOption  -- ^ OnOtherGhostTalk notification setting
  }
  deriving ( Show, Eq )

--------------------------------------------------------------------------------
-- Defaults
--------------------------------------------------------------------------------

-- | Empty PluginDescript with default values
emptyPluginDescript :: PluginDescript
emptyPluginDescript
  = PluginDescript
  { pdCharset = ""
  , pdName = ""
  , pdId = ""
  , pdFilename = ""
  , pdType = "plugin"
  , pdCraftman = Nothing
  , pdCraftmanw = Nothing
  , pdCraftmanUrl = Nothing
  , pdHomeUrl = Nothing
  , pdReadme = "readme.txt"
  , pdReadmeCharset = Nothing
  , pdSecondChangeInterval = 1
  , pdOtherGhostTalk = OtherGhostTalkDisabled
  }

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- | Parse OtherGhostTalk option from text
parseOtherGhostTalk :: Text -> OtherGhostTalkOption
parseOtherGhostTalk val
  | val == "false" || val == "0" = OtherGhostTalkDisabled
  | val == "true" || val == "1" || val == "after" = OtherGhostTalkAfter
  | val == "before" = OtherGhostTalkBefore
  | otherwise = OtherGhostTalkDisabled

-- | Read and parse a plugin descript.txt file
readPluginDescript :: FilePath -> IO PluginDescript
readPluginDescript path = do
  rawBytes <- BL.readFile path
  let detectedCharset = detectCharsetFromBytes rawBytes
      utf8Bytes       = convertToUtf8 detectedCharset rawBytes
      contents        = TE.decodeUtf8 (BL.toStrict utf8Bytes)
  return $ foldl' parseLine emptyPluginDescript (T.lines contents)
  where
    parseLine :: PluginDescript -> Text -> PluginDescript
    parseLine pd line = case T.breakOn "," line of
      ( rawKey, rest )
        | not (T.null rest) -> let
            key = T.toLower (clean rawKey)
            val = clean (T.drop 1 rest)
          in
            parseKey pd key val
      _ -> pd

    parseKey :: PluginDescript -> Text -> Text -> PluginDescript
    parseKey pd key val
      -- Basic info
      | key == "charset" = pd { pdCharset = val }
      | key == "name" = pd { pdName = val }
      | key == "id" = pd { pdId = val }
      | key == "filename" = pd { pdFilename = val }
      | key == "type" = pd { pdType = val }
      -- Author info
      | key == "craftman" = pd { pdCraftman = Just val }
      | key == "craftmanw" = pd { pdCraftmanw = Just val }
      | key == "craftmanurl" = pd { pdCraftmanUrl = Just val }
      | key == "homeurl" = pd { pdHomeUrl = Just val }
      -- Readme
      | key == "readme" = pd { pdReadme = val }
      | key == "readme.charset" = pd { pdReadmeCharset = Just val }
      -- Plugin behavior
      | key == "secondchangeinterval" = pd { pdSecondChangeInterval = readIntOr 1 val }
      | key == "otherghosttalk" = pd { pdOtherGhostTalk = parseOtherGhostTalk val }
      | otherwise = pd
