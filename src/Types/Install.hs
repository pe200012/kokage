{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | NAR installation types and parsing
-- Reference: https://ssp.shillest.net/ukadoc/manual/install.html
module Types.Install
  ( -- * Data Types
    InstallType(..)
  , InstallDescript(..)
  , DeleteList
  , DeveloperOption(..)
  , DeveloperOptions
    -- * Defaults
  , emptyInstallDescript
    -- * Parsing
  , parseInstallType
  , readInstallDescript
  , readDeleteList
  , readDeveloperOptions
  ) where

import           Data.Text                  ( Text )
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Map.Strict            as Map
import           Data.Map.Strict            ( Map )

import           Types.Ghost                ( detectCharsetFromBytes
                                            , convertToUtf8
                                            , clean
                                            )

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

-- | Type of content being installed from a NAR archive
data InstallType
  = InstallGhost           -- ^ ghost - Ghost with AI/personality
  | InstallShell           -- ^ shell - Shell/skin for a ghost
  | InstallSupplement      -- ^ supplement - Additional files for existing ghost
  | InstallBalloon         -- ^ balloon - Speech balloon style
  | InstallPlugin          -- ^ plugin - SAORI plugin
  | InstallHeadline        -- ^ headline - News headline sensor
  | InstallCalendar        -- ^ calendar - Calendar skin
  | InstallCalendarSkin    -- ^ calendarskin - Calendar skin (alt name)
  | InstallPackage         -- ^ package - Multiple items bundled
  | InstallUnknown Text    -- ^ Unknown type
  deriving ( Show, Eq )

-- | Installation descriptor from install.txt
data InstallDescript
  = InstallDescript
  { -- Required fields
    idCharset :: Text                          -- ^ Character encoding
  , idName :: Text                             -- ^ Object name (required)
  , idType :: InstallType                      -- ^ Content type (required)
  , idDirectory :: Text                        -- ^ Installation directory name
    -- Optional fields
  , idAccept :: Maybe Text                     -- ^ Target ghost name (for shell/supplement)
  , idRefresh :: Bool                          -- ^ Clear directory before install (default: False)
  , idRefreshUndeleteMask :: [Text]            -- ^ Files to keep during refresh
    -- Bundled content directories
  , idBalloonDirectory :: Maybe Text           -- ^ Bundled balloon directory
  , idBalloonSourceDirectory :: Maybe Text     -- ^ Source dir in archive for balloon
  , idPluginDirectory :: Maybe Text            -- ^ Bundled plugin directory
  , idPluginSourceDirectory :: Maybe Text      -- ^ Source dir in archive for plugin
  , idHeadlineDirectory :: Maybe Text          -- ^ Bundled headline directory
  , idHeadlineSourceDirectory :: Maybe Text    -- ^ Source dir in archive for headline
  , idCalendarDirectory :: Maybe Text          -- ^ Bundled calendar directory
  , idCalendarSourceDirectory :: Maybe Text    -- ^ Source dir in archive for calendar
  , idCalendarSkinDirectory :: Maybe Text      -- ^ Bundled calendar skin directory
  , idCalendarSkinSourceDirectory :: Maybe Text -- ^ Source dir in archive for calendar skin
    -- Raw key-value store for extensions
  , idRaw :: Map Text Text                     -- ^ All parsed key-value pairs
  }
  deriving ( Show, Eq )

-- | List of files/directories to delete on network update
type DeleteList = [Text]

-- | Developer option flags for archive generation
data DeveloperOption
  = NoNar      -- ^ Exclude from NAR archive
  | NoUpdate   -- ^ Exclude from network update archive
  deriving ( Show, Eq )

-- | Developer options mapping paths to their options
type DeveloperOptions = Map Text [DeveloperOption]

--------------------------------------------------------------------------------
-- Defaults
--------------------------------------------------------------------------------

-- | Empty InstallDescript with default values
emptyInstallDescript :: InstallDescript
emptyInstallDescript
  = InstallDescript
  { idCharset = ""
  , idName = ""
  , idType = InstallGhost
  , idDirectory = ""
  , idAccept = Nothing
  , idRefresh = False
  , idRefreshUndeleteMask = []
  , idBalloonDirectory = Nothing
  , idBalloonSourceDirectory = Nothing
  , idPluginDirectory = Nothing
  , idPluginSourceDirectory = Nothing
  , idHeadlineDirectory = Nothing
  , idHeadlineSourceDirectory = Nothing
  , idCalendarDirectory = Nothing
  , idCalendarSourceDirectory = Nothing
  , idCalendarSkinDirectory = Nothing
  , idCalendarSkinSourceDirectory = Nothing
  , idRaw = Map.empty
  }

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- | Parse install type from text
parseInstallType :: Text -> InstallType
parseInstallType t = case T.toLower (T.strip t) of
  "ghost"        -> InstallGhost
  "shell"        -> InstallShell
  "supplement"   -> InstallSupplement
  "balloon"      -> InstallBalloon
  "plugin"       -> InstallPlugin
  "headline"     -> InstallHeadline
  "calendar"     -> InstallCalendar
  "calendarskin" -> InstallCalendarSkin
  "package"      -> InstallPackage
  other          -> InstallUnknown other

-- | Read and parse an install.txt file
readInstallDescript :: FilePath -> IO InstallDescript
readInstallDescript path = do
  rawBytes <- BL.readFile path
  let detectedCharset = detectCharsetFromBytes rawBytes
      utf8Bytes       = convertToUtf8 detectedCharset rawBytes
      contents        = TE.decodeUtf8 (BL.toStrict utf8Bytes)
  return $ foldl' parseLine emptyInstallDescript (T.lines contents)
  where
    parseLine :: InstallDescript -> Text -> InstallDescript
    parseLine inst line = case T.breakOn "," line of
      ( rawKey, rest )
        | not (T.null rest) -> let
            key = T.toLower (clean rawKey)
            val = clean (T.drop 1 rest)
            rawMap = Map.insert key val (idRaw inst)
          in
            parseKey (inst { idRaw = rawMap }) key val
      _ -> inst

    parseKey :: InstallDescript -> Text -> Text -> InstallDescript
    parseKey inst key val
      -- Required fields
      | key == "charset" = inst { idCharset = val }
      | key == "name" = inst { idName = val }
      | key == "type" = inst { idType = parseInstallType val }
      | key == "directory" = inst { idDirectory = val }
      -- Optional fields
      | key == "accept" = inst { idAccept = Just val }
      | key == "refresh" = inst { idRefresh = val == "1" || T.toLower val == "true" }
      | key == "refreshundeletemask" = inst { idRefreshUndeleteMask = T.splitOn ":" val }
      -- Bundled balloon
      | key == "balloon.directory" = inst { idBalloonDirectory = Just val }
      | key == "balloon.source.directory" = inst { idBalloonSourceDirectory = Just val }
      -- Bundled plugin
      | key == "plugin.directory" = inst { idPluginDirectory = Just val }
      | key == "plugin.source.directory" = inst { idPluginSourceDirectory = Just val }
      -- Bundled headline
      | key == "headline.directory" = inst { idHeadlineDirectory = Just val }
      | key == "headline.source.directory" = inst { idHeadlineSourceDirectory = Just val }
      -- Bundled calendar
      | key == "calendar.directory" = inst { idCalendarDirectory = Just val }
      | key == "calendar.source.directory" = inst { idCalendarSourceDirectory = Just val }
      -- Bundled calendar skin
      | key == "calendarskin.directory" = inst { idCalendarSkinDirectory = Just val }
      | key == "calendarskin.source.directory" = inst { idCalendarSkinSourceDirectory = Just val }
      | otherwise = inst

-- | Read and parse a delete.txt file
-- Format: one relative path per line, backslash-separated, directories end with \
readDeleteList :: FilePath -> IO DeleteList
readDeleteList path = do
  rawBytes <- BL.readFile path
  let detectedCharset = detectCharsetFromBytes rawBytes
      utf8Bytes       = convertToUtf8 detectedCharset rawBytes
      contents        = TE.decodeUtf8 (BL.toStrict utf8Bytes)
      -- Normalize backslashes to forward slashes for cross-platform
      normalize = T.replace "\\" "/"
  return $ filter (not . T.null) $ map (normalize . T.strip) (T.lines contents)

-- | Read and parse a developer_options.txt file
-- Format: relative/path,option1,option2,...
-- Options: nonar, noupdate
readDeveloperOptions :: FilePath -> IO DeveloperOptions
readDeveloperOptions path = do
  rawBytes <- BL.readFile path
  -- developer_options.txt is typically UTF-8
  let contents = TE.decodeUtf8 (BL.toStrict rawBytes)
  return $ foldl' parseLine Map.empty (T.lines contents)
  where
    parseLine :: DeveloperOptions -> Text -> DeveloperOptions
    parseLine opts line =
      let parts = T.splitOn "," (T.strip line)
      in case parts of
        (filePath : optStrs) | not (T.null filePath) ->
          let options = mapMaybe parseOption optStrs
          in if null options
             then opts
             else Map.insert filePath options opts
        _ -> opts

    parseOption :: Text -> Maybe DeveloperOption
    parseOption opt = case T.toLower (T.strip opt) of
      "nonar"    -> Just NoNar
      "noupdate" -> Just NoUpdate
      _          -> Nothing

    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe _ [] = []
    mapMaybe f (x:xs) = case f x of
      Just y  -> y : mapMaybe f xs
      Nothing -> mapMaybe f xs
