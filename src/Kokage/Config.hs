{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Configuration types for Kokage application.
module Kokage.Config
  ( -- * Application configuration
    KokageConfig(..)
  , defaultConfig
    -- * Base directory
  , BaseDir(..)
  , getDefaultBaseDir
    -- * Ghost history
  , GhostHistory(..)
  , defaultGhostHistory
  , historyFilePath
  , loadGhostHistory
  , saveGhostHistory
  , parseHistory
  , isFirstBoot
  ) where

import           Control.Exception ( SomeException, try )

import           Data.Maybe        ( fromMaybe )
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO

import           System.Directory  ( XdgDirectory(..)
                                   , createDirectoryIfMissing
                                   , doesFileExist
                                   , getXdgDirectory
                                   )
import           System.FilePath   ( (</>), takeDirectory )

-- | Base directory for ghost/balloon/shell resources.
newtype BaseDir = BaseDir { unBaseDir :: FilePath }
  deriving ( Show, Eq )

-- | Configuration for the Kokage application.
data KokageConfig
  = KokageConfig
  { kcGhostDir   :: !BaseDir        -- ^ Directory containing ghost subdirectories
  , kcBalloonDir :: !BaseDir        -- ^ Directory containing balloon resources
  , kcGhostPath  :: !(Maybe FilePath)  -- ^ Specific ghost path to load (overrides default)
  , kcDebug      :: !Bool           -- ^ Enable debug logging
  }
  deriving ( Show, Eq )

-- | Default configuration.
-- Uses XDG data directory for ghosts and balloons.
defaultConfig :: IO KokageConfig
defaultConfig = do
  baseDir <- getDefaultBaseDir
  return
    KokageConfig
    { kcGhostDir = baseDir, kcBalloonDir = baseDir, kcGhostPath = Nothing, kcDebug = False }

-- | Get the default base directories for NAR installation.
-- Uses ~/.local/share/ukagaka/ on Linux.
getDefaultBaseDir :: IO BaseDir
getDefaultBaseDir = do
  dataDir <- getXdgDirectory XdgData "ukagaka"
  createDirectoryIfMissing True dataDir
  return $ BaseDir dataDir

-- | Ghost history data stored in HISTORY file.
data GhostHistory
  = GhostHistory { ghBootCount    :: !Int      -- ^ Number of times ghost has been booted
                 , ghTotalRuntime :: !Int      -- ^ Total runtime in seconds
                 , ghLastBoot     :: !T.Text   -- ^ Last boot timestamp
                 }
  deriving ( Show, Eq )

-- | Default history for a new ghost.
defaultGhostHistory :: GhostHistory
defaultGhostHistory = GhostHistory 0 0 ""

-- | Path to the HISTORY file for a ghost.
historyFilePath :: FilePath -> FilePath
historyFilePath ghostPath = ghostPath </> "ghost" </> "master" </> "HISTORY"

-- | Load ghost history from HISTORY file.
loadGhostHistory :: FilePath -> IO (Maybe GhostHistory)
loadGhostHistory ghostPath = do
  let path = historyFilePath ghostPath
  exists <- doesFileExist path
  if exists
    then do
      result <- try (TIO.readFile path) :: IO (Either SomeException T.Text)
      case result of
        Left _        -> return Nothing
        Right content -> return $ Just $ parseHistory content
    else return Nothing

-- | Parse HISTORY file content.
-- Format:
--   boot_count,<n>
--   total_runtime,<seconds>
--   last_boot,<timestamp>
parseHistory :: T.Text -> GhostHistory
parseHistory content = foldr parseLine defaultGhostHistory (T.lines content)
  where
    parseLine line hist = case T.breakOn "," line of
      ( "boot_count", rest ) -> hist { ghBootCount = readInt (T.drop 1 rest) }
      ( "total_runtime", rest ) -> hist { ghTotalRuntime = readInt (T.drop 1 rest) }
      ( "last_boot", rest ) -> hist { ghLastBoot = T.drop 1 rest }
      _ -> hist

    readInt :: T.Text -> Int
    readInt t = fromMaybe 0 (readMaybe (T.unpack t))

    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
      [ ( x, "" ) ] -> Just x
      _ -> Nothing

-- | Save ghost history to HISTORY file.
saveGhostHistory :: FilePath -> GhostHistory -> IO ()
saveGhostHistory ghostPath hist = do
  let path    = historyFilePath ghostPath
      content
        = T.unlines
          [ "boot_count," <> T.pack (show $ ghBootCount hist)
          , "total_runtime," <> T.pack (show $ ghTotalRuntime hist)
          , "last_boot," <> ghLastBoot hist
          ]
  createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path content

-- | Check if this is the first boot for a ghost.
-- Returns True if HISTORY file doesn't exist or boot_count is 0.
isFirstBoot :: FilePath -> IO Bool
isFirstBoot ghostPath = do
  mHist <- loadGhostHistory ghostPath
  return $ case mHist of
    Nothing   -> True
    Just hist -> ghBootCount hist == 0
