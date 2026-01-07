{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Ghost scanning, resolution, and management utilities.
module Kokage.Ghost
  ( -- * Ghost scanning
    scanGhosts
  , isValidGhostDir
    -- * Ghost resolution
  , resolveGhost
  , getDefaultShell
    -- * Last ghost persistence
  , saveLastGhost
  , loadLastGhost
    -- * Balloon resolution
  , findBalloonDir
    -- * Screen geometry
  , getScreenGeometry
  ) where

import           Control.Exception          ( SomeException, try )
import           Control.Monad              ( filterM )

import           Data.List                  ( find, sortOn )
import           Data.Maybe                 ( fromMaybe, listToMaybe )
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO

import           System.Directory           ( createDirectoryIfMissing
                                            , doesDirectoryExist
                                            , doesFileExist
                                            , getXdgDirectory
                                            , listDirectory
                                            , XdgDirectory(..)
                                            )
import           System.FilePath            ( (</>), takeBaseName )

import qualified GI.Gdk                     as Gdk
import qualified GI.Gio                     as Gio

import           Kokage.Config              ( BaseDir(..) )
import           Types.Ghost                ( Ghost(..), Shell(..)
                                            , shellDescriptName, shellDescript
                                            )

-- | Get the default shell from a ghost.
-- Returns the "master" shell if it exists, otherwise the first shell.
getDefaultShell :: Ghost -> Maybe Shell
getDefaultShell ghost = case ghostShells ghost of
  []     -> Nothing
  (s:ss) -> Just $ fromMaybe s $
    find (\sh -> shellDescriptName (shellDescript sh) == "master") (s:ss)

-- | Get screen geometry (width, height) from the default monitor.
getScreenGeometry :: IO (Int, Int)
getScreenGeometry = do
  display <- Gdk.displayGetDefault >>= \case
    Nothing -> error "No display available"
    Just d  -> return d
  monitors <- Gdk.displayGetMonitors display
  nMonitors <- Gio.listModelGetNItems monitors
  if nMonitors == 0
    then return (1920, 1080)  -- Fallback default
    else do
      mObj <- Gio.listModelGetItem monitors 0
      case mObj of
        Nothing -> return (1920, 1080)
        Just obj -> do
          monitor <- Gdk.unsafeCastTo Gdk.Monitor obj
          geo <- Gdk.monitorGetGeometry monitor
          w <- Gdk.getRectangleWidth geo
          h <- Gdk.getRectangleHeight geo
          return (fromIntegral w, fromIntegral h)

-- | Scan a directory for valid ghost subdirectories.
-- Returns list of (ghost_name, ghost_path) pairs sorted by name.
scanGhosts :: BaseDir -> IO [(T.Text, FilePath)]
scanGhosts (BaseDir baseDir) = do
  let ghostBaseDir = baseDir </> "ghost"
  exists <- doesDirectoryExist ghostBaseDir
  if not exists
    then return []
    else do
      entries <- listDirectory ghostBaseDir
      let fullPaths = map (ghostBaseDir </>) entries
      validPaths <- filterM isValidGhostDir fullPaths
      return $ sortOn fst
        [ (T.pack $ takeBaseName p, p) | p <- validPaths ]

-- | Check if a directory is a valid ghost directory.
-- A valid ghost has ghost/master/descript.txt
isValidGhostDir :: FilePath -> IO Bool
isValidGhostDir path = do
  let descriptPath = path </> "ghost" </> "master" </> "descript.txt"
  doesFileExist descriptPath

-- | Resolve a ghost path from config or scan for available ghosts.
-- Priority:
-- 1. Explicit ghost path from config
-- 2. Last used ghost (from persistence)
-- 3. First available ghost from scan
resolveGhost :: Maybe FilePath -> BaseDir -> IO (Maybe FilePath)
resolveGhost mExplicitPath baseDir = case mExplicitPath of
  Just path -> do
    valid <- isValidGhostDir path
    if valid then return (Just path) else scanFirst
  Nothing -> do
    mLast <- loadLastGhost
    case mLast of
      Just lastPath -> do
        valid <- isValidGhostDir lastPath
        if valid then return (Just lastPath) else scanFirst
      Nothing -> scanFirst
  where
    scanFirst = do
      ghosts <- scanGhosts baseDir
      return $ snd <$> listToMaybe ghosts

-- | Save the last used ghost path to XDG config.
saveLastGhost :: FilePath -> IO ()
saveLastGhost gPath = do
  configDir <- getXdgDirectory XdgConfig "kokage"
  createDirectoryIfMissing True configDir
  let configFile = configDir </> "last_ghost"
  TIO.writeFile configFile (T.pack gPath)

-- | Load the last used ghost path from XDG config.
loadLastGhost :: IO (Maybe FilePath)
loadLastGhost = do
  configDir <- getXdgDirectory XdgConfig "kokage"
  let configFile = configDir </> "last_ghost"
  exists <- doesFileExist configFile
  if exists
    then do
      result <- try (TIO.readFile configFile) :: IO (Either SomeException T.Text)
      case result of
        Left _        -> return Nothing
        Right content -> return $ Just $ T.unpack $ T.strip content
    else return Nothing

-- | Find balloon directory.
-- Search order:
-- 1. Ghost's recommended balloon path
-- 2. Ghost's default balloon path
-- 3. Balloon with matching name in base balloon directory
-- 4. "master" balloon in base directory
-- 5. First available balloon
findBalloonDir :: BaseDir -> Ghost -> IO (Maybe FilePath)
findBalloonDir (BaseDir baseDir) _ghost = do
  let balloonBaseDir = baseDir </> "balloon"
  
  -- Check if balloon base directory exists
  exists <- doesDirectoryExist balloonBaseDir
  if not exists
    then return Nothing
    else do
      entries <- listDirectory balloonBaseDir
      let fullPaths = map (balloonBaseDir </>) entries
      validPaths <- filterM doesDirectoryExist fullPaths
      
      -- Filter for directories with descript.txt
      validBalloons <- filterM (\p -> doesFileExist (p </> "descript.txt")) validPaths
      
      if null validBalloons
        then return Nothing
        else do
          -- Try to find "master" balloon first
          let masterPath = balloonBaseDir </> "master"
          hasMaster <- doesDirectoryExist masterPath
          if hasMaster
            then return (Just masterPath)
            else return (listToMaybe validBalloons)
