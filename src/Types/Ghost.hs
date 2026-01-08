{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Ghost, Shell, and Surface type definitions and parsers.
-- This module re-exports from submodules for convenience.
module Types.Ghost
  ( -- * Re-exports from Utils
    module Utils.Charset
  , module Utils.Text
    -- * Re-exports from Ghost.Descript
  , module Types.Ghost.Descript
    -- * Re-exports from Ghost.Shell
  , module Types.Ghost.Shell
    -- * Re-exports from Ghost.Surface
  , module Types.Ghost.Surface
    -- * Ghost and Shell types
  , Shell(..)
  , loadShell
  , Ghost(..)
  , loadGhost
  ) where

import           Control.Exception    ( SomeException, try )
import           Control.Monad        ( filterM, forM )

import           Data.Maybe           ( catMaybes )

import           System.Directory     ( doesDirectoryExist, doesFileExist, listDirectory )
import           System.FilePath      ( (</>) )

import           Types.Ghost.Descript
import           Types.Ghost.Shell
import           Types.Ghost.Surface

-- Re-export modules
import           Utils.Charset
import           Utils.Text

-- | Shell data structure with descript, surfaces, and path
data Shell
  = Shell { shellDescript :: ShellDescript
          , shellSurfaces :: Surfaces
          , shellPath     :: FilePath      -- ^ Path to the shell directory
          }
  deriving ( Show, Eq )

-- | Ghost data structure with descript, shells, and path
data Ghost
  = Ghost { ghostDescript :: GhostDescript
          , ghostShells   :: [ Shell ]
          , ghostPath     :: FilePath      -- ^ Path to the ghost root directory
          }
  deriving ( Show, Eq )

-- | Load a shell from a shell directory
-- Shell directory should contain descript.txt and surfaces.txt
loadShell :: FilePath -> IO (Maybe Shell)
loadShell shellDir = do
  let descriptPath = shellDir </> "descript.txt"
      surfacesPath = shellDir </> "surfaces.txt"

  descriptExists <- doesFileExist descriptPath
  surfacesExists <- doesFileExist surfacesPath

  if not descriptExists
    then return Nothing
    else do
      -- Load shell descript (required)
      descriptResult <- try (readShellDescript descriptPath)
        :: IO (Either SomeException ShellDescript)
      case descriptResult of
        Left _         -> return Nothing
        Right descript -> do
          -- Load surfaces (optional, use empty if not present)
          surfaces <- if surfacesExists
            then do
              surfResult <- try (readSurfaces surfacesPath) :: IO (Either SomeException Surfaces)
              case surfResult of
                Left _  -> return emptySurfaces
                Right s -> return s
            else return emptySurfaces

          return
            $ Just
              Shell { shellDescript = descript, shellSurfaces = surfaces, shellPath = shellDir }

-- | Load a ghost from a ghost root directory
-- Ghost directory structure:
--   (myghost)/
--   ├── ghost/master/descript.txt   -- Ghost descript (required)
--   ├── shell/master/               -- Default shell (required)
--   │   ├── descript.txt
--   │   └── surfaces.txt
--   └── shell/(additional)/         -- Additional shells (optional)
loadGhost :: FilePath -> IO (Maybe Ghost)
loadGhost ghostDir = do
  let ghostDescriptPath = ghostDir </> "ghost" </> "master" </> "descript.txt"
      shellBaseDir      = ghostDir </> "shell"

  -- Check if ghost descript exists
  ghostDescriptExists <- doesFileExist ghostDescriptPath
  shellDirExists <- doesDirectoryExist shellBaseDir

  if not ghostDescriptExists || not shellDirExists
    then return Nothing
    else do
      -- Load ghost descript
      descriptResult <- try (readGhostDescript ghostDescriptPath)
        :: IO (Either SomeException GhostDescript)
      case descriptResult of
        Left _         -> return Nothing
        Right descript -> do
          -- Find all shell directories
          shellDirs <- listDirectory shellBaseDir
          shellPaths <- filterM doesDirectoryExist (map (shellBaseDir </>) shellDirs)

          -- Load all shells
          shells <- forM shellPaths loadShell
          let loadedShells = catMaybes shells

          -- Must have at least the master shell
          if null loadedShells
            then return Nothing
            else return
              $ Just
                Ghost
                { ghostDescript = descript, ghostShells = loadedShells, ghostPath = ghostDir }
