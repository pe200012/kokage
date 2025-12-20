{-# LANGUAGE OverloadedStrings #-}

-- | Balloon surface loading.
-- Handles loading balloon background images and arrow images.
--
-- Balloon surface naming convention:
-- - @balloons0.png@, @balloons1.png@, ... for sakura (main character)
-- - @balloonk0.png@, @balloonk1.png@, ... for kero (secondary character)
-- - @balloonc0.png@, @balloonc1.png@, ... for communicate box
-- - @arrow0.png@, @arrow1.png@ for scroll arrows (up/down)
-- - @sstp.png@ for SSTP marker
--
-- The balloon directory structure:
-- @
-- balloon_name/
--   descript.txt
--   balloons0.png    -- sakura balloon surface 0
--   balloons1.png    -- sakura balloon surface 1 (alternate direction)
--   balloonk0.png    -- kero balloon surface 0
--   arrow0.png       -- up scroll arrow
--   arrow1.png       -- down scroll arrow
-- @
module Kokage.Balloon.Surface
  ( -- * Types
    BalloonSurfaces(..)
  , BalloonDirection(..)
    -- * Loading
  , loadBalloonSurfaces
  , loadBalloonSurface
  , loadArrowSurface
  ) where

import qualified Data.Map.Strict     as Map
import           Data.Map.Strict     ( Map )
import qualified Data.Text           as T
import           Data.Text           ( Text )

import qualified GI.GdkPixbuf        as Pixbuf

import           Kokage.Transparency ( loadWithTransparency )

import           System.Directory    ( doesFileExist )
import           System.FilePath     ( (</>) )
import Data.Maybe (isJust)


-- | Direction the balloon faces (left or right).
-- Even numbers (0, 2, 4, ...) are typically left-facing.
-- Odd numbers (1, 3, 5, ...) are typically right-facing.
data BalloonDirection
  = BalloonLeft   -- ^ Balloon faces left (sakura on right)
  | BalloonRight  -- ^ Balloon faces right (sakura on left)
  deriving ( Show, Eq, Ord )

-- | Loaded balloon surfaces for a balloon directory.
data BalloonSurfaces
  = BalloonSurfaces
  { bsSakura      :: !(Map Int Pixbuf.Pixbuf)  -- ^ Sakura balloon surfaces (by index)
  , bsKero        :: !(Map Int Pixbuf.Pixbuf)  -- ^ Kero balloon surfaces (by index)
  , bsCommunicate :: !(Map Int Pixbuf.Pixbuf)  -- ^ Communicate box surfaces
  , bsArrowUp     :: !(Maybe Pixbuf.Pixbuf)    -- ^ Up scroll arrow (arrow0.png)
  , bsArrowDown   :: !(Maybe Pixbuf.Pixbuf)    -- ^ Down scroll arrow (arrow1.png)
  , bsSstp        :: !(Maybe Pixbuf.Pixbuf)    -- ^ SSTP marker image
  }

-- | Supported image extensions in order of preference.
imageExtensions :: [ String ]
imageExtensions = [ ".png", ".PNG", ".bmp", ".BMP" ]

-- | Find an image file with extension fallback.
findBalloonImage :: FilePath -> String -> IO (Maybe FilePath)
findBalloonImage balloonDir baseName = do
  let candidates = [ balloonDir </> baseName <> ext | ext <- imageExtensions ]
  findExisting candidates
  where
    findExisting :: [ FilePath ] -> IO (Maybe FilePath)
    findExisting []       = return Nothing
    findExisting (p : ps) = do
      exists <- doesFileExist p
      if exists
        then return (Just p)
        else findExisting ps

-- | Load a single balloon surface by character type and index.
-- Character types: "s" for sakura, "k" for kero, "c" for communicate box.
--
-- For balloons, the index is typically:
-- - Even numbers (0, 2, 4) for left-facing
-- - Odd numbers (1, 3, 5) for right-facing
--
-- Example: loadBalloonSurface "/path/to/balloon" "s" 0
-- loads "balloons0.png"
loadBalloonSurface :: FilePath      -- ^ Balloon directory path
                   -> Text          -- ^ Character type: "s", "k", or "c"
                   -> Int           -- ^ Surface index
                   -> IO (Maybe Pixbuf.Pixbuf)
loadBalloonSurface balloonDir charType index = do
  let baseName = "balloon" <> T.unpack charType <> show index
  mPath <- findBalloonImage balloonDir baseName
  case mPath of
    Nothing   -> return Nothing
    Just path -> loadWithTransparency path False

-- | Load an arrow surface (for scrolling).
-- Arrow 0 is typically the "up" arrow, arrow 1 is "down".
loadArrowSurface :: FilePath -> Int -> IO (Maybe Pixbuf.Pixbuf)
loadArrowSurface balloonDir arrowIndex = do
  let baseName = "arrow" <> show arrowIndex
  mPath <- findBalloonImage balloonDir baseName
  case mPath of
    Nothing   -> return Nothing
    Just path -> loadWithTransparency path False

-- | Load the SSTP marker image.
loadSstpSurface :: FilePath -> IO (Maybe Pixbuf.Pixbuf)
loadSstpSurface balloonDir = do
  mPath <- findBalloonImage balloonDir "sstp"
  case mPath of
    Nothing   -> return Nothing
    Just path -> loadWithTransparency path False

-- | Load all balloon surfaces from a balloon directory.
-- This loads sakura and kero surfaces for indices 0-9,
-- plus arrow images.
loadBalloonSurfaces :: FilePath -> IO BalloonSurfaces
loadBalloonSurfaces balloonDir = do
  -- Load sakura surfaces (balloons0..9)
  sakuraSurfaces <- loadSurfaceRange balloonDir "s" [0..9]

  -- Load kero surfaces (balloonk0..9)
  keroSurfaces <- loadSurfaceRange balloonDir "k" [0..9]

  -- Load communicate box surfaces (balloonc0..9)
  communicateSurfaces <- loadSurfaceRange balloonDir "c" [0..9]

  -- Load arrow images
  arrowUp <- loadArrowSurface balloonDir 0
  arrowDown <- loadArrowSurface balloonDir 1

  -- Load SSTP marker
  sstp <- loadSstpSurface balloonDir

  return BalloonSurfaces
    { bsSakura      = sakuraSurfaces
    , bsKero        = keroSurfaces
    , bsCommunicate = communicateSurfaces
    , bsArrowUp     = arrowUp
    , bsArrowDown   = arrowDown
    , bsSstp        = sstp
    }

-- | Load a range of balloon surfaces into a Map.
loadSurfaceRange :: FilePath -> Text -> [ Int ] -> IO (Map Int Pixbuf.Pixbuf)
loadSurfaceRange balloonDir charType indices = do
  pairs <- mapM loadOne indices
  return $ Map.fromList [ (i, pb) | (i, Just pb) <- pairs ]
  where
    loadOne i = do
      mPb <- loadBalloonSurface balloonDir charType i
      return (i, mPb)
