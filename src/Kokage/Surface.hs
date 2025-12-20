{-# LANGUAGE OverloadedStrings #-}

-- | Surface image loading and compositing.
-- Handles loading surface images and compositing elements with transparency.
module Kokage.Surface
  ( compositeSurface
  , loadDefaultSurface
  , findElementImage
  , findSurfaceById
  ) where

import Control.Monad ( forM, forM_, when )
import Data.Foldable ( find )
import Data.List ( sortBy )
import Data.Maybe ( catMaybes )
import Data.Ord ( comparing )
import Data.Text ( Text )
import qualified Data.Text as T

import qualified GI.GdkPixbuf as Pixbuf

import System.Directory ( doesFileExist )
import System.FilePath ( (</>) )
import Text.Printf ( printf )

import Types.Ghost
  ( Element(..)
  , DrawMethod(..)
  , SurfaceDefinition(..)
  , Surfaces(..)
  )

import Kokage.Transparency ( loadWithTransparency )


-- | Supported image extensions in order of preference.
imageExtensions :: [ String ]
imageExtensions = [ ".png", ".PNG", ".bmp", ".BMP" ]

-- | Find an image file with extension fallback.
-- Given a shell path and element filename (without extension),
-- try each supported extension until one exists.
findElementImage :: FilePath -> Text -> IO (Maybe FilePath)
findElementImage shellDir fileName = do
  let baseName = T.unpack fileName
      -- First try the exact filename (may already have extension)
      candidates = (shellDir </> baseName)
                 : [ shellDir </> baseName <> ext | ext <- imageExtensions ]
  findExisting candidates
  where
    findExisting :: [ FilePath ] -> IO (Maybe FilePath)
    findExisting [] = return Nothing
    findExisting (p : ps) = do
      exists <- doesFileExist p
      if exists
        then return (Just p)
        else findExisting ps

-- | Load a single element as a pixbuf with transparency applied.
loadElementPixbuf :: FilePath -> Element -> IO (Maybe ( Pixbuf.Pixbuf, Int, Int ))
loadElementPixbuf shellDir el = do
  mPath <- findElementImage shellDir (elemFile el)
  case mPath of
    Nothing -> do
      putStrLn $ "Warning: Could not find image: " <> T.unpack (elemFile el)
      return Nothing
    Just path -> do
      -- Load with transparency (chroma-key or PNA)
      -- DrawAsis means "as-is" - use PNG's native alpha, not chroma-key
      let useSelfAlpha = elemMethod el == DrawAsis
      mPixbuf <- loadWithTransparency path useSelfAlpha
      case mPixbuf of
        Nothing -> do
          putStrLn $ "Warning: Failed to load image: " <> path
          return Nothing
        Just pixbuf -> return $ Just ( pixbuf, elemX el, elemY el )

-- | Composite all elements of a surface definition into a single pixbuf.
-- Returns the composited pixbuf and its dimensions.
-- If no elements are defined, tries to load the default surface image (surface{id:04d}.png)
compositeSurface :: FilePath -> SurfaceDefinition -> IO (Maybe Pixbuf.Pixbuf)
compositeSurface shellDir surfDef = do
  -- Load all element pixbufs
  let elements = sortBy (comparing elemIndex) (sdElements surfDef)

  -- If no elements defined, try loading default surface image
  if null elements
    then loadDefaultSurface shellDir (sdId surfDef)
    else do
      loaded <- forM elements $ \el -> case elemMethod el of
        -- Only handle simple overlay methods for now
        DrawBase    -> loadElementPixbuf shellDir el
        DrawOverlay -> loadElementPixbuf shellDir el
        DrawBind    -> loadElementPixbuf shellDir el
        DrawAdd     -> loadElementPixbuf shellDir el
        _           -> return Nothing  -- Skip complex draw methods

      let pixbufs = catMaybes loaded

      case pixbufs of
        [] -> loadDefaultSurface shellDir (sdId surfDef)  -- Fallback to default
        (( first, _, _ ) : _) -> compositePixbufs first pixbufs

-- | Load the default surface image for a given surface ID.
-- Ukagaka convention: surface{id:04d}.png (e.g., surface0000.png)
loadDefaultSurface :: FilePath -> Int -> IO (Maybe Pixbuf.Pixbuf)
loadDefaultSurface shellDir surfId = do
  let defaultName = T.pack $ printf "surface%04d" surfId
  mPath <- findElementImage shellDir defaultName
  case mPath of
    Nothing -> do
      putStrLn $ "Warning: No default surface image found for surface " <> show surfId
      return Nothing
    Just path -> do
      putStrLn $ "Loading default surface: " <> path
      loadWithTransparency path False

-- | Composite multiple pixbufs onto a destination.
compositePixbufs :: Pixbuf.Pixbuf -> [ ( Pixbuf.Pixbuf, Int, Int ) ] -> IO (Maybe Pixbuf.Pixbuf)
compositePixbufs first pixbufs = do
  -- Get dimensions from first element
  width <- Pixbuf.pixbufGetWidth first
  height <- Pixbuf.pixbufGetHeight first

  -- Create a new pixbuf for compositing (with alpha)
  mDest <- Pixbuf.pixbufNew
    Pixbuf.ColorspaceRgb  -- colorspace
    True                  -- has_alpha
    8                     -- bits_per_sample
    width
    height

  case mDest of
    Nothing -> do
      putStrLn "Error: Failed to create destination pixbuf"
      return Nothing
    Just dest -> do
      -- Fill with transparent
      Pixbuf.pixbufFill dest 0x00000000

      -- Composite each element onto the destination
      forM_ pixbufs $ \( pixbuf, x, y ) -> do
        srcWidth <- Pixbuf.pixbufGetWidth pixbuf
        srcHeight <- Pixbuf.pixbufGetHeight pixbuf

        -- Clamp coordinates to valid range
        let destX = max 0 (fromIntegral x)
            destY = max 0 (fromIntegral y)

        -- Only composite if within bounds
        when (destX < width && destY < height)
          $ Pixbuf.pixbufComposite
              pixbuf              -- src
              dest                -- dest
              destX               -- dest_x
              destY               -- dest_y
              (min srcWidth (width - destX))   -- dest_width
              (min srcHeight (height - destY)) -- dest_height
              (fromIntegral x)    -- offset_x
              (fromIntegral y)    -- offset_y
              1.0                 -- scale_x
              1.0                 -- scale_y
              Pixbuf.InterpTypeBilinear  -- interp_type
              255                 -- overall_alpha

      return $ Just dest

-- | Find a surface by ID from a surfaces definition.
findSurfaceById :: Int -> Surfaces -> Maybe SurfaceDefinition
findSurfaceById surfId surfaces =
  find (\sd -> sdId sd == surfId) (surfaceDefinitions surfaces)
