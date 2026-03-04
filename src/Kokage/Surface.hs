{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Surface image loading and compositing.
-- Handles loading surface images and compositing elements with transparency.
-- Uses Cairo for proper blend mode support (overlay, overlayfast, replace, interpolate, reduce).
module Kokage.Surface
  ( compositeSurface
  , loadDefaultSurface
  , findElementImage
  , findSurfaceById
  , drawMethodToOperator
  ) where

import           Data.GI.Base.ManagedPtr   ( wrapBoxed )
import qualified Data.Text                 as T

import           Foreign.Ptr               ( Ptr, castPtr )

import qualified GI.Cairo.Render           as Cairo
import qualified GI.Cairo.Render.Connector as Cairo
import           GI.Cairo.Render.Types     ( withSurface )
import qualified GI.Cairo.Structs.Surface  as CairoSurface
import qualified GI.Gdk                    as Gdk
import qualified GI.GdkPixbuf              as Pixbuf

import           Kokage.Transparency       ( loadWithTransparency )

import           Prelude                   ()

import           Relude

import           System.Directory          ( doesFileExist )
import           System.FilePath           ( (</>) )

import           Text.Printf               ( printf )

import           Types.Ghost               ( DrawMethod(..)
                                           , Element(..)
                                           , SurfaceDefinition(..)
                                           , Surfaces(..)
                                           )

-- | FFI import for cairo_surface_reference to increment reference count.
foreign import ccall "cairo_surface_reference" cairo_surface_reference :: Ptr () -> IO (Ptr ())

-- | Supported image extensions in order of preference.
imageExtensions :: [ String ]
imageExtensions = [ ".png", ".PNG", ".bmp", ".BMP" ]

-- | Map DrawMethod to Cairo compositing operator.
--   base → SOURCE, overlay → OVER, overlayfast → ATOP,
--   overlaymultiply → ATOP, replace → SOURCE, interpolate → SATURATE,
--   asis → OVER, bind → OVER, add → OVER, reduce → DEST_IN
drawMethodToOperator :: DrawMethod -> Cairo.Operator
drawMethodToOperator DrawBase = Cairo.OperatorSource
drawMethodToOperator DrawOverlay = Cairo.OperatorOver
drawMethodToOperator DrawOverlayfast = Cairo.OperatorAtop
drawMethodToOperator DrawOverlaymultiply = Cairo.OperatorAtop
drawMethodToOperator DrawReplace = Cairo.OperatorSource
drawMethodToOperator DrawInterpolate = Cairo.OperatorSaturate
drawMethodToOperator DrawAsis = Cairo.OperatorOver
drawMethodToOperator DrawBind = Cairo.OperatorOver
drawMethodToOperator DrawAdd = Cairo.OperatorAdd
drawMethodToOperator DrawReduce = Cairo.OperatorDestIn
-- Animation control methods - use OVER as default (they control timing, not blending)
drawMethodToOperator DrawMove = Cairo.OperatorOver
drawMethodToOperator (DrawInsert _) = Cairo.OperatorOver
drawMethodToOperator (DrawStart _) = Cairo.OperatorOver
drawMethodToOperator (DrawStop _) = Cairo.OperatorOver
drawMethodToOperator (DrawAlternativeStart _) = Cairo.OperatorOver
drawMethodToOperator (DrawAlternativeStop _) = Cairo.OperatorOver
drawMethodToOperator (DrawParallelStart _) = Cairo.OperatorOver
drawMethodToOperator (DrawParallelStop _) = Cairo.OperatorOver

-- | Find an image file with extension fallback.
-- Given a shell path and element filename (without extension),
-- try each supported extension until one exists.
findElementImage :: FilePath -> Text -> IO (Maybe FilePath)
findElementImage shellDir fileName = do
  let baseName   = T.unpack fileName
      -- First try the exact filename (may already have extension)
      candidates
        = (shellDir </> baseName) : [ shellDir </> baseName <> ext | ext <- imageExtensions ]
  findExisting candidates
  where
    findExisting :: [ FilePath ] -> IO (Maybe FilePath)
    findExisting []       = return Nothing
    findExisting (p : ps) = do
      exists <- doesFileExist p
      if exists
        then return (Just p)
        else findExisting ps

-- | Load a single element as a pixbuf with transparency applied.
-- Returns the pixbuf, position, and draw method for compositing.
loadElementPixbuf :: FilePath -> Element -> IO (Maybe ( Pixbuf.Pixbuf, Int, Int, DrawMethod ))
loadElementPixbuf shellDir el = do
  mPath <- findElementImage shellDir (elemFile el)
  case mPath of
    Nothing   -> do
      putStrLn $ "Warning: Could not find image: " <> T.unpack (elemFile el)
      return Nothing
    Just path -> do
      -- Load with transparency (chroma-key or PNA)
      -- DrawAsis means "as-is" - use PNG's native alpha, not chroma-key
      let useSelfAlpha = elemMethod el == DrawAsis
      mPixbuf <- loadWithTransparency path useSelfAlpha
      case mPixbuf of
        Nothing     -> do
          putStrLn $ "Warning: Failed to load image: " <> path
          return Nothing
        Just pixbuf -> return $ Just ( pixbuf, elemX el, elemY el, elemMethod el )

-- | Composite all elements of a surface definition into a single pixbuf.
-- Returns the composited pixbuf and its dimensions.
-- If no elements are defined, tries to load the default surface image (surface{id:04d}.png)
-- Uses Cairo for proper blend mode support.
compositeSurface :: FilePath -> SurfaceDefinition -> IO (Maybe Pixbuf.Pixbuf)
compositeSurface shellDir surfDef = do
  -- Load all element pixbufs
  let elements = sortBy (comparing elemIndex) (sdElements surfDef)

  -- If no elements defined, try loading default surface image
  if null elements
    then loadDefaultSurface shellDir (sdId surfDef)
    else do
      loaded <- forM elements $ loadElementPixbuf shellDir

      let pixbufs = catMaybes loaded

      case pixbufs of
        [] -> loadDefaultSurface shellDir (sdId surfDef)  -- Fallback to default
        (( firstPixbuf, _, _, _ ) : _) -> compositeWithCairo firstPixbuf pixbufs

-- | Load the default surface image for a given surface ID.
-- Ukagaka convention: surface{id:04d}.png (e.g., surface0000.png)
loadDefaultSurface :: FilePath -> Int -> IO (Maybe Pixbuf.Pixbuf)
loadDefaultSurface shellDir surfId = do
  let defaultName = T.pack $ printf "surface%04d" surfId
  mPath <- findElementImage shellDir defaultName
  case mPath of
    Nothing   -> do
      putStrLn $ "Warning: No default surface image found for surface " <> show surfId
      return Nothing
    Just path -> do
      putStrLn $ "Loading default surface: " <> path
      loadWithTransparency path False

-- | Composite multiple pixbufs using Cairo for proper blend mode support.
-- Each element specifies its own DrawMethod which maps to a Cairo operator.
compositeWithCairo
  :: Pixbuf.Pixbuf -> [ ( Pixbuf.Pixbuf, Int, Int, DrawMethod ) ] -> IO (Maybe Pixbuf.Pixbuf)
compositeWithCairo firstPixbuf pixbufs = do
  -- Get dimensions from first element
  width <- Pixbuf.pixbufGetWidth firstPixbuf
  height <- Pixbuf.pixbufGetHeight firstPixbuf

  -- Create a Cairo image surface for compositing
  Cairo.withImageSurface Cairo.FormatARGB32 (fromIntegral width) (fromIntegral height)
    $ \surface -> do
      Cairo.renderWith surface $ do
        -- Clear to transparent
        Cairo.setSourceRGBA 0 0 0 0
        Cairo.setOperator Cairo.OperatorClear
        Cairo.paint

        -- Composite each element with its specific blend mode
        forM_ pixbufs $ \( pixbuf, x, y, method ) -> do
          Cairo.setOperator (drawMethodToOperator method)
          -- Use save/restore to isolate the source setting
          Cairo.save
          Cairo.translate (fromIntegral x) (fromIntegral y)
          -- Get Cairo context and set pixbuf as source
          ctx <- Cairo.getContext
          Cairo.liftIO $ Gdk.cairoSetSourcePixbuf ctx pixbuf 0 0
          Cairo.paint
          Cairo.restore

      -- Convert Cairo surface back to Pixbuf via Gdk.pixbufGetFromSurface
      cairoSurfaceToPixbuf surface width height

-- | Convert a Cairo ImageSurface to a GdkPixbuf.
-- Uses Gdk.pixbufGetFromSurface which handles the ARGB->RGBA conversion.
cairoSurfaceToPixbuf :: Cairo.Surface -> Int32 -> Int32 -> IO (Maybe Pixbuf.Pixbuf)
cairoSurfaceToPixbuf surface width height = do
  Cairo.surfaceFlush surface
  -- Convert gi-cairo-render Surface to gi-cairo Surface for use with GDK
  giSurface <- renderSurfaceToGiSurface surface
  Gdk.pixbufGetFromSurface giSurface 0 0 width height

-- | Convert a gi-cairo-render Surface to a gi-cairo Surface.
-- Both are wrappers around the same cairo_surface_t, but different Haskell types.
-- We increment the reference count so both wrappers can safely free their reference.
renderSurfaceToGiSurface :: Cairo.Surface -> IO CairoSurface.Surface
renderSurfaceToGiSurface renderSurface = do
  withSurface renderSurface $ \ptr -> do
    -- Increment the reference count so both the gi-cairo-render Surface
    -- and the gi-cairo Surface can safely free their reference
    _ <- cairo_surface_reference (castPtr ptr)
    wrapBoxed CairoSurface.Surface (castPtr ptr)

-- | Find a surface by ID from a surfaces definition.
findSurfaceById :: Int -> Surfaces -> Maybe SurfaceDefinition
findSurfaceById surfId surfaces = find (\sd -> sdId sd == surfId) (surfaceDefinitions surfaces)
