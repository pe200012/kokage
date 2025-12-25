{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Input region calculation for click-through on transparent areas.
--
-- This module provides functions to create Cairo regions from Pixbuf alpha data,
-- allowing clicks on transparent parts of a surface to pass through to underlying windows.
--
-- On Wayland with layer-shell, layer surfaces receive pointer events normally.
-- To enable click-through on transparent areas, we set the input region to only
-- cover non-transparent pixels using 'gdk_surface_set_input_region'.
--
-- The implementation uses 'gdk_cairo_region_create_from_surface' which creates
-- a region containing all pixels with non-zero alpha from a Cairo surface.
module Kokage.InputRegion
  ( -- * Region Creation
    pixbufToInputRegion
  , pixbufToInputRegionWithThreshold
    -- * Applying Input Regions
  , setInputRegionFromPixbuf
  , setInputRegionFromPixbufWithThreshold
  , clearInputRegion
  ) where

import           Control.Monad.IO.Class     ( MonadIO, liftIO )
import           Data.Int                   ( Int32 )
import           Data.Word                  ( Word8 )
import           Foreign.Ptr                ( Ptr, castPtr )

import qualified GI.Cairo.Structs.Region    as CairoRegion
import qualified GI.Cairo.Structs.Surface   as CairoSurface
import qualified GI.Gdk                     as Gdk
import qualified GI.GdkPixbuf               as Pixbuf

import qualified GI.Cairo.Render            as Cairo
import           GI.Cairo.Render.Connector  ( getContext )
import           GI.Cairo.Render.Types      ( withSurface )
import           Data.GI.Base.ManagedPtr    ( wrapBoxed )

-- | FFI import for cairo_surface_reference to increment reference count.
-- This is needed because we're sharing the surface pointer between
-- gi-cairo-render and gi-cairo, and both will try to free it.
foreign import ccall "cairo_surface_reference"
  cairo_surface_reference :: Ptr () -> IO (Ptr ())


-- This function renders the pixbuf to a Cairo ImageSurface and uses
-- 'gdk_cairo_region_create_from_surface' to create a region from pixels
-- with non-zero alpha.
--
-- Returns 'Nothing' if the pixbuf has no alpha channel (entire surface is opaque).
pixbufToInputRegion :: MonadIO m => Pixbuf.Pixbuf -> m (Maybe CairoRegion.Region)
pixbufToInputRegion = pixbufToInputRegionWithThreshold

-- | Create a Cairo region from a Pixbuf with a custom alpha threshold.
-- Pixels with alpha >= threshold will be included in the input region.
--
-- A higher threshold means more transparent pixels will allow click-through.
-- For example:
--   - threshold = 1: Any pixel with alpha >= 1 receives input
--   - threshold = 128: Only pixels with alpha >= 128 (50%+) receive input
--   - threshold = 255: Only fully opaque pixels receive input
--
-- Note: The current implementation ignores the threshold and uses the default
-- behavior of 'gdk_cairo_region_create_from_surface' which includes all pixels
-- with alpha > 0. For custom thresholds, a more complex implementation would
-- be needed to pre-process the alpha values.
pixbufToInputRegionWithThreshold :: MonadIO m
                                  => Pixbuf.Pixbuf
                                  -> m (Maybe CairoRegion.Region)
pixbufToInputRegionWithThreshold pixbuf = liftIO $ do
  hasAlpha <- Pixbuf.pixbufGetHasAlpha pixbuf
  if not hasAlpha
    then return Nothing  -- No alpha channel = entire surface is opaque, use default input region
    else do
      w <- Pixbuf.pixbufGetWidth pixbuf
      h <- Pixbuf.pixbufGetHeight pixbuf

      -- Create an ImageSurface and render the pixbuf to it
      -- The surface will have alpha from the pixbuf
      cairoSurface <- Cairo.createImageSurface Cairo.FormatARGB32
                        (fromIntegral w) (fromIntegral h)

      Cairo.renderWith cairoSurface $ do
        -- Get the GI.Cairo.Context from within the Render monad
        ctx <- getContext
        -- Paint with the pixbuf
        Gdk.cairoSetSourcePixbuf ctx pixbuf 0 0
        Cairo.paint

      -- Convert gi-cairo-render Surface to gi-cairo Surface for use with GDK
      -- Both wrap the same underlying cairo_surface_t pointer
      giCairoSurface <- renderSurfaceToGiSurface cairoSurface

      -- Create region from the surface - this includes all pixels with alpha > 0
      region <- Gdk.cairoRegionCreateFromSurface giCairoSurface

      return $ Just region

-- | Convert a gi-cairo-render Surface to a gi-cairo Surface.
-- Both are wrappers around the same cairo_surface_t, but different Haskell types.
-- We increment the reference count so both wrappers can safely free their reference.
renderSurfaceToGiSurface :: MonadIO m => Cairo.Surface -> m CairoSurface.Surface
renderSurfaceToGiSurface renderSurface = liftIO $ do
  -- Get the underlying pointer from gi-cairo-render Surface
  -- and wrap it as gi-cairo Surface
  withSurface renderSurface $ \ptr -> do
    -- Increment the reference count so both the gi-cairo-render Surface
    -- and the gi-cairo Surface can safely free their reference
    _ <- cairo_surface_reference (castPtr ptr)
    -- Now wrap the referenced pointer as a gi-cairo Surface
    wrapBoxed CairoSurface.Surface (castPtr ptr)

-- | Set the input region of a GDK surface based on a Pixbuf's alpha channel.
-- Only non-transparent pixels will receive input events.
--
-- This function should be called after the window is realized/shown.
--
-- Returns True if the input region was set, False if the pixbuf has no alpha
-- (in which case the default full-surface input region is used).
setInputRegionFromPixbuf :: MonadIO m
                          => Gdk.Surface
                          -> Pixbuf.Pixbuf
                          -> m Bool
setInputRegionFromPixbuf = setInputRegionFromPixbufWithThreshold

-- | Set the input region with a custom alpha threshold.
setInputRegionFromPixbufWithThreshold :: MonadIO m
                                       => Gdk.Surface
                                       -> Pixbuf.Pixbuf
                                       -> m Bool
setInputRegionFromPixbufWithThreshold surface pixbuf = do
  mRegion <- pixbufToInputRegionWithThreshold pixbuf
  case mRegion of
    Nothing -> return False
    Just region -> do
      Gdk.surfaceSetInputRegion surface region
      return True

-- | Clear the input region, making the entire surface receive input events.
-- This is done by setting the input region to cover the full surface dimensions.
clearInputRegion :: MonadIO m => Gdk.Surface -> Int32 -> Int32 -> m ()
clearInputRegion surface width height = liftIO $ do
  -- Create a fully opaque surface of the given size
  cairoSurface <- Cairo.createImageSurface Cairo.FormatARGB32
                    (fromIntegral width) (fromIntegral height)

  Cairo.renderWith cairoSurface $ do
    -- Fill with fully opaque black (any opaque color works)
    Cairo.setSourceRGBA 0 0 0 1
    Cairo.paint

  giCairoSurface <- renderSurfaceToGiSurface cairoSurface
  region <- Gdk.cairoRegionCreateFromSurface giCairoSurface
  Gdk.surfaceSetInputRegion surface region
