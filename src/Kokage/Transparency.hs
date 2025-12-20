{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Transparency handling for ghost surfaces.
-- Implements chroma-key (top-left pixel) and PNA alpha mask transparency.
--
-- Per ukadoc specification:
-- - Chroma-key: "画像左上の1ドット(座標0,0)と同色の領域は透過色"
--   (The top-left pixel color becomes transparent)
-- - PNA: Grayscale mask where black=transparent, white=opaque
module Kokage.Transparency
  ( -- * Transparency Operations
    applyChromaKey
  , applyPnaAlpha
  , loadWithTransparency
  ) where

import Control.Exception ( SomeException, catch, bracket )
import Control.Monad ( forM_ )

import qualified Data.ByteString as BS
import Data.Int ( Int32 )
import Data.Word ( Word8 )

import Foreign.Marshal.Alloc ( mallocBytes, free )
import Foreign.Ptr ( Ptr, plusPtr )
import Foreign.Storable ( poke )

import qualified GI.GdkPixbuf as Pixbuf

import System.Directory ( doesFileExist )
import System.FilePath ( replaceExtension )


-- | Apply chroma-key transparency using top-left pixel color.
-- The color at pixel (0,0) becomes fully transparent throughout the image.
applyChromaKey :: Pixbuf.Pixbuf -> IO Pixbuf.Pixbuf
applyChromaKey pixbuf = do
  width <- Pixbuf.pixbufGetWidth pixbuf
  height <- Pixbuf.pixbufGetHeight pixbuf
  rowstride <- Pixbuf.pixbufGetRowstride pixbuf
  nChannels <- Pixbuf.pixbufGetNChannels pixbuf

  -- Get pixel data as ByteString (O(1) indexing)
  pixelData <- Pixbuf.pixbufGetPixels pixbuf

  -- Get top-left pixel color (chroma key)
  let keyR = BS.index pixelData 0
      keyG = BS.index pixelData 1
      keyB = BS.index pixelData 2

  -- Create new RGBA buffer
  let newRowstride = fromIntegral width * 4
      bufferSize = fromIntegral height * newRowstride

  -- Use bracket to ensure cleanup on error
  bracket (mallocBytes bufferSize) free $ \buffer -> do
    -- Process pixels
    processChromaKey buffer pixelData
      (fromIntegral width) (fromIntegral height)
      (fromIntegral rowstride) (fromIntegral nChannels)
      newRowstride keyR keyG keyB

    -- Create new pixbuf - copy the data so we can free buffer
    createRgbaPixbuf buffer width height newRowstride

-- | Process pixels: copy RGB and apply chroma key to alpha channel.
processChromaKey
  :: Ptr Word8    -- ^ Destination buffer
  -> BS.ByteString -- ^ Source pixel data
  -> Int -> Int    -- ^ Width, height
  -> Int -> Int    -- ^ Source rowstride, channels
  -> Int           -- ^ Dest rowstride
  -> Word8 -> Word8 -> Word8  -- ^ Chroma key RGB
  -> IO ()
processChromaKey destBuf srcBS width height srcRowstride srcChannels
                 destRowstride keyR keyG keyB =
  forM_ [ 0 .. height - 1 ] $ \y ->
    forM_ [ 0 .. width - 1 ] $ \x -> do
      let srcOffset = y * srcRowstride + x * srcChannels
          destOffset = y * destRowstride + x * 4
          destPtr = destBuf `plusPtr` destOffset

          -- O(1) ByteString indexing
          r = BS.index srcBS srcOffset
          g = BS.index srcBS (srcOffset + 1)
          b = BS.index srcBS (srcOffset + 2)

          -- Alpha: 0 if matches chroma key, 255 otherwise
          alpha = if r == keyR && g == keyG && b == keyB
                  then 0 :: Word8
                  else 255

      poke destPtr r
      poke (destPtr `plusPtr` 1) g
      poke (destPtr `plusPtr` 2) b
      poke (destPtr `plusPtr` 3) alpha


-- | Apply PNA grayscale alpha mask to a pixbuf.
-- The R channel of the PNA image becomes the alpha channel.
applyPnaAlpha :: Pixbuf.Pixbuf -> Pixbuf.Pixbuf -> IO Pixbuf.Pixbuf
applyPnaAlpha pnaPixbuf pngPixbuf = do
  pngWidth <- Pixbuf.pixbufGetWidth pngPixbuf
  pngHeight <- Pixbuf.pixbufGetHeight pngPixbuf
  pngRowstride <- Pixbuf.pixbufGetRowstride pngPixbuf
  pngNChannels <- Pixbuf.pixbufGetNChannels pngPixbuf
  pnaRowstride <- Pixbuf.pixbufGetRowstride pnaPixbuf
  pnaNChannels <- Pixbuf.pixbufGetNChannels pnaPixbuf

  -- Get pixel data as ByteString
  pngData <- Pixbuf.pixbufGetPixels pngPixbuf
  pnaData <- Pixbuf.pixbufGetPixels pnaPixbuf

  -- Create new RGBA buffer
  let newRowstride = fromIntegral pngWidth * 4
      bufferSize = fromIntegral pngHeight * newRowstride

  bracket (mallocBytes bufferSize) free $ \buffer -> do
    -- Copy RGB from PNG, alpha from PNA
    forM_ [ 0 .. fromIntegral pngHeight - 1 ] $ \y ->
      forM_ [ 0 .. fromIntegral pngWidth - 1 ] $ \x -> do
        let pngOffset = y * fromIntegral pngRowstride + x * fromIntegral pngNChannels
            pnaOffset = y * fromIntegral pnaRowstride + x * fromIntegral pnaNChannels
            destOffset = y * newRowstride + x * 4
            destPtr = buffer `plusPtr` destOffset

            r = BS.index pngData pngOffset
            g = BS.index pngData (pngOffset + 1)
            b = BS.index pngData (pngOffset + 2)
            alpha = BS.index pnaData pnaOffset  -- PNA R channel = alpha

        poke destPtr r
        poke (destPtr `plusPtr` 1) g
        poke (destPtr `plusPtr` 2) b
        poke (destPtr `plusPtr` 3) alpha

    createRgbaPixbuf buffer pngWidth pngHeight newRowstride


-- | Load a surface image with appropriate transparency applied.
-- Priority: PNA file > chroma-key (unless useSelfAlpha is True).
loadWithTransparency
  :: FilePath  -- ^ Full path to PNG file
  -> Bool      -- ^ Use PNG's native alpha (skip chroma-key/PNA)
  -> IO (Maybe Pixbuf.Pixbuf)
loadWithTransparency pngPath useSelfAlpha = do
  mPixbuf <- loadPixbufSafe pngPath
  case mPixbuf of
    Nothing -> pure Nothing
    Just pixbuf
      | useSelfAlpha -> ensureAlpha pixbuf
      | otherwise -> do
          let pnaPath = replaceExtension pngPath ".pna"
          hasPna <- doesFileExist pnaPath
          if hasPna
            then do
              mPna <- loadPixbufSafe pnaPath
              case mPna of
                Just pna -> Just <$> applyPnaAlpha pna pixbuf
                Nothing  -> Just <$> applyChromaKey pixbuf
            else Just <$> applyChromaKey pixbuf


-- | Ensure pixbuf has an alpha channel.
ensureAlpha :: Pixbuf.Pixbuf -> IO (Maybe Pixbuf.Pixbuf)
ensureAlpha pixbuf = do
  hasAlpha <- Pixbuf.pixbufGetHasAlpha pixbuf
  if hasAlpha
    then pure (Just pixbuf)
    else Pixbuf.pixbufAddAlpha pixbuf False 0 0 0


-- | Safely load a pixbuf, returning Nothing on error.
loadPixbufSafe :: FilePath -> IO (Maybe Pixbuf.Pixbuf)
loadPixbufSafe path =
  Pixbuf.pixbufNewFromFile path
    `catch` \(_ :: SomeException) -> pure Nothing


-- | Create an RGBA pixbuf by copying data from a buffer.
-- This copies the data so the source buffer can be freed.
createRgbaPixbuf :: Ptr Word8 -> Int32 -> Int32 -> Int -> IO Pixbuf.Pixbuf
createRgbaPixbuf srcBuffer width height rowstride = do
  -- Create temporary pixbuf from our buffer (no destroy function)
  tmpPixbuf <- Pixbuf.pixbufNewFromData
    srcBuffer
    Pixbuf.ColorspaceRgb
    True   -- has_alpha
    8      -- bits_per_sample
    width
    height
    (fromIntegral rowstride)
    Nothing  -- No destroy function, we manage the buffer with bracket

  -- Copy to a new pixbuf so the original buffer can be freed
  mCopied <- Pixbuf.pixbufCopy tmpPixbuf
  case mCopied of
    Nothing -> error "Failed to copy pixbuf"
    Just copied -> pure copied
