{-# LANGUAGE OverloadedStrings #-}

-- | Charset detection and conversion utilities.
-- Used for reading Japanese ghost files encoded in various charsets (Shift_JIS, UTF-8, etc.)
module Utils.Charset
  ( -- * Charset detection
    isValidUtf8
  , hasUtf8Bom
  , guessCharset
  , detectCharset
  , detectCharsetFromBytes
  , findCharsetDeclaration
  , normalizeCharset
    -- * Charset conversion
  , convertToUtf8
  ) where

import           Codec.Text.IConv           ( EncodingName, convert )

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Char                  ( toLower )
import           Data.Maybe                 ( fromMaybe )
import           Data.Word                  ( Word8 )

import           Utils.Text                 ( cleanStr )

-- | Check if bytes appear to be valid UTF-8.
-- Returns True if the bytes are valid UTF-8, False otherwise.
isValidUtf8 :: BL.ByteString -> Bool
isValidUtf8 bs = go (BL.unpack bs)
  where
    go :: [ Word8 ] -> Bool
    go []         = True
    go (b : rest)
      -- ASCII (0x00-0x7F)

        | b <= 0x7F = go rest
        -- 2-byte sequence (0xC0-0xDF followed by 0x80-0xBF)
        | b >= 0xC2 && b <= 0xDF = check1Continuation rest
        -- 3-byte sequence (0xE0-0xEF followed by 2 continuation bytes)
        | b >= 0xE0 && b <= 0xEF = check2Continuations b rest
        -- 4-byte sequence (0xF0-0xF4 followed by 3 continuation bytes)
        | b >= 0xF0 && b <= 0xF4 = check3Continuations b rest
        -- Invalid leading byte
        | otherwise = False

    check1Continuation :: [ Word8 ] -> Bool
    check1Continuation (c : rest)
      | c >= 0x80 && c <= 0xBF = go rest
    check1Continuation _          = False

    check2Continuations :: Word8 -> [ Word8 ] -> Bool
    check2Continuations lead (c1 : c2 : rest)
      | c1 >= 0x80 && c1 <= 0xBF && c2 >= 0x80 && c2 <= 0xBF
        =
        -- Check for overlong encodings and surrogates
        case lead of
          0xE0 -> c1 >= 0xA0 && go rest  -- Overlong check
          0xED -> c1 <= 0x9F && go rest  -- Surrogate check
          _    -> go rest
    check2Continuations _ _ = False

    check3Continuations :: Word8 -> [ Word8 ] -> Bool
    check3Continuations lead (c1 : c2 : c3 : rest)
      | c1 >= 0x80 && c1 <= 0xBF && c2 >= 0x80 && c2 <= 0xBF && c3 >= 0x80 && c3 <= 0xBF
        = case lead of
          0xF0 -> c1 >= 0x90 && go rest  -- Overlong check
          0xF4 -> c1 <= 0x8F && go rest  -- Max codepoint check
          _    -> go rest
    check3Continuations _ _ = False

-- | Check if bytes have UTF-8 BOM (Byte Order Mark).
hasUtf8Bom :: BL.ByteString -> Bool
hasUtf8Bom bs = BL.take 3 bs == BL.pack [ 0xEF, 0xBB, 0xBF ]

-- | Guess charset by analyzing byte patterns.
-- Strategy:
-- 1. Check for UTF-8 BOM
-- 2. Check for explicit charset declaration
-- 3. Try to validate as UTF-8
-- 4. Default to CP932 (Shift_JIS) for Japanese ghost files
guessCharset :: BL.ByteString -> EncodingName
guessCharset bytes
  -- UTF-8 BOM present

    | hasUtf8Bom bytes = "UTF-8"
    -- Check for explicit charset declaration
    | Just charset <- findCharsetDeclaration (BL8.lines bytes) = charset
    -- Check if it's valid UTF-8 with non-ASCII characters
    | hasNonAscii bytes && isValidUtf8 bytes = "UTF-8"
    -- Default to CP932 (Shift_JIS) - most common for older Japanese files
    | otherwise = "CP932"
  where
    hasNonAscii :: BL.ByteString -> Bool
    hasNonAscii = BL.any (> 0x7F)

-- | Find charset declaration in file lines.
findCharsetDeclaration :: [ BL.ByteString ] -> Maybe EncodingName
findCharsetDeclaration lns = case filter (BL8.isPrefixOf (BL8.pack "charset,")) lns of
  (l : _) -> let
      charset = map toLower . cleanStr . BL8.unpack . BL8.drop 8 $ l
    in 
      Just $ normalizeCharset charset
  []      -> Nothing

-- | Normalize charset name to iconv encoding name.
normalizeCharset :: String -> EncodingName
normalizeCharset charset = case charset of
  "shift_jis" -> "CP932"
  "shiftjis"  -> "CP932"
  "sjis"      -> "CP932"
  "utf-8"     -> "UTF-8"
  "utf8"      -> "UTF-8"
  "euc-jp"    -> "EUC-JP"
  "eucjp"     -> "EUC-JP"
  "ascii"     -> "UTF-8"  -- ASCII is a subset of UTF-8
  _           -> "CP932"  -- Default to CP932 for unknown charsets

-- | Detect charset from raw descript file lines.
-- Looks for "charset,<encoding>" line and normalizes to iconv encoding name.
-- If no charset declaration found, guesses based on byte patterns.
detectCharset :: [ BL.ByteString ] -> EncodingName
detectCharset = fromMaybe "CP932" . findCharsetDeclaration  -- Default to Shift_JIS for Japanese files without declaration

-- | Detect charset from raw bytes with guessing.
detectCharsetFromBytes :: BL.ByteString -> EncodingName
detectCharsetFromBytes = guessCharset

-- | Convert bytes to UTF-8, skipping conversion if already UTF-8.
convertToUtf8 :: EncodingName -> BL.ByteString -> BL.ByteString
convertToUtf8 srcEnc bytes
  | srcEnc == "UTF-8" = bytes
  | otherwise = convert srcEnc "UTF-8" bytes
