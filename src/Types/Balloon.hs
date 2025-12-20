{-# LANGUAGE OverloadedStrings #-}

module Types.Balloon ( module Types.Balloon ) where

import qualified Data.ByteString.Lazy as BL
import           Data.Map.Strict      ( Map )
import qualified Data.Map.Strict      as Map
import           Data.Text            ( Text )
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

import           Types.Ghost          ( clean
                                      , convertToUtf8
                                      , detectCharsetFromBytes
                                      , readIntOr
                                      , readMaybeBool
                                      , readMaybeInt
                                      )

-- ============================================================================
-- Helper Types
-- ============================================================================

-- | RGB color value. Each component is 0-255.
-- Some fields support "none" which is represented as Nothing.
data RGB = RGB { rgbR :: Int, rgbG :: Int, rgbB :: Int }
  deriving ( Show, Eq )

-- | Optional RGB that can be "none"
data MaybeRGB
  = RGBNone           -- ^ "none" value
  | RGBValue RGB      -- ^ Actual color value
  deriving ( Show, Eq )

-- | Shadow style for text
data ShadowStyle
  = ShadowNone        -- ^ No shadow
  | ShadowOffset      -- ^ Offset shadow (default)
  | ShadowOutline     -- ^ Outline shadow
  deriving ( Show, Eq )

-- | Marker style for selection cursor
data MarkerStyle
  = MarkerSquare            -- ^ Square marker (default)
  | MarkerUnderline         -- ^ Underline marker
  | MarkerSquareUnderline   -- ^ Square + underline combined
  | MarkerNone              -- ^ No marker
  deriving ( Show, Eq )

-- | Blend method for cursor/anchor rendering
-- Based on Win32 SetROP2 operators
data BlendMethod
  = BlendNone          -- ^ No blending
  | BlendNotMaskPen    -- ^ NOT (mask OR pen)
  | BlendMergePenNot   -- ^ pen OR (NOT mask)
-- Additional Win32 ROP2 codes can be added as needed


  deriving ( Show, Eq )

-- | Font settings used for various text elements
data FontSettings
  = FontSettings
  { fsName         :: Maybe Text      -- ^ Font name
  , fsHeight       :: Maybe Int       -- ^ Font height in pixels
  , fsColorR       :: Maybe Int       -- ^ Text color R (0-255)
  , fsColorG       :: Maybe Int       -- ^ Text color G (0-255)
  , fsColorB       :: Maybe Int       -- ^ Text color B (0-255)
  , fsShadowColorR :: Maybe Int      -- ^ Shadow color R
  , fsShadowColorG :: Maybe Int      -- ^ Shadow color G
  , fsShadowColorB :: Maybe Int      -- ^ Shadow color B
  , fsShadowStyle  :: Maybe ShadowStyle  -- ^ Shadow style (offset/outline)
  , fsBold         :: Maybe Bool      -- ^ Bold text
  , fsItalic       :: Maybe Bool      -- ^ Italic text
  , fsUnderline    :: Maybe Bool      -- ^ Underlined text
  , fsStrike       :: Maybe Bool      -- ^ Strikethrough text
  , fsOutline      :: Maybe Bool      -- ^ Outline text (different from shadow outline)
  }
  deriving ( Show, Eq )

-- | Empty font settings
emptyFontSettings :: FontSettings
emptyFontSettings
  = FontSettings
  { fsName         = Nothing
  , fsHeight       = Nothing
  , fsColorR       = Nothing
  , fsColorG       = Nothing
  , fsColorB       = Nothing
  , fsShadowColorR = Nothing
  , fsShadowColorG = Nothing
  , fsShadowColorB = Nothing
  , fsShadowStyle  = Nothing
  , fsBold         = Nothing
  , fsItalic       = Nothing
  , fsUnderline    = Nothing
  , fsStrike       = Nothing
  , fsOutline      = Nothing
  }

-- | Pen settings for GDI drawing
data PenSettings
  = PenSettings { penColorR :: Maybe Int, penColorG :: Maybe Int, penColorB :: Maybe Int }
  deriving ( Show, Eq )

emptyPenSettings :: PenSettings
emptyPenSettings = PenSettings Nothing Nothing Nothing

-- | Brush settings for GDI drawing
data BrushSettings
  = BrushSettings { brushColorR :: Maybe Int, brushColorG :: Maybe Int, brushColorB :: Maybe Int }
  deriving ( Show, Eq )

emptyBrushSettings :: BrushSettings
emptyBrushSettings = BrushSettings Nothing Nothing Nothing

-- | Cursor (selection marker) settings
data CursorSettings
  = CursorSettings { cursorStyle       :: Maybe MarkerStyle
                   , cursorBlendMethod :: Maybe BlendMethod
                   , cursorFont        :: FontSettings
                   , cursorPen         :: PenSettings
                   , cursorBrush       :: BrushSettings
                   }
  deriving ( Show, Eq )

emptyCursorSettings :: CursorSettings
emptyCursorSettings
  = CursorSettings { cursorStyle       = Nothing
                   , cursorBlendMethod = Nothing
                   , cursorFont        = emptyFontSettings
                   , cursorPen         = emptyPenSettings
                   , cursorBrush       = emptyBrushSettings
                   }

-- | Anchor (hyperlink) settings
data AnchorSettings
  = AnchorSettings { anchorStyle       :: Maybe MarkerStyle
                   , anchorBlendMethod :: Maybe BlendMethod
                   , anchorFont        :: FontSettings
                   , anchorPen         :: PenSettings
                   , anchorBrush       :: BrushSettings
                   }
  deriving ( Show, Eq )

emptyAnchorSettings :: AnchorSettings
emptyAnchorSettings
  = AnchorSettings { anchorStyle       = Nothing
                   , anchorBlendMethod = Nothing
                   , anchorFont        = emptyFontSettings
                   , anchorPen         = emptyPenSettings
                   , anchorBrush       = emptyBrushSettings
                   }

-- | Communicate box background settings
data CommunicateBoxBackground
  = CommunicateBoxBackground
  { cbbColorR :: Maybe Int, cbbColorG :: Maybe Int, cbbColorB :: Maybe Int }
  deriving ( Show, Eq )

emptyCommunicateBoxBackground :: CommunicateBoxBackground
emptyCommunicateBoxBackground = CommunicateBoxBackground Nothing Nothing Nothing

-- ============================================================================
-- BalloonDescript - Main balloon configuration from descript.txt
-- ============================================================================

data BalloonDescript
  = BalloonDescript
  { -- Basic info
    bdCharset :: Text              -- ^ Character encoding
  , bdName :: Text              -- ^ Balloon name
  , bdType :: Text              -- ^ File set type ("balloon")
  , bdId :: Maybe Text        -- ^ ID name (alphanumeric)
  , bdCraftman :: Maybe Text        -- ^ Author name (alphanumeric)
  , bdCraftmanw :: Maybe Text        -- ^ Author name (full-width allowed)
  , bdCraftmanUrl :: Maybe Text        -- ^ Author's URL
  , bdHomeUrl :: Maybe Text        -- ^ Network update URL
  , bdReadme :: Text              -- ^ Readme filename (default: readme.txt)
  , bdReadmeCharset :: Maybe Text        -- ^ Readme charset
  , bdDpi :: Int               -- ^ Recommended DPI (default: 96)
    -- Cursors
  , bdCursor :: Maybe Text        -- ^ Default cursor file
  , bdMouseCursor :: Maybe Text        -- ^ Alias for cursor
  , bdMouseCursorText :: Maybe Text        -- ^ Text input cursor
  , bdMouseCursorWait :: Maybe Text        -- ^ Wait cursor
  , bdMouseCursorArrow :: Maybe Text        -- ^ Arrow cursor
    -- Text positioning
  , bdOriginX :: Maybe Int         -- ^ Text origin X
  , bdOriginY :: Maybe Int         -- ^ Text origin Y
  , bdValidRectLeft :: Maybe Int         -- ^ Valid text area left
  , bdValidRectTop :: Maybe Int         -- ^ Valid text area top
  , bdValidRectRight :: Maybe Int         -- ^ Valid text area right (negative = from right edge)
  , bdValidRectBottom :: Maybe Int         -- ^ Valid text area bottom (negative = from bottom edge)
  , bdWordWrapPointX :: Maybe Int         -- ^ Word wrap point X
    -- Main font settings
  , bdFont :: FontSettings      -- ^ Default font settings
    -- Disabled font settings (when balloon is inactive)
  , bdDisableFont :: FontSettings      -- ^ Disabled state font settings
    -- Cursor (selection marker) settings
  , bdCursorSettings :: CursorSettings    -- ^ Selected state cursor
  , bdCursorNotSelect :: CursorSettings    -- ^ Not selected state cursor
    -- Anchor (hyperlink) settings
  , bdAnchor :: AnchorSettings    -- ^ Selected anchor
  , bdAnchorNotSelect :: AnchorSettings    -- ^ Not selected anchor
  , bdAnchorVisited :: AnchorSettings    -- ^ Visited anchor
    -- Marker positions
  , bdClickWaitMarkerX :: Maybe Int         -- ^ Click wait marker X
  , bdClickWaitMarkerY :: Maybe Int         -- ^ Click wait marker Y
  , bdSstpMarkerX :: Maybe Int         -- ^ SSTP marker X
  , bdSstpMarkerY :: Maybe Int         -- ^ SSTP marker Y
  , bdOnlineMarkerX :: Maybe Int         -- ^ Online marker X
  , bdOnlineMarkerY :: Maybe Int         -- ^ Online marker Y
  , bdOnlineMarkerInterval :: Maybe Int      -- ^ Online marker animation interval (ms)
    -- SSTP message display
  , bdSstpMessageX :: Maybe Int         -- ^ SSTP message X
  , bdSstpMessageY :: Maybe Int         -- ^ SSTP message Y
  , bdSstpMessageXr :: Maybe Int         -- ^ SSTP message X from right
  , bdSstpMessageFont :: FontSettings      -- ^ SSTP message font
    -- Number display (for counter etc.)
  , bdNumberFont :: FontSettings      -- ^ Number display font
  , bdNumberXr :: Maybe Int         -- ^ Number X from right
  , bdNumberY :: Maybe Int         -- ^ Number Y
    -- Arrow positions (scroll arrows)
  , bdArrow0X :: Maybe Int         -- ^ Arrow 0 (up) X
  , bdArrow0Y :: Maybe Int         -- ^ Arrow 0 (up) Y
  , bdArrow1X :: Maybe Int         -- ^ Arrow 1 (down) X
  , bdArrow1Y :: Maybe Int         -- ^ Arrow 1 (down) Y
    -- Communicate box settings
  , bdCommunicateBoxFont :: FontSettings              -- ^ Communicate box font
  , bdCommunicateBoxBackground :: CommunicateBoxBackground  -- ^ Communicate box background
  , bdCommunicateBoxX :: Maybe Int                 -- ^ Communicate box X
  , bdCommunicateBoxY :: Maybe Int                 -- ^ Communicate box Y
  , bdCommunicateBoxWidth :: Maybe Int                 -- ^ Communicate box width
  , bdCommunicateBoxHeight :: Maybe Int                 -- ^ Communicate box height
    -- Rendering options
  , bdPaintTransparentRegionBlack :: Maybe Bool  -- ^ Paint transparent regions black
  , bdUseSelfAlpha :: Maybe Bool  -- ^ Use image's own alpha channel
  , bdUseInputAlpha :: Maybe Bool  -- ^ Use input alpha
  , bdOverlayOutsideBalloon :: Maybe Bool  -- ^ Allow overlay outside balloon area
    -- Window position
  , bdWindowPositionX :: Maybe Int       -- ^ Window position X
  , bdWindowPositionY :: Maybe Int       -- ^ Window position Y
  , bdWindowPositionLimit :: Maybe Text      -- ^ Window position limit mode
    -- Recommended ghost
  , bdRecommendedGhost :: Maybe Text     -- ^ Recommended ghost name
  , bdRecommendedGhostPath :: Maybe Text     -- ^ Recommended ghost path
  }
  deriving ( Show, Eq )

-- | Empty BalloonDescript with defaults
emptyBalloonDescript :: BalloonDescript
emptyBalloonDescript
  = BalloonDescript
  { bdCharset = ""
  , bdName = ""
  , bdType = "balloon"
  , bdId = Nothing
  , bdCraftman = Nothing
  , bdCraftmanw = Nothing
  , bdCraftmanUrl = Nothing
  , bdHomeUrl = Nothing
  , bdReadme = "readme.txt"
  , bdReadmeCharset = Nothing
  , bdDpi = 96
  , bdCursor = Nothing
  , bdMouseCursor = Nothing
  , bdMouseCursorText = Nothing
  , bdMouseCursorWait = Nothing
  , bdMouseCursorArrow = Nothing
  , bdOriginX = Nothing
  , bdOriginY = Nothing
  , bdValidRectLeft = Nothing
  , bdValidRectTop = Nothing
  , bdValidRectRight = Nothing
  , bdValidRectBottom = Nothing
  , bdWordWrapPointX = Nothing
  , bdFont = emptyFontSettings
  , bdDisableFont = emptyFontSettings
  , bdCursorSettings = emptyCursorSettings
  , bdCursorNotSelect = emptyCursorSettings
  , bdAnchor = emptyAnchorSettings
  , bdAnchorNotSelect = emptyAnchorSettings
  , bdAnchorVisited = emptyAnchorSettings
  , bdClickWaitMarkerX = Nothing
  , bdClickWaitMarkerY = Nothing
  , bdSstpMarkerX = Nothing
  , bdSstpMarkerY = Nothing
  , bdOnlineMarkerX = Nothing
  , bdOnlineMarkerY = Nothing
  , bdOnlineMarkerInterval = Nothing
  , bdSstpMessageX = Nothing
  , bdSstpMessageY = Nothing
  , bdSstpMessageXr = Nothing
  , bdSstpMessageFont = emptyFontSettings
  , bdNumberFont = emptyFontSettings
  , bdNumberXr = Nothing
  , bdNumberY = Nothing
  , bdArrow0X = Nothing
  , bdArrow0Y = Nothing
  , bdArrow1X = Nothing
  , bdArrow1Y = Nothing
  , bdCommunicateBoxFont = emptyFontSettings
  , bdCommunicateBoxBackground = emptyCommunicateBoxBackground
  , bdCommunicateBoxX = Nothing
  , bdCommunicateBoxY = Nothing
  , bdCommunicateBoxWidth = Nothing
  , bdCommunicateBoxHeight = Nothing
  , bdPaintTransparentRegionBlack = Nothing
  , bdUseSelfAlpha = Nothing
  , bdUseInputAlpha = Nothing
  , bdOverlayOutsideBalloon = Nothing
  , bdWindowPositionX = Nothing
  , bdWindowPositionY = Nothing
  , bdWindowPositionLimit = Nothing
  , bdRecommendedGhost = Nothing
  , bdRecommendedGhostPath = Nothing
  }

-- ============================================================================
-- BalloonSurfaceOption - Per-surface overrides from balloons*s.txt etc.
-- ============================================================================

-- | Per-surface options that can override descript.txt settings
-- Used in balloons*s.txt, balloonk*s.txt, balloonc*s.txt, balloonp*def*s.txt
data BalloonSurfaceOption
  = BalloonSurfaceOption
  { -- Text positioning overrides
    bsoOriginX :: Maybe Int
  , bsoOriginY :: Maybe Int
  , bsoValidRectLeft :: Maybe Int
  , bsoValidRectTop :: Maybe Int
  , bsoValidRectRight :: Maybe Int
  , bsoValidRectBottom :: Maybe Int
  , bsoWordWrapPointX :: Maybe Int
    -- Marker positions overrides
  , bsoClickWaitMarkerX :: Maybe Int
  , bsoClickWaitMarkerY :: Maybe Int
  , bsoSstpMarkerX :: Maybe Int
  , bsoSstpMarkerY :: Maybe Int
  , bsoOnlineMarkerX :: Maybe Int
  , bsoOnlineMarkerY :: Maybe Int
    -- Arrow positions overrides
  , bsoArrow0X :: Maybe Int
  , bsoArrow0Y :: Maybe Int
  , bsoArrow1X :: Maybe Int
  , bsoArrow1Y :: Maybe Int
    -- Filename overrides (specific to surface options)
  , bsoArrowFilename :: Maybe Text  -- ^ Arrow image filename
  , bsoSstpMarkerFilename :: Maybe Text  -- ^ SSTP marker filename
  , bsoMarkerFilename :: Maybe Text  -- ^ General marker filename
  , bsoOnlineMarkerFilename :: Maybe Text  -- ^ Online marker filename
  , bsoClickWaitMarkerFilename :: Maybe Text -- ^ Click wait marker filename
  }
  deriving ( Show, Eq )

-- | Empty surface option
emptyBalloonSurfaceOption :: BalloonSurfaceOption
emptyBalloonSurfaceOption
  = BalloonSurfaceOption
  { bsoOriginX = Nothing
  , bsoOriginY = Nothing
  , bsoValidRectLeft = Nothing
  , bsoValidRectTop = Nothing
  , bsoValidRectRight = Nothing
  , bsoValidRectBottom = Nothing
  , bsoWordWrapPointX = Nothing
  , bsoClickWaitMarkerX = Nothing
  , bsoClickWaitMarkerY = Nothing
  , bsoSstpMarkerX = Nothing
  , bsoSstpMarkerY = Nothing
  , bsoOnlineMarkerX = Nothing
  , bsoOnlineMarkerY = Nothing
  , bsoArrow0X = Nothing
  , bsoArrow0Y = Nothing
  , bsoArrow1X = Nothing
  , bsoArrow1Y = Nothing
  , bsoArrowFilename = Nothing
  , bsoSstpMarkerFilename = Nothing
  , bsoMarkerFilename = Nothing
  , bsoOnlineMarkerFilename = Nothing
  , bsoClickWaitMarkerFilename = Nothing
  }

-- ============================================================================
-- Balloon - Container type
-- ============================================================================

-- | Complete balloon data structure
data Balloon
  = Balloon
  { balloonDescript      :: BalloonDescript                    -- ^ Main descript.txt settings
  , balloonSakuraOptions :: Map Int BalloonSurfaceOption       -- ^ balloons*s.txt (main character)
  , balloonKeroOptions   :: Map Int BalloonSurfaceOption       -- ^ balloonk*s.txt (partner)
  , balloonCharOptions   :: Map Int (Map Int BalloonSurfaceOption)  -- ^ balloonp*def*s.txt (char[2+])
  , balloonInputOptions  :: Map Int BalloonSurfaceOption       -- ^ balloonc*s.txt (input boxes)
  }
  deriving ( Show, Eq )

-- | Empty balloon
emptyBalloon :: Balloon
emptyBalloon
  = Balloon { balloonDescript      = emptyBalloonDescript
            , balloonSakuraOptions = Map.empty
            , balloonKeroOptions   = Map.empty
            , balloonCharOptions   = Map.empty
            , balloonInputOptions  = Map.empty
            }

-- ============================================================================
-- Parsers
-- ============================================================================

-- | Parse shadow style from text
parseShadowStyle :: Text -> Maybe ShadowStyle
parseShadowStyle txt = case T.toLower (T.strip txt) of
  "none"    -> Just ShadowNone
  "offset"  -> Just ShadowOffset
  "outline" -> Just ShadowOutline
  _         -> Nothing

-- | Parse marker style from text
parseMarkerStyle :: Text -> Maybe MarkerStyle
parseMarkerStyle txt = case T.toLower (T.strip txt) of
  "square" -> Just MarkerSquare
  "underline" -> Just MarkerUnderline
  "square+underline" -> Just MarkerSquareUnderline
  "none" -> Just MarkerNone
  _ -> Nothing

-- | Parse blend method from text
parseBlendMethod :: Text -> Maybe BlendMethod
parseBlendMethod txt = case T.toLower (T.strip txt) of
  "none" -> Just BlendNone
  "notmaskpen" -> Just BlendNotMaskPen
  "mergepennot" -> Just BlendMergePenNot
  _ -> Nothing

-- | Parse coordinate value, handling double-minus for negative values
-- In balloon specs, "--5" means actual -5 (double minus = negative)
parseCoordinate :: Text -> Maybe Int
parseCoordinate txt
  = let
      stripped = T.strip txt
    in 
      if "--" `T.isPrefixOf` stripped
        then negate <$> readMaybeInt (T.drop 2 stripped)
        else readMaybeInt stripped

-- | Read BalloonDescript from descript.txt file
readBalloonDescript :: FilePath -> IO BalloonDescript
readBalloonDescript path = do
  rawBytes <- BL.readFile path
  let detectedCharset = detectCharsetFromBytes rawBytes
      utf8Bytes       = convertToUtf8 detectedCharset rawBytes
      contents        = TE.decodeUtf8 (BL.toStrict utf8Bytes)
  return $ foldl' parseLine emptyBalloonDescript (T.lines contents)
  where
    parseLine :: BalloonDescript -> Text -> BalloonDescript
    parseLine bd line = case T.breakOn "," line of
      ( rawKey, rest )
        | not (T.null rest) -> let
            key = T.toLower (clean rawKey)
            val = clean (T.drop 1 rest)
          in 
            parseKey bd key val
      _ -> bd

    parseKey :: BalloonDescript -> Text -> Text -> BalloonDescript
    parseKey bd key val
      -- Basic info


        | key == "charset" = bd { bdCharset = val }
        | key == "name" = bd { bdName = val }
        | key == "type" = bd { bdType = val }
        | key == "id" = bd { bdId = Just val }
        | key == "craftman" = bd { bdCraftman = Just val }
        | key == "craftmanw" = bd { bdCraftmanw = Just val }
        | key == "craftmanurl" = bd { bdCraftmanUrl = Just val }
        | key == "homeurl" = bd { bdHomeUrl = Just val }
        | key == "readme" = bd { bdReadme = val }
        | key == "readme.charset" = bd { bdReadmeCharset = Just val }
        | key == "dpi" = bd { bdDpi = readIntOr 96 val }

        -- Cursors
        | key == "cursor" = bd { bdCursor = Just val }
        | key == "mousecursor" = bd { bdMouseCursor = Just val }
        | key == "mousecursor.text" = bd { bdMouseCursorText = Just val }
        | key == "mousecursor.wait" = bd { bdMouseCursorWait = Just val }
        | key == "mousecursor.arrow" = bd { bdMouseCursorArrow = Just val }

        -- Text positioning
        | key == "origin.x" = bd { bdOriginX = parseCoordinate val }
        | key == "origin.y" = bd { bdOriginY = parseCoordinate val }
        | key == "validrect.left" = bd { bdValidRectLeft = parseCoordinate val }
        | key == "validrect.top" = bd { bdValidRectTop = parseCoordinate val }
        | key == "validrect.right" = bd { bdValidRectRight = parseCoordinate val }
        | key == "validrect.bottom" = bd { bdValidRectBottom = parseCoordinate val }
        | key == "wordwrappoint.x" = bd { bdWordWrapPointX = parseCoordinate val }

        -- Main font settings
        | key == "font.name" = bd { bdFont = (bdFont bd) { fsName = Just val } }
        | key == "font.height" = bd { bdFont = (bdFont bd) { fsHeight = readMaybeInt val } }
        | key == "font.color.r" = bd { bdFont = (bdFont bd) { fsColorR = readMaybeInt val } }
        | key == "font.color.g" = bd { bdFont = (bdFont bd) { fsColorG = readMaybeInt val } }
        | key == "font.color.b" = bd { bdFont = (bdFont bd) { fsColorB = readMaybeInt val } }
        | key == "font.shadowcolor.r"
          = bd { bdFont = (bdFont bd) { fsShadowColorR = readMaybeInt val } }
        | key == "font.shadowcolor.g"
          = bd { bdFont = (bdFont bd) { fsShadowColorG = readMaybeInt val } }
        | key == "font.shadowcolor.b"
          = bd { bdFont = (bdFont bd) { fsShadowColorB = readMaybeInt val } }
        | key == "font.shadowstyle"
          = bd { bdFont = (bdFont bd) { fsShadowStyle = parseShadowStyle val } }
        | key == "font.bold" = bd { bdFont = (bdFont bd) { fsBold = readMaybeBool val } }
        | key == "font.italic" = bd { bdFont = (bdFont bd) { fsItalic = readMaybeBool val } }
        | key == "font.underline" = bd { bdFont = (bdFont bd) { fsUnderline = readMaybeBool val } }
        | key == "font.strike" = bd { bdFont = (bdFont bd) { fsStrike = readMaybeBool val } }
        | key == "font.outline" = bd { bdFont = (bdFont bd) { fsOutline = readMaybeBool val } }

        -- Disabled font settings
        | key == "disable.font.name"
          = bd { bdDisableFont = (bdDisableFont bd) { fsName = Just val } }
        | key == "disable.font.height"
          = bd { bdDisableFont = (bdDisableFont bd) { fsHeight = readMaybeInt val } }
        | key == "disable.font.color.r"
          = bd { bdDisableFont = (bdDisableFont bd) { fsColorR = readMaybeInt val } }
        | key == "disable.font.color.g"
          = bd { bdDisableFont = (bdDisableFont bd) { fsColorG = readMaybeInt val } }
        | key == "disable.font.color.b"
          = bd { bdDisableFont = (bdDisableFont bd) { fsColorB = readMaybeInt val } }

        -- Cursor (selection marker) settings
        | key == "cursor.style"
          = bd { bdCursorSettings = (bdCursorSettings bd) { cursorStyle = parseMarkerStyle val } }
        | key == "cursor.blendmethod"
          = bd
          { bdCursorSettings = (bdCursorSettings bd) { cursorBlendMethod = parseBlendMethod val } }
        | key == "cursor.font.name"
          = bd { bdCursorSettings
                   = updateCursorFont (bdCursorSettings bd) (\f -> f { fsName = Just val })
               }
        | key == "cursor.font.color.r"
          = bd
          { bdCursorSettings
              = updateCursorFont (bdCursorSettings bd) (\f -> f { fsColorR = readMaybeInt val })
          }
        | key == "cursor.font.color.g"
          = bd
          { bdCursorSettings
              = updateCursorFont (bdCursorSettings bd) (\f -> f { fsColorG = readMaybeInt val })
          }
        | key == "cursor.font.color.b"
          = bd
          { bdCursorSettings
              = updateCursorFont (bdCursorSettings bd) (\f -> f { fsColorB = readMaybeInt val })
          }
        | key == "cursor.pen.color.r"
          = bd
          { bdCursorSettings
              = updateCursorPen (bdCursorSettings bd) (\p -> p { penColorR = readMaybeInt val })
          }
        | key == "cursor.pen.color.g"
          = bd
          { bdCursorSettings
              = updateCursorPen (bdCursorSettings bd) (\p -> p { penColorG = readMaybeInt val })
          }
        | key == "cursor.pen.color.b"
          = bd
          { bdCursorSettings
              = updateCursorPen (bdCursorSettings bd) (\p -> p { penColorB = readMaybeInt val })
          }
        | key == "cursor.brush.color.r"
          = bd { bdCursorSettings = updateCursorBrush
                   (bdCursorSettings bd)
                   (\b -> b { brushColorR = readMaybeInt val })
               }
        | key == "cursor.brush.color.g"
          = bd { bdCursorSettings = updateCursorBrush
                   (bdCursorSettings bd)
                   (\b -> b { brushColorG = readMaybeInt val })
               }
        | key == "cursor.brush.color.b"
          = bd { bdCursorSettings = updateCursorBrush
                   (bdCursorSettings bd)
                   (\b -> b { brushColorB = readMaybeInt val })
               }

        -- Cursor not selected
        | key == "cursor.notselect.font.color.r"
          = bd
          { bdCursorNotSelect
              = updateCursorFont (bdCursorNotSelect bd) (\f -> f { fsColorR = readMaybeInt val })
          }
        | key == "cursor.notselect.font.color.g"
          = bd
          { bdCursorNotSelect
              = updateCursorFont (bdCursorNotSelect bd) (\f -> f { fsColorG = readMaybeInt val })
          }
        | key == "cursor.notselect.font.color.b"
          = bd
          { bdCursorNotSelect
              = updateCursorFont (bdCursorNotSelect bd) (\f -> f { fsColorB = readMaybeInt val })
          }
        | key == "cursor.notselect.pen.color.r"
          = bd
          { bdCursorNotSelect
              = updateCursorPen (bdCursorNotSelect bd) (\p -> p { penColorR = readMaybeInt val })
          }
        | key == "cursor.notselect.pen.color.g"
          = bd
          { bdCursorNotSelect
              = updateCursorPen (bdCursorNotSelect bd) (\p -> p { penColorG = readMaybeInt val })
          }
        | key == "cursor.notselect.pen.color.b"
          = bd
          { bdCursorNotSelect
              = updateCursorPen (bdCursorNotSelect bd) (\p -> p { penColorB = readMaybeInt val })
          }
        | key == "cursor.notselect.brush.color.r"
          = bd { bdCursorNotSelect = updateCursorBrush
                   (bdCursorNotSelect bd)
                   (\b -> b { brushColorR = readMaybeInt val })
               }
        | key == "cursor.notselect.brush.color.g"
          = bd { bdCursorNotSelect = updateCursorBrush
                   (bdCursorNotSelect bd)
                   (\b -> b { brushColorG = readMaybeInt val })
               }
        | key == "cursor.notselect.brush.color.b"
          = bd { bdCursorNotSelect = updateCursorBrush
                   (bdCursorNotSelect bd)
                   (\b -> b { brushColorB = readMaybeInt val })
               }

        -- Anchor settings
        | key == "anchor.style"
          = bd { bdAnchor = (bdAnchor bd) { anchorStyle = parseMarkerStyle val } }
        | key == "anchor.blendmethod"
          = bd { bdAnchor = (bdAnchor bd) { anchorBlendMethod = parseBlendMethod val } }
        | key == "anchor.font.name"
          = bd { bdAnchor = updateAnchorFont (bdAnchor bd) (\f -> f { fsName = Just val }) }
        | key == "anchor.font.color.r"
          = bd
          { bdAnchor = updateAnchorFont (bdAnchor bd) (\f -> f { fsColorR = readMaybeInt val }) }
        | key == "anchor.font.color.g"
          = bd
          { bdAnchor = updateAnchorFont (bdAnchor bd) (\f -> f { fsColorG = readMaybeInt val }) }
        | key == "anchor.font.color.b"
          = bd
          { bdAnchor = updateAnchorFont (bdAnchor bd) (\f -> f { fsColorB = readMaybeInt val }) }
        | key == "anchor.font.underline"
          = bd { bdAnchor = updateAnchorFont (bdAnchor bd) (\f -> f
                                                            { fsUnderline = readMaybeBool val }) }
        | key == "anchor.pen.color.r"
          = bd
          { bdAnchor = updateAnchorPen (bdAnchor bd) (\p -> p { penColorR = readMaybeInt val }) }
        | key == "anchor.pen.color.g"
          = bd
          { bdAnchor = updateAnchorPen (bdAnchor bd) (\p -> p { penColorG = readMaybeInt val }) }
        | key == "anchor.pen.color.b"
          = bd
          { bdAnchor = updateAnchorPen (bdAnchor bd) (\p -> p { penColorB = readMaybeInt val }) }
        | key == "anchor.brush.color.r"
          = bd { bdAnchor = updateAnchorBrush (bdAnchor bd) (\b -> b
                                                             { brushColorR = readMaybeInt val }) }
        | key == "anchor.brush.color.g"
          = bd { bdAnchor = updateAnchorBrush (bdAnchor bd) (\b -> b
                                                             { brushColorG = readMaybeInt val }) }
        | key == "anchor.brush.color.b"
          = bd { bdAnchor = updateAnchorBrush (bdAnchor bd) (\b -> b
                                                             { brushColorB = readMaybeInt val }) }

        -- Anchor not selected
        | key == "anchor.notselect.font.color.r"
          = bd
          { bdAnchorNotSelect
              = updateAnchorFont (bdAnchorNotSelect bd) (\f -> f { fsColorR = readMaybeInt val })
          }
        | key == "anchor.notselect.font.color.g"
          = bd
          { bdAnchorNotSelect
              = updateAnchorFont (bdAnchorNotSelect bd) (\f -> f { fsColorG = readMaybeInt val })
          }
        | key == "anchor.notselect.font.color.b"
          = bd
          { bdAnchorNotSelect
              = updateAnchorFont (bdAnchorNotSelect bd) (\f -> f { fsColorB = readMaybeInt val })
          }

        -- Anchor visited
        | key == "anchor.visited.font.color.r"
          = bd
          { bdAnchorVisited
              = updateAnchorFont (bdAnchorVisited bd) (\f -> f { fsColorR = readMaybeInt val })
          }
        | key == "anchor.visited.font.color.g"
          = bd
          { bdAnchorVisited
              = updateAnchorFont (bdAnchorVisited bd) (\f -> f { fsColorG = readMaybeInt val })
          }
        | key == "anchor.visited.font.color.b"
          = bd
          { bdAnchorVisited
              = updateAnchorFont (bdAnchorVisited bd) (\f -> f { fsColorB = readMaybeInt val })
          }

        -- Marker positions
        | key == "clickwaitmarker.x" = bd { bdClickWaitMarkerX = parseCoordinate val }
        | key == "clickwaitmarker.y" = bd { bdClickWaitMarkerY = parseCoordinate val }
        | key == "sstpmarker.x" = bd { bdSstpMarkerX = parseCoordinate val }
        | key == "sstpmarker.y" = bd { bdSstpMarkerY = parseCoordinate val }
        | key == "onlinemarker.x" = bd { bdOnlineMarkerX = parseCoordinate val }
        | key == "onlinemarker.y" = bd { bdOnlineMarkerY = parseCoordinate val }
        | key == "onlinemarker.interval" = bd { bdOnlineMarkerInterval = readMaybeInt val }

        -- SSTP message
        | key == "sstpmessage.x" = bd { bdSstpMessageX = parseCoordinate val }
        | key == "sstpmessage.y" = bd { bdSstpMessageY = parseCoordinate val }
        | key == "sstpmessage.xr" = bd { bdSstpMessageXr = parseCoordinate val }
        | key == "sstpmessage.font.name"
          = bd { bdSstpMessageFont = (bdSstpMessageFont bd) { fsName = Just val } }
        | key == "sstpmessage.font.height"
          = bd { bdSstpMessageFont = (bdSstpMessageFont bd) { fsHeight = readMaybeInt val } }
        | key == "sstpmessage.font.color.r"
          = bd { bdSstpMessageFont = (bdSstpMessageFont bd) { fsColorR = readMaybeInt val } }
        | key == "sstpmessage.font.color.g"
          = bd { bdSstpMessageFont = (bdSstpMessageFont bd) { fsColorG = readMaybeInt val } }
        | key == "sstpmessage.font.color.b"
          = bd { bdSstpMessageFont = (bdSstpMessageFont bd) { fsColorB = readMaybeInt val } }

        -- Number display
        | key == "number.font.name" = bd { bdNumberFont = (bdNumberFont bd) { fsName = Just val } }
        | key == "number.font.height"
          = bd { bdNumberFont = (bdNumberFont bd) { fsHeight = readMaybeInt val } }
        | key == "number.font.color.r"
          = bd { bdNumberFont = (bdNumberFont bd) { fsColorR = readMaybeInt val } }
        | key == "number.font.color.g"
          = bd { bdNumberFont = (bdNumberFont bd) { fsColorG = readMaybeInt val } }
        | key == "number.font.color.b"
          = bd { bdNumberFont = (bdNumberFont bd) { fsColorB = readMaybeInt val } }
        | key == "number.xr" = bd { bdNumberXr = parseCoordinate val }
        | key == "number.y" = bd { bdNumberY = parseCoordinate val }

        -- Arrows
        | key == "arrow0.x" = bd { bdArrow0X = parseCoordinate val }
        | key == "arrow0.y" = bd { bdArrow0Y = parseCoordinate val }
        | key == "arrow1.x" = bd { bdArrow1X = parseCoordinate val }
        | key == "arrow1.y" = bd { bdArrow1Y = parseCoordinate val }

        -- Communicate box
        | key == "communicatebox.font.name"
          = bd { bdCommunicateBoxFont = (bdCommunicateBoxFont bd) { fsName = Just val } }
        | key == "communicatebox.font.height"
          = bd { bdCommunicateBoxFont = (bdCommunicateBoxFont bd) { fsHeight = readMaybeInt val } }
        | key == "communicatebox.font.color.r"
          = bd { bdCommunicateBoxFont = (bdCommunicateBoxFont bd) { fsColorR = readMaybeInt val } }
        | key == "communicatebox.font.color.g"
          = bd { bdCommunicateBoxFont = (bdCommunicateBoxFont bd) { fsColorG = readMaybeInt val } }
        | key == "communicatebox.font.color.b"
          = bd { bdCommunicateBoxFont = (bdCommunicateBoxFont bd) { fsColorB = readMaybeInt val } }
        | key == "communicatebox.background.r"
          = bd { bdCommunicateBoxBackground = (bdCommunicateBoxBackground bd)
                   { cbbColorR              = readMaybeInt val }
               }
        | key == "communicatebox.background.g"
          = bd { bdCommunicateBoxBackground = (bdCommunicateBoxBackground bd)
                   { cbbColorG              = readMaybeInt val }
               }
        | key == "communicatebox.background.b"
          = bd { bdCommunicateBoxBackground = (bdCommunicateBoxBackground bd)
                   { cbbColorB              = readMaybeInt val }
               }
        | key == "communicatebox.x" = bd { bdCommunicateBoxX = parseCoordinate val }
        | key == "communicatebox.y" = bd { bdCommunicateBoxY = parseCoordinate val }
        | key == "communicatebox.width" = bd { bdCommunicateBoxWidth = readMaybeInt val }
        | key == "communicatebox.height" = bd { bdCommunicateBoxHeight = readMaybeInt val }

        -- Rendering options
        | key == "paint_transparent_region_black"
          = bd { bdPaintTransparentRegionBlack = readMaybeBool val }
        | key == "use_self_alpha" = bd { bdUseSelfAlpha = readMaybeBool val }
        | key == "use_input_alpha" = bd { bdUseInputAlpha = readMaybeBool val }
        | key == "overlay_outside_balloon" = bd { bdOverlayOutsideBalloon = readMaybeBool val }

        -- Window position
        | key == "windowposition.x" = bd { bdWindowPositionX = parseCoordinate val }
        | key == "windowposition.y" = bd { bdWindowPositionY = parseCoordinate val }
        | key == "windowposition.limit" = bd { bdWindowPositionLimit = Just val }

        -- Recommended ghost
        | key == "recommended.ghost" = bd { bdRecommendedGhost = Just val }
        | key == "recommended.ghost.path" = bd { bdRecommendedGhostPath = Just val }

        | otherwise = bd

    -- Helper functions for nested updates
    updateCursorFont :: CursorSettings -> (FontSettings -> FontSettings) -> CursorSettings
    updateCursorFont cs f = cs { cursorFont = f (cursorFont cs) }

    updateCursorPen :: CursorSettings -> (PenSettings -> PenSettings) -> CursorSettings
    updateCursorPen cs f = cs { cursorPen = f (cursorPen cs) }

    updateCursorBrush :: CursorSettings -> (BrushSettings -> BrushSettings) -> CursorSettings
    updateCursorBrush cs f = cs { cursorBrush = f (cursorBrush cs) }

    updateAnchorFont :: AnchorSettings -> (FontSettings -> FontSettings) -> AnchorSettings
    updateAnchorFont as f = as { anchorFont = f (anchorFont as) }

    updateAnchorPen :: AnchorSettings -> (PenSettings -> PenSettings) -> AnchorSettings
    updateAnchorPen as f = as { anchorPen = f (anchorPen as) }

    updateAnchorBrush :: AnchorSettings -> (BrushSettings -> BrushSettings) -> AnchorSettings
    updateAnchorBrush as f = as { anchorBrush = f (anchorBrush as) }

-- | Read BalloonSurfaceOption from a surface option file (balloons*s.txt etc.)
readBalloonSurfaceOption :: FilePath -> IO BalloonSurfaceOption
readBalloonSurfaceOption path = do
  rawBytes <- BL.readFile path
  let detectedCharset = detectCharsetFromBytes rawBytes
      utf8Bytes       = convertToUtf8 detectedCharset rawBytes
      contents        = TE.decodeUtf8 (BL.toStrict utf8Bytes)
  return $ foldl' parseLine emptyBalloonSurfaceOption (T.lines contents)
  where
    parseLine :: BalloonSurfaceOption -> Text -> BalloonSurfaceOption
    parseLine bso line = case T.breakOn "," line of
      ( rawKey, rest )
        | not (T.null rest) -> let
            key = T.toLower (clean rawKey)
            val = clean (T.drop 1 rest)
          in 
            parseKey bso key val
      _ -> bso

    parseKey :: BalloonSurfaceOption -> Text -> Text -> BalloonSurfaceOption
    parseKey bso key val
      -- Text positioning


        | key == "origin.x" = bso { bsoOriginX = parseCoordinate val }
        | key == "origin.y" = bso { bsoOriginY = parseCoordinate val }
        | key == "validrect.left" = bso { bsoValidRectLeft = parseCoordinate val }
        | key == "validrect.top" = bso { bsoValidRectTop = parseCoordinate val }
        | key == "validrect.right" = bso { bsoValidRectRight = parseCoordinate val }
        | key == "validrect.bottom" = bso { bsoValidRectBottom = parseCoordinate val }
        | key == "wordwrappoint.x" = bso { bsoWordWrapPointX = parseCoordinate val }

        -- Marker positions
        | key == "clickwaitmarker.x" = bso { bsoClickWaitMarkerX = parseCoordinate val }
        | key == "clickwaitmarker.y" = bso { bsoClickWaitMarkerY = parseCoordinate val }
        | key == "sstpmarker.x" = bso { bsoSstpMarkerX = parseCoordinate val }
        | key == "sstpmarker.y" = bso { bsoSstpMarkerY = parseCoordinate val }
        | key == "onlinemarker.x" = bso { bsoOnlineMarkerX = parseCoordinate val }
        | key == "onlinemarker.y" = bso { bsoOnlineMarkerY = parseCoordinate val }

        -- Arrows
        | key == "arrow0.x" = bso { bsoArrow0X = parseCoordinate val }
        | key == "arrow0.y" = bso { bsoArrow0Y = parseCoordinate val }
        | key == "arrow1.x" = bso { bsoArrow1X = parseCoordinate val }
        | key == "arrow1.y" = bso { bsoArrow1Y = parseCoordinate val }

        -- Filename overrides
        | key == "arrow.filename" = bso { bsoArrowFilename = Just val }
        | key == "sstpmarker.filename" = bso { bsoSstpMarkerFilename = Just val }
        | key == "marker.filename" = bso { bsoMarkerFilename = Just val }
        | key == "onlinemarker.filename" = bso { bsoOnlineMarkerFilename = Just val }
        | key == "clickwaitmarker.filename" = bso { bsoClickWaitMarkerFilename = Just val }

        | otherwise = bso
