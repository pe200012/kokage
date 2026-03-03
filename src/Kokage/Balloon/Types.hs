{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Balloon runtime type definitions.
module Kokage.Balloon.Types
  ( -- * Choice types
    BalloonChoice(..)
    -- * Direction
  , BalloonDirection(..)
    -- * Text segments
  , TextSegment(..)
    -- * Configuration
  , BalloonConfig(..)
  , defaultBalloonConfig
  , configFromDescript
    -- * State
  , BalloonState(..)
  ) where

import Prelude ()
import Relude

import qualified Data.Text       as T

import qualified GI.Cairo.Render as Cairo
import qualified GI.GdkPixbuf    as Pixbuf
import qualified GI.Gtk          as Gtk

import           Types.Balloon   ( BalloonDescript(..), FontSettings(..), ShadowStyle(..) )

-- | A choice presented in the balloon that the user can click.
data BalloonChoice
  = BalloonChoice
  { bcText   :: !T.Text   -- ^ Display text for the choice
  , bcId     :: !T.Text   -- ^ Choice ID (for \q[id,text,action] style)
  , bcAction :: !T.Text   -- ^ Action to take when clicked (event ID, script, or URL)
  }
  deriving ( Show, Eq )

-- | Balloon direction relative to the character.
-- This determines which side of the character the balloon appears on.
data BalloonDirection
  = BalloonLeft   -- ^ Balloon appears to the left of the character
  | BalloonRight  -- ^ Balloon appears to the right of the character
  deriving ( Show, Eq )

-- | A text segment with its style at the time of addition.
-- This allows different parts of the text to have different styles.
data TextSegment
  = TextSegment
  { tsText      :: !T.Text     -- ^ The text content
  , tsBold      :: !Bool       -- ^ Bold style
  , tsItalic    :: !Bool       -- ^ Italic style
  , tsUnderline :: !Bool       -- ^ Underline style
  , tsStrike    :: !Bool       -- ^ Strikethrough style
  , tsSub       :: !Bool       -- ^ Subscript mode
  , tsSup       :: !Bool       -- ^ Superscript mode
  , tsFontSize  :: !Int        -- ^ Font size
  , tsFontName  :: !T.Text     -- ^ Font name
  , tsColorR    :: !Double     -- ^ Text color R
  , tsColorG    :: !Double     -- ^ Text color G
  , tsColorB    :: !Double     -- ^ Text color B
  }
  deriving ( Show, Eq )

-- | Configuration for balloon rendering.
-- Default values match typical ukagaka balloon settings.
data BalloonConfig
  = BalloonConfig
  { bcfOriginX       :: !Int        -- ^ Text origin X (default: 10)
  , bcfOriginY       :: !Int        -- ^ Text origin Y (default: 10)
  , bcfValidWidth    :: !Int        -- ^ Text area width (default: 280)
  , bcfValidHeight   :: !Int        -- ^ Text area height (default: 130)
  , bcfFontName      :: !T.Text     -- ^ Font name (default: "Sans")
  , bcfFontSize      :: !Int        -- ^ Font size in pixels (default: 12)
  , bcfTextColorR    :: !Double     -- ^ Text color R (0.0-1.0, default: 0.2)
  , bcfTextColorG    :: !Double     -- ^ Text color G (0.0-1.0, default: 0.2)
  , bcfTextColorB    :: !Double     -- ^ Text color B (0.0-1.0, default: 0.2)
  , bcfBgColorR      :: !Double     -- ^ Background color R (default: 1.0)
  , bcfBgColorG      :: !Double     -- ^ Background color G (default: 1.0)
  , bcfBgColorB      :: !Double     -- ^ Background color B (default: 0.94)
  , bcfBgAlpha       :: !Double     -- ^ Background alpha (default: 0.95)
  , bcfLineSpacing   :: !Int        -- ^ Line spacing in pixels (default: 2)
  , bcfFontBold      :: !Bool       -- ^ Bold text
  , bcfFontItalic    :: !Bool       -- ^ Italic text
  , bcfFontUnderline :: !Bool       -- ^ Underline text
  , bcfFontStrike    :: !Bool       -- ^ Strikethrough text
  , bcfShadowStyle   :: !ShadowStyle -- ^ Shadow style
  , bcfShadowColorR  :: !Double     -- ^ Shadow color R
  , bcfShadowColorG  :: !Double     -- ^ Shadow color G
  , bcfShadowColorB  :: !Double     -- ^ Shadow color B
  }
  deriving ( Show, Eq )

-- | Default balloon configuration.
defaultBalloonConfig :: BalloonConfig
defaultBalloonConfig
  = BalloonConfig
  { bcfOriginX       = 10
  , bcfOriginY       = 10
  , bcfValidWidth    = 280
  , bcfValidHeight   = 130
  , bcfFontName      = "Sans"
  , bcfFontSize      = 12
  , bcfTextColorR    = 0.2
  , bcfTextColorG    = 0.2
  , bcfTextColorB    = 0.2
  , bcfBgColorR      = 1.0
  , bcfBgColorG      = 1.0
  , bcfBgColorB      = 0.94
  , bcfBgAlpha       = 0.95
  , bcfLineSpacing   = 2
  , bcfFontBold      = False
  , bcfFontItalic    = False
  , bcfFontUnderline = False
  , bcfFontStrike    = False
  , bcfShadowStyle   = ShadowNone
  , bcfShadowColorR  = 0.8
  , bcfShadowColorG  = 0.8
  , bcfShadowColorB  = 0.8
  }

-- | Create BalloonConfig from BalloonDescript and image dimensions.
--
-- The text area is calculated according to ukadoc:
-- - X = origin.x + validrect.left
-- - Y = origin.y + validrect.top
-- - Width = (image_width + validrect.right) - X - origin.x
--   - Or use wordwrappoint.x if specified (negative value from right edge)
-- - Height = (image_height + validrect.bottom) - Y - origin.y
configFromDescript :: BalloonDescript -> Int -> Int -> BalloonConfig
configFromDescript bd imgWidth imgHeight
  = BalloonConfig
  { bcfOriginX       = originX
  , bcfOriginY       = originY
  , bcfValidWidth    = validWidth
  , bcfValidHeight   = validHeight
  , bcfFontName      = fromMaybe "Sans" (fsName (bdFont bd))
  , bcfFontSize      = fromMaybe 12 (fsHeight (bdFont bd))
  , bcfTextColorR    = maybe 0.2 (\v -> fromIntegral v / 255.0) (fsColorR (bdFont bd))
  , bcfTextColorG    = maybe 0.2 (\v -> fromIntegral v / 255.0) (fsColorG (bdFont bd))
  , bcfTextColorB    = maybe 0.2 (\v -> fromIntegral v / 255.0) (fsColorB (bdFont bd))
  , bcfBgColorR      = 1.0
  , bcfBgColorG      = 1.0
  , bcfBgColorB      = 0.94
  , bcfBgAlpha       = 0.95
  , bcfLineSpacing   = 2
  , bcfFontBold      = fromMaybe False (fsBold (bdFont bd))
  , bcfFontItalic    = fromMaybe False (fsItalic (bdFont bd))
  , bcfFontUnderline = fromMaybe False (fsUnderline (bdFont bd))
  , bcfFontStrike    = fromMaybe False (fsStrike (bdFont bd))
  , bcfShadowStyle   = fromMaybe ShadowNone (fsShadowStyle (bdFont bd))
  , bcfShadowColorR  = maybe 0.8 (\v -> fromIntegral v / 255.0) (fsShadowColorR (bdFont bd))
  , bcfShadowColorG  = maybe 0.8 (\v -> fromIntegral v / 255.0) (fsShadowColorG (bdFont bd))
  , bcfShadowColorB  = maybe 0.8 (\v -> fromIntegral v / 255.0) (fsShadowColorB (bdFont bd))
  }
  where
    originX         = fromMaybe 10 (bdOriginX bd)

    originY         = fromMaybe 10 (bdOriginY bd)

    validRectLeft   = fromMaybe 0 (bdValidRectLeft bd)

    validRectTop    = fromMaybe 0 (bdValidRectTop bd)

    validRectRight  = fromMaybe 0 (bdValidRectRight bd)

    validRectBottom = fromMaybe 0 (bdValidRectBottom bd)

    textAreaX       = originX + validRectLeft

    textAreaY       = originY + validRectTop

    validWidth      = case bdWordWrapPointX bd of
      Just wwpX -> imgWidth + wwpX - textAreaX
      Nothing   -> imgWidth + validRectRight - textAreaX - originX

    validHeight     = imgHeight + validRectBottom - textAreaY - originY

-- | State for a balloon window.
data BalloonState
  = BalloonState
  { bsWindow         :: !Gtk.Window                                              -- ^ The balloon window
  , bsDrawArea       :: !Gtk.DrawingArea                                         -- ^ Drawing area for balloon content
  , bsConfig         :: !(IORef BalloonConfig)                                   -- ^ Balloon configuration
  , bsText           :: !(IORef [ TextSegment ])                                   -- ^ Current text segments with styles
  , bsScrollLine     :: !(IORef Int)                                             -- ^ Current scroll line (0 = top)
  , bsSurface        :: !(IORef (Maybe Pixbuf.Pixbuf))                           -- ^ Current balloon surface image
  , bsCairoSurface   :: !(IORef (Maybe Cairo.Surface))                           -- ^ Cached Cairo surface for drawing
  , bsVisible        :: !(IORef Bool)                                            -- ^ Whether balloon is visible
  , bsLayerShell     :: !(IORef Bool)                                            -- ^ Whether layer-shell was initialized
  , bsChoices        :: !(IORef [ BalloonChoice ])                                 -- ^ Current choices to display
  , bsChoiceCallback :: !(IORef (Maybe (BalloonChoice -> IO ())))                -- ^ Callback when choice is selected
  , bsChoiceRects    :: !(IORef [ ( BalloonChoice, Double, Double, Double, Double ) ]) -- ^ Choice hit boxes (choice, x, y, w, h)
  , bsBalloonDir     :: !(IORef (Maybe FilePath))                                -- ^ Balloon directory path for surface loading
  , bsCharType       :: !(IORef T.Text)                                          -- ^ Character type: "s" (sakura), "k" (kero), "c" (communicate)
  , bsBalloonId      :: !(IORef Int)                                             -- ^ Current balloon ID: 0=default, 1=choice surface, etc.
  , bsPosition       :: !(IORef ( Int, Int ))                                      -- ^ Current balloon position (x, y)
  , bsAutoScroll     :: !(IORef Bool)                                            -- ^ Whether to auto-scroll when text overflows (default: True)
  , bsDescript       :: !(IORef (Maybe BalloonDescript))                         -- ^ Balloon descript.txt settings
  , bsDirection      :: !(IORef Int)                                             -- ^ Balloon direction: 0=left of char, 1=right of char
  }
