{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Balloon window for displaying text from SHIORI responses.
-- This implementation uses Cairo for rendering balloon surfaces and Pango for text
module Kokage.Balloon
  ( -- * Balloon State
    BalloonState(..)  -- Export all fields for FRP network access
  , BalloonConfig(..)
  , BalloonDirection(..)
  , defaultBalloonConfig
  , newBalloonState
  , newBalloonStateWithConfig
  , newBalloonStateWithSurface
    -- * Balloon Operations
  , showBalloon
  , hideBalloon
  , clearBalloon
  , appendText
  , appendChar
  , appendNewline
  , appendNewlineHalf
  , appendNewlinePercent
  , clearChars
  , moveCursor
  , setBalloonSurface
  , loadAndSetBalloonSurface
  , setBalloonId
  , setBalloonPosition
  , updateBalloonPosition
  , updateBalloonPositionWithChar
  , getBalloonSize
  , getBalloonDir
  , initBalloonAlwaysOnTop
  , scrollUp
  , scrollDown
  , setAutoScroll
  , getAutoScroll
    -- * Font Operations
  , setFontName
  , setFontSize
  , getFontSize
  , setFontColor
  , setFontBold
  , setFontItalic
  , setFontUnderline
  , setFontStrike
  , setFontSub
  , setFontSup
  , resetFont
    -- * Choice Support
  , BalloonChoice(..)
  , addChoice
  , clearChoices
  , hasChoices
  , setChoiceCallback
    -- * SakuraScript Text Extraction
  , extractPlainText
  ) where

import qualified Data.ByteString           as BS
import           Data.GI.Base              ( AttrOp((:=)), new )
import qualified Data.GI.Base              as GI
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE

import           Foreign.Ptr               ( castPtr )

import qualified GI.Cairo.Render           as Cairo
import qualified GI.Cairo.Render.Connector as Cairo ( getContext, renderWithContext )
import qualified GI.GLib                   as GLib
import qualified GI.Gdk                    as Gdk
import qualified GI.GdkPixbuf              as Pixbuf
import qualified GI.Gtk                    as Gtk
import qualified GI.Pango                  as Pango
import qualified GI.PangoCairo             as PangoCairo

import           Kokage.Balloon.Surface    ( loadBalloonSurface )
import           Kokage.Platform           ( Layer(..)
                                           , initPlatformWindow
                                           , setWindowAlwaysOnTop
                                           , setWindowLayer
                                           , setWindowPosition
                                           )

import           System.Directory          ( doesFileExist )
import           System.FilePath           ( (</>) )

import           Types.Balloon             ( BalloonDescript(..)
                                           , FontSettings(..)
                                           , ShadowStyle(..)
                                           , readBalloonDescript
                                           )
import           Types.SakuraScript        ( BalloonCmd(..), SakuraScript(..), Script )

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
  { bcOriginX       :: !Int        -- ^ Text origin X (default: 10)
  , bcOriginY       :: !Int        -- ^ Text origin Y (default: 10)
  , bcValidWidth    :: !Int        -- ^ Text area width (default: 280)
  , bcValidHeight   :: !Int        -- ^ Text area height (default: 130)
  , bcFontName      :: !T.Text     -- ^ Font name (default: "Sans")
  , bcFontSize      :: !Int        -- ^ Font size in pixels (default: 12)
  , bcBaseFontSize  :: !Int        -- ^ Base font size for reset (default: 12)
  , bcTextColorR    :: !Double     -- ^ Text color R (0.0-1.0, default: 0.2)
  , bcTextColorG    :: !Double     -- ^ Text color G (0.0-1.0, default: 0.2)
  , bcTextColorB    :: !Double     -- ^ Text color B (0.0-1.0, default: 0.2)
  , bcBgColorR      :: !Double     -- ^ Background color R (default: 1.0)
  , bcBgColorG      :: !Double     -- ^ Background color G (default: 1.0)
  , bcBgColorB      :: !Double     -- ^ Background color B (default: 0.94)
  , bcBgAlpha       :: !Double     -- ^ Background alpha (default: 0.95)
  , bcLineSpacing   :: !Int        -- ^ Line spacing in pixels (default: 2)
  , bcFontBold      :: !Bool       -- ^ Bold text
  , bcFontItalic    :: !Bool       -- ^ Italic text
  , bcFontUnderline :: !Bool     -- ^ Underline text
  , bcFontStrike    :: !Bool       -- ^ Strikethrough text
  , bcFontSub       :: !Bool       -- ^ Subscript mode
  , bcFontSup       :: !Bool       -- ^ Superscript mode
  , bcShadowStyle   :: !ShadowStyle -- ^ Shadow style
  , bcShadowColorR  :: !Double    -- ^ Shadow color R
  , bcShadowColorG  :: !Double    -- ^ Shadow color G
  , bcShadowColorB  :: !Double    -- ^ Shadow color B
  }
  deriving ( Show, Eq )

-- | Default balloon configuration.
defaultBalloonConfig :: BalloonConfig
defaultBalloonConfig
  = BalloonConfig
  { bcOriginX       = 10
  , bcOriginY       = 10
  , bcValidWidth    = 280
  , bcValidHeight   = 130
  , bcFontName      = "Sans"
  , bcFontSize      = 9
  , bcBaseFontSize  = 9
  , bcTextColorR    = 0.2
  , bcTextColorG    = 0.2
  , bcTextColorB    = 0.2
  , bcBgColorR      = 1.0
  , bcBgColorG      = 1.0
  , bcBgColorB      = 0.94
  , bcBgAlpha       = 0.95
  , bcLineSpacing   = 2
  , bcFontBold      = False
  , bcFontItalic    = False
  , bcFontUnderline = False
  , bcFontStrike    = False
  , bcFontSub       = False
  , bcFontSup       = False
  , bcShadowStyle   = ShadowNone
  , bcShadowColorR  = 0.8  -- Default shadow color often light gray if not specified
  , bcShadowColorG  = 0.8
  , bcShadowColorB  = 0.8
  }

-- | Create BalloonConfig from BalloonDescript and image dimensions
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
  { bcOriginX       = originX
  , bcOriginY       = originY
  , bcValidWidth    = validWidth
  , bcValidHeight   = validHeight
  , bcFontName      = fromMaybe "Sans" (fsName (bdFont bd))
  , bcFontSize      = fromMaybe 12 (fsHeight (bdFont bd))
  , bcBaseFontSize  = fromMaybe 12 (fsHeight (bdFont bd))
  , bcTextColorR    = maybe 0.2 (\v -> fromIntegral v / 255.0) (fsColorR (bdFont bd))
  , bcTextColorG    = maybe 0.2 (\v -> fromIntegral v / 255.0) (fsColorG (bdFont bd))
  , bcTextColorB    = maybe 0.2 (\v -> fromIntegral v / 255.0) (fsColorB (bdFont bd))
  , bcBgColorR      = 1.0
  , bcBgColorG      = 1.0
  , bcBgColorB      = 0.94
  , bcBgAlpha       = 0.95
  , bcLineSpacing   = 2
  , bcFontBold      = fromMaybe False (fsBold (bdFont bd))
  , bcFontItalic    = fromMaybe False (fsItalic (bdFont bd))
  , bcFontUnderline = fromMaybe False (fsUnderline (bdFont bd))
  , bcFontStrike    = fromMaybe False (fsStrike (bdFont bd))
  , bcFontSub       = False
  , bcFontSup       = False
  , bcShadowStyle   = fromMaybe ShadowNone (fsShadowStyle (bdFont bd))
  , bcShadowColorR  = maybe 0.8 (\v -> fromIntegral v / 255.0) (fsShadowColorR (bdFont bd))
  , bcShadowColorG  = maybe 0.8 (\v -> fromIntegral v / 255.0) (fsShadowColorG (bdFont bd))
  , bcShadowColorB  = maybe 0.8 (\v -> fromIntegral v / 255.0) (fsShadowColorB (bdFont bd))
  }
  where
    -- Origin point (where text starts)
    originX         = fromMaybe 10 (bdOriginX bd)

    originY         = fromMaybe 10 (bdOriginY bd)

    -- Valid rect offsets (can be negative)
    validRectLeft   = fromMaybe 0 (bdValidRectLeft bd)

    validRectTop    = fromMaybe 0 (bdValidRectTop bd)

    validRectRight  = fromMaybe 0 (bdValidRectRight bd)

    validRectBottom = fromMaybe 0 (bdValidRectBottom bd)

    -- Text area start position
    textAreaX       = originX + validRectLeft

    textAreaY       = originY + validRectTop

    -- Calculate valid width
    -- If wordwrappoint.x is specified, use it (negative value from right edge)
    -- Otherwise use validrect.right
    validWidth      = case bdWordWrapPointX bd of
      Just wwpX ->
        -- wordwrappoint.x is offset from right edge (negative)
        -- width = imgWidth + wwpX - textAreaX
        imgWidth + wwpX - textAreaX
      Nothing   ->
        -- Use validrect.right (can be negative, meaning from right edge)
        -- width = (imgWidth + validRectRight) - originX - textAreaX
        -- Simplified: imgWidth + validRectRight - originX - (originX + validRectLeft)
        --           = imgWidth + validRectRight - 2*originX - validRectLeft
        imgWidth + validRectRight - textAreaX - originX

    -- Calculate valid height
    -- validrect.bottom can be negative, meaning offset from bottom edge
    validHeight     = imgHeight + validRectBottom - textAreaY - originY

-- | State for a balloon window.
data BalloonState
  = BalloonState
  { bsWindow         :: !Gtk.Window            -- ^ The balloon window
  , bsDrawArea       :: !Gtk.DrawingArea       -- ^ Drawing area for balloon content
  , bsConfig         :: !(IORef BalloonConfig) -- ^ Balloon configuration
  , bsText           :: !(IORef [ TextSegment ]) -- ^ Current text segments with styles
  , bsScrollLine     :: !(IORef Int)           -- ^ Current scroll line (0 = top)
  , bsSurface        :: !(IORef (Maybe Pixbuf.Pixbuf))  -- ^ Current balloon surface image
  , bsCairoSurface   :: !(IORef (Maybe Cairo.Surface)) -- ^ Cached Cairo surface for drawing
  , bsVisible        :: !(IORef Bool)          -- ^ Whether balloon is visible
  , bsLayerShell     :: !(IORef Bool)          -- ^ Whether layer-shell was initialized
  , bsChoices        :: !(IORef [ BalloonChoice ]) -- ^ Current choices to display
  , bsChoiceCallback :: !(IORef (Maybe (BalloonChoice -> IO ()))) -- ^ Callback when choice is selected
  , bsChoiceRects    :: !(IORef [ ( BalloonChoice, Double, Double, Double, Double ) ]) -- ^ Choice hit boxes (choice, x, y, w, h)
  , bsBalloonDir     :: !(IORef (Maybe FilePath))  -- ^ Balloon directory path for surface loading
  , bsCharType       :: !(IORef T.Text)            -- ^ Character type: "s" (sakura), "k" (kero), "c" (communicate)
  , bsBalloonId      :: !(IORef Int)               -- ^ Current balloon ID: 0=default, 1=choice surface, etc.
  , bsPosition       :: !(IORef ( Int, Int ))        -- ^ Current balloon position (x, y)
  , bsAutoScroll     :: !(IORef Bool)              -- ^ Whether to auto-scroll when text overflows (default: True)
  , bsDescript       :: !(IORef (Maybe BalloonDescript))  -- ^ Balloon descript.txt settings
  , bsDirection      :: !(IORef Int)                      -- ^ Balloon direction: 0=left of char (balloons0.png), 1=right of char (balloons1.png)
  }

-- | Create a new balloon state with default configuration.
-- The window is created but not shown initially.
-- After creating the balloon, call 'initBalloonAlwaysOnTop' before showing it.
newBalloonState :: Gtk.Application -> IO BalloonState
newBalloonState app = newBalloonStateWithConfig app defaultBalloonConfig

-- | Create a new balloon state with custom configuration.
newBalloonStateWithConfig :: Gtk.Application -> BalloonConfig -> IO BalloonState
newBalloonStateWithConfig app config = do
  -- Initialize state refs
  configRef <- newIORef config
  textRef <- newIORef []
  scrollLineRef <- newIORef 0
  surfaceRef <- newIORef Nothing
  cairoSurfaceRef <- newIORef Nothing
  visibleRef <- newIORef False
  layerShellRef <- newIORef False
  choicesRef <- newIORef []
  choiceCallbackRef <- newIORef Nothing
  choiceRectsRef <- newIORef []
  balloonDirRef <- newIORef Nothing
  charTypeRef <- newIORef "s"  -- Default to sakura
  balloonIdRef <- newIORef 0   -- Default balloon ID: 0=default, 1=choice surface
  positionRef <- newIORef ( bcOriginX config, bcOriginY config ) -- Initial balloon position
  autoScrollRef <- newIORef True  -- Auto-scroll enabled by default
  descriptRef <- newIORef Nothing  -- Balloon descript (loaded later)
  directionRef <- newIORef 0       -- Default direction: 0=right of char

  -- Create the balloon window
  window <- new
    Gtk.Window
    [ #application := app
    , #title := "Balloon"
    , #defaultWidth := fromIntegral (bcOriginX config * 2 + bcValidWidth config)
    , #defaultHeight := fromIntegral (bcOriginY config * 2 + bcValidHeight config)
    , #resizable := False
    , #decorated := False  -- No title bar for balloon
    ]

  -- Make window transparent using CSS (fallback when no surface image)
  cssProvider <- new Gtk.CssProvider []
  Gtk.cssProviderLoadFromString cssProvider
    $ T.unlines
      [ "window.balloon {"
      , "  background-color: transparent;"
      , "}"
      , "drawingarea {"
      , "  background-color: transparent;"
      , "}"
      ]
  display <- Gdk.displayGetDefault
  case display of
    Nothing -> putStrLn "[Balloon] Warning: Could not get default display"
    Just d  -> Gtk.styleContextAddProviderForDisplay d cssProvider 800
  Gtk.widgetAddCssClass window "balloon"

  -- Create drawing area for custom balloon rendering
  drawArea <- new Gtk.DrawingArea [ #hexpand := True, #vexpand := True ]

  -- Set up the draw function
  Gtk.drawingAreaSetDrawFunc drawArea $ Just $ \_ cairoContext _w _h -> do
    cfg <- readIORef configRef
    text <- readIORef textRef
    scrollLine <- readIORef scrollLineRef
    mCairoSurface <- readIORef cairoSurfaceRef
    choices <- readIORef choicesRef
    -- Draw and get choice rects
    rects <- Cairo.renderWithContext
      (drawBalloonCairo cfg text scrollLine mCairoSurface choices)
      cairoContext
    writeIORef choiceRectsRef rects

  -- Add scroll controller for mouse wheel scrolling
  scrollController
    <- new Gtk.EventControllerScroll [ #flags := [ Gtk.EventControllerScrollFlagsVertical ] ]
  void $ GI.on scrollController #scroll $ \_dx dy -> do
    if dy > 0
      then modifyIORef' scrollLineRef (+ 1)  -- Scroll down
      else modifyIORef' scrollLineRef (\n -> max 0 (n - 1))  -- Scroll up
    Gtk.widgetQueueDraw drawArea
    return True
  Gtk.widgetAddController drawArea scrollController

  -- Add click gesture for choice selection
  clickGesture <- new Gtk.GestureClick []
  void $ GI.on clickGesture #released $ \_nPress x y -> do
    choiceRects <- readIORef choiceRectsRef
    mCallback <- readIORef choiceCallbackRef
    -- Check if click is within any choice rect
    case mCallback of
      Nothing       -> return ()
      Just callback -> do
        let clickedChoice = findClickedChoice x y choiceRects
        case clickedChoice of
          Nothing     -> return ()
          Just choice -> do
            putStrLn $ "[Balloon] Choice clicked: " <> T.unpack (bcText choice)
            callback choice
  Gtk.widgetAddController drawArea clickGesture

  -- Note: Drag gesture for window movement is set up separately via FRP network.
  -- The setupBalloonNetwork function in Event.hs handles drag-based movement
  -- with delta-based tracking to avoid jitter on layer-shell windows.

  Gtk.windowSetChild window (Just drawArea)

  -- Handle close request - just hide, don't destroy
  void $ GI.on window #closeRequest $ do
    Gtk.widgetSetVisible window False
    writeIORef visibleRef False
    return True  -- Prevent destruction

  return
    BalloonState
    { bsWindow         = window
    , bsDrawArea       = drawArea
    , bsConfig         = configRef
    , bsText           = textRef
    , bsScrollLine     = scrollLineRef
    , bsSurface        = surfaceRef
    , bsCairoSurface   = cairoSurfaceRef
    , bsVisible        = visibleRef
    , bsLayerShell     = layerShellRef
    , bsChoices        = choicesRef
    , bsChoiceCallback = choiceCallbackRef
    , bsChoiceRects    = choiceRectsRef
    , bsBalloonDir     = balloonDirRef
    , bsCharType       = charTypeRef
    , bsBalloonId      = balloonIdRef
    , bsPosition       = positionRef
    , bsAutoScroll     = autoScrollRef
    , bsDescript       = descriptRef
    , bsDirection      = directionRef
    }

-- | Create a new balloon state with a surface loaded from a balloon directory.
-- This is the preferred method for creating balloons for characters, as it
-- automatically loads the correct balloon surface based on character type.
--
-- Character types:
-- - Scope 0 (sakura): "s" -> loads balloons0.png
-- - Scope 1+ (kero, etc.): "k" -> loads balloonk0.png
newBalloonStateWithSurface :: Gtk.Application
                           -> FilePath         -- ^ Balloon directory path
                           -> T.Text           -- ^ Character type: "s" for sakura, "k" for kero
                           -> IO BalloonState
newBalloonStateWithSurface app balloonDir charType = do
  -- Try to load descript.txt from balloon directory
  let descriptPath = balloonDir </> "descript.txt"
  descriptExists <- doesFileExist descriptPath
  mDescript <- if descriptExists
    then do
      descript <- readBalloonDescript descriptPath
      putStrLn $ "[Balloon] Loaded descript.txt from: " <> descriptPath
      putStrLn
        $ "[Balloon]   origin: ("
        <> show (bdOriginX descript)
        <> ", "
        <> show (bdOriginY descript)
        <> ")"
      putStrLn
        $ "[Balloon]   validrect: left="
        <> show (bdValidRectLeft descript)
        <> " top="
        <> show (bdValidRectTop descript)
        <> " right="
        <> show (bdValidRectRight descript)
        <> " bottom="
        <> show (bdValidRectBottom descript)
      putStrLn $ "[Balloon]   wordwrappoint.x: " <> show (bdWordWrapPointX descript)
      return (Just descript)
    else do
      putStrLn $ "[Balloon] No descript.txt found at: " <> descriptPath <> ", using defaults"
      return Nothing

  -- Create balloon with default config first
  bs <- newBalloonStateWithConfig app defaultBalloonConfig

  -- Store balloon directory, char type, and descript for future surface switches
  writeIORef (bsBalloonDir bs) (Just balloonDir)
  writeIORef (bsCharType bs) charType
  writeIORef (bsDescript bs) mDescript

  -- Load the initial surface (index 0) - this will also update config based on image size
  _ <- loadAndSetBalloonSurface bs balloonDir charType 0
  return bs

-- | Cairo drawing implementation.
-- Returns the list of choice rectangles for click detection.
drawBalloonCairo :: BalloonConfig
                 -> [ TextSegment ]
                 -> Int
                 -> Maybe Cairo.Surface
                 -> [ BalloonChoice ]
                 -> Cairo.Render [ ( BalloonChoice, Double, Double, Double, Double ) ]
drawBalloonCairo config segments scrollLine mSurface choices = do
  -- Draw background
  case mSurface of
    Just surface -> do
      -- Draw the balloon surface image as background
      Cairo.setSourceSurface surface 0 0
      Cairo.paint
    Nothing      -> do
      -- Draw solid background with rounded corners (fallback)
      drawSolidBackground config

  -- Draw text using PangoCairo (with clipping and scrolling)
  -- Returns the Y position after text for choice drawing
  textEndY <- drawText config segments scrollLine

  -- Draw choices below the text
  drawChoices config choices textEndY scrollLine

-- | Draw solid background with rounded corners.
drawSolidBackground :: BalloonConfig -> Cairo.Render ()
drawSolidBackground config = do
  let r   = 10  -- Corner radius
      w   = fromIntegral (bcOriginX config * 2 + bcValidWidth config)
      h   = fromIntegral (bcOriginY config * 2 + bcValidHeight config)
      pi' = pi :: Double

  -- Draw rounded rectangle path
  Cairo.newPath
  Cairo.arc (w - r) r r (-(pi' / 2)) 0
  Cairo.arc (w - r) (h - r) r 0 (pi' / 2)
  Cairo.arc r (h - r) r (pi' / 2) pi'
  Cairo.arc r r r pi' (3 * pi' / 2)
  Cairo.closePath

  -- Fill with background color
  Cairo.setSourceRGBA
    (bcBgColorR config)
    (bcBgColorG config)
    (bcBgColorB config)
    (bcBgAlpha config)
  Cairo.fill

  -- Draw border
  Cairo.newPath
  Cairo.arc (w - r) r r (-(pi' / 2)) 0
  Cairo.arc (w - r) (h - r) r 0 (pi' / 2)
  Cairo.arc r (h - r) r (pi' / 2) pi'
  Cairo.arc r r r pi' (3 * pi' / 2)
  Cairo.closePath
  Cairo.setSourceRGBA 0.5 0.5 0.5 1.0
  Cairo.setLineWidth 1
  Cairo.stroke

-- | Draw text using PangoCairo with clipping and scroll support.
-- Returns the Y position after the text (for choice rendering).
-- Now supports styled text segments - each segment can have different styles.
drawText :: BalloonConfig -> [ TextSegment ] -> Int -> Cairo.Render Double
drawText config segments scrollLine = do
  -- Get the underlying Cairo context for PangoCairo functions
  ctx <- Cairo.getContext

  -- Create Pango layout using PangoCairo
  layout <- Cairo.liftIO $ PangoCairo.createLayout ctx

  -- Concatenate all text segments
  let fullText = T.concat (map tsText segments)

  -- Set text
  Cairo.liftIO $ Pango.layoutSetText layout fullText (-1)

  -- Set base font (will be overridden by attributes per segment)
  fontDesc <- Cairo.liftIO Pango.fontDescriptionNew
  Cairo.liftIO $ Pango.fontDescriptionSetFamily fontDesc (bcFontName config)
  Cairo.liftIO
    $ Pango.fontDescriptionSetSize
      fontDesc
      (fromIntegral $ bcFontSize config * fromIntegral Pango.SCALE)
  Cairo.liftIO $ Pango.layoutSetFontDescription layout (Just fontDesc)

  -- Build attribute list from segments
  attrs <- Cairo.liftIO $ buildAttributeList segments
  Cairo.liftIO $ Pango.layoutSetAttributes layout (Just attrs)

  -- Set wrapping - use CHAR mode for Japanese text support
  Cairo.liftIO
    $ Pango.layoutSetWidth layout (fromIntegral $ bcValidWidth config * fromIntegral Pango.SCALE)
  Cairo.liftIO $ Pango.layoutSetWrap layout Pango.WrapModeChar

  -- Set line spacing
  Cairo.liftIO
    $ Pango.layoutSetSpacing
      layout
      (fromIntegral $ bcLineSpacing config * fromIntegral Pango.SCALE)

  -- Calculate line height for scrolling
  lineHeight <- Cairo.liftIO $ do
    pangoCtx <- Pango.layoutGetContext layout
    metrics <- Pango.contextGetMetrics pangoCtx (Just fontDesc) Nothing
    ascent <- Pango.fontMetricsGetAscent metrics
    descent <- Pango.fontMetricsGetDescent metrics
    return
      $ (fromIntegral ascent
         + fromIntegral descent
         + fromIntegral (bcLineSpacing config * fromIntegral Pango.SCALE))
      / fromIntegral Pango.SCALE

  -- Get text layout height
  ( _, textHeight ) <- Cairo.liftIO $ Pango.layoutGetPixelSize layout

  -- Set up clipping rectangle for text area (validrect)
  Cairo.save
  Cairo.rectangle
    (fromIntegral $ bcOriginX config)
    (fromIntegral $ bcOriginY config)
    (fromIntegral $ bcValidWidth config)
    (fromIntegral $ bcValidHeight config)
  Cairo.clip

  -- Move to text origin with scroll offset
  let scrollOffset = fromIntegral scrollLine * lineHeight

  -- Draw Shadow (if enabled)
  case bcShadowStyle config of
    ShadowNone    -> return ()
    ShadowOffset  -> do
      Cairo.save
      Cairo.setSourceRGB (bcShadowColorR config) (bcShadowColorG config) (bcShadowColorB config)
      Cairo.moveTo
        (fromIntegral (bcOriginX config) + 1)
        (fromIntegral (bcOriginY config) - scrollOffset + 1)
      Cairo.liftIO $ PangoCairo.showLayout ctx layout
      Cairo.restore
    ShadowOutline -> do
      Cairo.save
      Cairo.setSourceRGB (bcShadowColorR config) (bcShadowColorG config) (bcShadowColorB config)
      Cairo.moveTo
        (fromIntegral (bcOriginX config))
        (fromIntegral (bcOriginY config) - scrollOffset)
      Cairo.liftIO $ PangoCairo.layoutPath ctx layout
      Cairo.setLineWidth 2.0
      Cairo.stroke
      Cairo.restore

  -- Draw main text
  Cairo.moveTo (fromIntegral $ bcOriginX config) (fromIntegral (bcOriginY config) - scrollOffset)

  -- Set default text color (segments with custom colors override via attributes)
  Cairo.setSourceRGB (bcTextColorR config) (bcTextColorG config) (bcTextColorB config)

  -- Draw the text
  Cairo.liftIO $ PangoCairo.showLayout ctx layout

  -- Restore Cairo state (removes clipping)
  Cairo.restore

  -- Return the Y position after the text (accounting for scroll)
  return
    $ fromIntegral (bcOriginY config) + fromIntegral textHeight - scrollOffset
    + fromIntegral (bcLineSpacing config)

-- | Build Pango attribute list from text segments.
-- Each segment can have different font styles (bold, italic, etc.)
-- IMPORTANT: Pango uses BYTE indices, not character indices.
-- For UTF-8 text (like Japanese), we must use byte length.
buildAttributeList :: [ TextSegment ] -> IO Pango.AttrList
buildAttributeList segments = do
  attrs <- Pango.attrListNew
  let go _ [] = return ()
      go startIdx (seg : rest) = do
        -- Use byte length for UTF-8 text (Japanese characters are 3 bytes each)
        let segByteLen = fromIntegral $ BS.length (TE.encodeUtf8 (tsText seg))
            endIdx     = startIdx + segByteLen

        -- Font family attribute
        fontAttr <- Pango.attrFamilyNew (tsFontName seg)
        Pango.setAttributeStartIndex fontAttr startIdx
        Pango.setAttributeEndIndex fontAttr endIdx
        Pango.attrListInsert attrs fontAttr

        -- Font size attribute
        sizeAttr <- Pango.attrSizeNew (fromIntegral $ tsFontSize seg * fromIntegral Pango.SCALE)
        Pango.setAttributeStartIndex sizeAttr startIdx
        Pango.setAttributeEndIndex sizeAttr endIdx
        Pango.attrListInsert attrs sizeAttr

        -- Bold attribute
        when (tsBold seg) $ do
          weightAttr <- Pango.attrWeightNew Pango.WeightBold
          Pango.setAttributeStartIndex weightAttr startIdx
          Pango.setAttributeEndIndex weightAttr endIdx
          Pango.attrListInsert attrs weightAttr

        -- Italic attribute
        when (tsItalic seg) $ do
          styleAttr <- Pango.attrStyleNew Pango.StyleItalic
          Pango.setAttributeStartIndex styleAttr startIdx
          Pango.setAttributeEndIndex styleAttr endIdx
          Pango.attrListInsert attrs styleAttr

        -- Underline attribute
        when (tsUnderline seg) $ do
          ulAttr <- Pango.attrUnderlineNew Pango.UnderlineSingle
          Pango.setAttributeStartIndex ulAttr startIdx
          Pango.setAttributeEndIndex ulAttr endIdx
          Pango.attrListInsert attrs ulAttr

        -- Strikethrough attribute
        when (tsStrike seg) $ do
          stAttr <- Pango.attrStrikethroughNew True
          Pango.setAttributeStartIndex stAttr startIdx
          Pango.setAttributeEndIndex stAttr endIdx
          Pango.attrListInsert attrs stAttr

        -- Subscript (negative rise)
        when (tsSub seg) $ do
          riseAttr <- Pango.attrRiseNew
            (fromIntegral $ -(tsFontSize seg * fromIntegral Pango.SCALE `div` 3))
          Pango.setAttributeStartIndex riseAttr startIdx
          Pango.setAttributeEndIndex riseAttr endIdx
          Pango.attrListInsert attrs riseAttr

        -- Superscript (positive rise)
        when (tsSup seg) $ do
          riseAttr <- Pango.attrRiseNew
            (fromIntegral $ tsFontSize seg * fromIntegral Pango.SCALE `div` 2)
          Pango.setAttributeStartIndex riseAttr startIdx
          Pango.setAttributeEndIndex riseAttr endIdx
          Pango.attrListInsert attrs riseAttr

        -- Color attribute (convert 0.0-1.0 to 0-65535)
        let r16 = round (tsColorR seg * 65535)
            g16 = round (tsColorG seg * 65535)
            b16 = round (tsColorB seg * 65535)
        colorAttr <- Pango.attrForegroundNew r16 g16 b16
        Pango.setAttributeStartIndex colorAttr startIdx
        Pango.setAttributeEndIndex colorAttr endIdx
        Pango.attrListInsert attrs colorAttr

        go endIdx rest
  go 0 segments
  return attrs

-- | Draw choices below the text.
-- Returns list of choice rectangles for hit testing.
drawChoices :: BalloonConfig
            -> [ BalloonChoice ]
            -> Double          -- ^ Y position to start drawing choices
            -> Int             -- ^ Scroll line offset (unused for now)
            -> Cairo.Render [ ( BalloonChoice, Double, Double, Double, Double ) ]
drawChoices config choices startY _scrollLine = do
  if null choices
    then return []
    else do
      ctx <- Cairo.getContext

      -- Create font description for choices
      fontDesc <- Cairo.liftIO Pango.fontDescriptionNew
      Cairo.liftIO $ Pango.fontDescriptionSetFamily fontDesc (bcFontName config)
      Cairo.liftIO
        $ Pango.fontDescriptionSetSize
          fontDesc
          (fromIntegral $ bcFontSize config * fromIntegral Pango.SCALE)

      -- Get line height from a sample layout
      sampleLayout <- Cairo.liftIO $ PangoCairo.createLayout ctx
      Cairo.liftIO $ Pango.layoutSetFontDescription sampleLayout (Just fontDesc)
      Cairo.liftIO $ Pango.layoutSetText sampleLayout "Test" (-1)
      ( _, sampleHeight ) <- Cairo.liftIO $ Pango.layoutGetPixelSize sampleLayout
      let lineHeight = fromIntegral sampleHeight + fromIntegral (bcLineSpacing config)

      -- Set up clipping rectangle for text area
      Cairo.save
      Cairo.rectangle
        (fromIntegral $ bcOriginX config)
        (fromIntegral $ bcOriginY config)
        (fromIntegral $ bcValidWidth config)
        (fromIntegral $ bcValidHeight config)
      Cairo.clip

      -- Draw each choice and collect rectangles
      rects <- drawChoiceLoop fontDesc config choices startY lineHeight []

      Cairo.restore
      return rects
  where
    drawChoiceLoop :: Pango.FontDescription
                   -> BalloonConfig
                   -> [ BalloonChoice ]
                   -> Double
                   -> Double
                   -> [ ( BalloonChoice, Double, Double, Double, Double ) ]
                   -> Cairo.Render [ ( BalloonChoice, Double, Double, Double, Double ) ]
    drawChoiceLoop _ _ [] _ _ acc = return $ reverse acc
    drawChoiceLoop fontDesc cfg (choice : rest) y lh acc = do
      -- Get the Cairo context for drawing
      ctx <- Cairo.getContext

      -- Create layout for this choice
      layout <- Cairo.liftIO $ PangoCairo.createLayout ctx
      let choiceText = "▶ " <> bcText choice  -- Add bullet prefix
      Cairo.liftIO $ Pango.layoutSetText layout choiceText (-1)
      Cairo.liftIO $ Pango.layoutSetFontDescription layout (Just fontDesc)
      Cairo.liftIO
        $ Pango.layoutSetWidth layout (fromIntegral $ bcValidWidth cfg * fromIntegral Pango.SCALE)

      -- Get choice dimensions
      ( textWidth, textHeight ) <- Cairo.liftIO $ Pango.layoutGetPixelSize layout

      -- Set choice color (blue for links)
      Cairo.setSourceRGB 0.0 0.4 0.8
      Cairo.moveTo (fromIntegral $ bcOriginX cfg) y
      Cairo.liftIO $ PangoCairo.showLayout ctx layout

      -- Add rect to accumulator
      let rect
            = ( choice
              , fromIntegral $ bcOriginX cfg
              , y
              , fromIntegral textWidth
              , fromIntegral textHeight
              )

      -- Continue with next choice
      drawChoiceLoop fontDesc cfg rest (y + lh) lh (rect : acc)

-- | Initialize platform-specific features for the balloon window to make it always-on-top.
-- This must be called BEFORE the balloon is shown for the first time.
-- Returns True if initialization was successful.
initBalloonAlwaysOnTop :: BalloonState -> IO Bool
initBalloonAlwaysOnTop bs = do
  let window = bsWindow bs
  -- Initialize platform (layer-shell on Wayland, nothing on X11)
  platformSuccess <- initPlatformWindow window
  if platformSuccess
    then do
      -- On Wayland, set layer to top
      setWindowLayer window LayerTop
      writeIORef (bsLayerShell bs) True
      putStrLn "[Balloon] Platform window initialized (Wayland layer-shell)"
      return True
    else do
      writeIORef (bsLayerShell bs) False
      return False

-- | Show the balloon window.
-- If platform init was not done, this will attempt to use X11 always-on-top.
-- This function is safe to call from any thread.
showBalloon :: BalloonState -> IO ()
showBalloon bs = do
  visible <- readIORef (bsVisible bs)
  unless visible $ do
    -- Schedule GTK operations on main thread
    _ <- GLib.idleAdd GLib.PRIORITY_HIGH $ do
      Gtk.windowPresent (bsWindow bs)
      writeIORef (bsVisible bs) True
      -- Apply always-on-top if not using layer-shell
      -- This needs to be done after the window is realized/shown
      isLayerShell <- readIORef (bsLayerShell bs)
      unless isLayerShell $ do
        x11Success <- setWindowAlwaysOnTop (bsWindow bs) True
        when x11Success $ putStrLn "[Balloon] Window set to always-on-top (X11)"
      return False  -- Don't repeat
    return ()

-- | Hide the balloon window.
-- This function is safe to call from any thread.
hideBalloon :: BalloonState -> IO ()
hideBalloon bs = void $ GLib.idleAdd GLib.PRIORITY_HIGH $ do
  Gtk.widgetSetVisible (bsWindow bs) False
  writeIORef (bsVisible bs) False
  return False

-- | Clear all text and choices from the balloon.
clearBalloon :: BalloonState -> IO ()
clearBalloon bs = do
  writeIORef (bsText bs) []
  writeIORef (bsScrollLine bs) 0
  writeIORef (bsChoices bs) []
  writeIORef (bsChoiceRects bs) []
  Gtk.widgetQueueDraw (bsDrawArea bs)

-- | Append text to the balloon.
-- This is the main function called when processing SakuraScript text.
-- Captures the current font style and stores it with the text segment.
-- If auto-scroll is enabled, automatically scrolls to show the last line.
appendText :: BalloonState -> T.Text -> IO ()
appendText bs txt = do
  cfg <- readIORef (bsConfig bs)
  let segment
        = TextSegment
        { tsText      = txt
        , tsBold      = bcFontBold cfg
        , tsItalic    = bcFontItalic cfg
        , tsUnderline = bcFontUnderline cfg
        , tsStrike    = bcFontStrike cfg
        , tsSub       = bcFontSub cfg
        , tsSup       = bcFontSup cfg
        , tsFontSize  = bcFontSize cfg
        , tsFontName  = bcFontName cfg
        , tsColorR    = bcTextColorR cfg
        , tsColorG    = bcTextColorG cfg
        , tsColorB    = bcTextColorB cfg
        }
  modifyIORef' (bsText bs) (<> [ segment ])
  -- Perform auto-scroll if enabled
  autoScroll <- readIORef (bsAutoScroll bs)
  when autoScroll $ autoScrollToLastLine bs
  Gtk.widgetQueueDraw (bsDrawArea bs)
  -- Auto-show when text is added
  showBalloon bs

-- | Append a single character to the balloon.
-- Used for character-by-character display animation.
appendChar :: BalloonState -> Char -> IO ()
appendChar bs c = appendText bs (T.singleton c)

-- | Append a newline to the balloon.
appendNewline :: BalloonState -> IO ()
appendNewline bs = appendText bs "\n"

-- | Append a half-height newline to the balloon.
-- Uses a special Unicode line separator with reduced line spacing.
appendNewlineHalf :: BalloonState -> IO ()
appendNewlineHalf bs = do
  -- Insert a special marker that drawText will recognize for half-height newline
  -- We use Unicode LINE SEPARATOR (U+2028) which Pango treats as a soft break
  -- Combined with temporary line spacing reduction
  cfg <- readIORef (bsConfig bs)
  let halfSpacing = max 1 (bcLineSpacing cfg `div` 2)
  -- Temporarily reduce line spacing, add newline, restore
  modifyIORef' (bsConfig bs) $ \c -> c { bcLineSpacing = halfSpacing }
  appendText bs "\n"
  modifyIORef' (bsConfig bs) $ \c -> c { bcLineSpacing = bcLineSpacing cfg }

-- | Append a percentage-height newline to the balloon.
-- The percentage affects the line height (100 = normal, 50 = half, etc.)
appendNewlinePercent :: BalloonState -> Int -> IO ()
appendNewlinePercent bs pct = do
  cfg <- readIORef (bsConfig bs)
  let adjustedSpacing = max 1 ((bcLineSpacing cfg * pct) `div` 100)
  -- Temporarily adjust line spacing based on percentage
  modifyIORef' (bsConfig bs) $ \c -> c { bcLineSpacing = adjustedSpacing }
  appendText bs "\n"
  modifyIORef' (bsConfig bs) $ \c -> c { bcLineSpacing = bcLineSpacing cfg }

-- | Clear n characters from the end of the balloon text.
clearChars :: BalloonState -> Int -> IO ()
clearChars bs n = do
  modifyIORef' (bsText bs) $ \segments -> let
      totalText = T.concat (map tsText segments)
      newLen    = max 0 (T.length totalText - n)
      newText   = T.take newLen totalText
    in
      rebuildSegments segments newText
  Gtk.widgetQueueDraw (bsDrawArea bs)
  where
    -- Rebuild segments keeping styles but truncating to new total length
    rebuildSegments :: [ TextSegment ] -> T.Text -> [ TextSegment ]
    rebuildSegments [] _ = []
    rebuildSegments _ remaining
      | T.null remaining = []
    rebuildSegments (seg : rest) remaining
      = let
          segLen       = T.length (tsText seg)
          remainingLen = T.length remaining
        in
          if remainingLen <= segLen
            then [ seg { tsText = remaining } ]
            else seg : rebuildSegments rest (T.drop segLen remaining)

-- | Move cursor to specified position.
-- This is used for \_l[x,y] command to position subsequent text.
-- Implemented by padding with spaces/newlines to reach the target position.
moveCursor :: BalloonState -> Int -> Int -> IO ()
moveCursor bs targetX targetY = do
  cfg <- readIORef (bsConfig bs)
  segments <- readIORef (bsText bs)
  let fullText     = T.concat (map tsText segments)
      lineHeight   = bcFontSize cfg
      charWidth    = lineHeight `div` 2  -- Approximate monospace character width
      currentLines = T.lines fullText
      currentY     = length currentLines * lineHeight
      currentX
        = if null currentLines
          then 0
          else maybe 0 T.length (viaNonEmpty last currentLines) * charWidth

  -- Add newlines to reach target Y if needed
  let neededNewlines = max 0 ((targetY - currentY) `div` lineHeight)
      newlinesText   = T.replicate neededNewlines "\n"

  -- Add spaces to reach target X
  let neededSpaces
        = max
          0
          ((targetX
            - if neededNewlines > 0
              then 0
              else currentX)
           `div` charWidth)
      spacesText   = T.replicate neededSpaces " "
      combinedText = newlinesText <> spacesText

  -- Add as segment with current style
  cfg' <- readIORef (bsConfig bs)
  unless (T.null combinedText) $ do
    let segment
          = TextSegment
          { tsText      = combinedText
          , tsBold      = bcFontBold cfg'
          , tsItalic    = bcFontItalic cfg'
          , tsUnderline = bcFontUnderline cfg'
          , tsStrike    = bcFontStrike cfg'
          , tsSub       = bcFontSub cfg'
          , tsSup       = bcFontSup cfg'
          , tsFontSize  = bcFontSize cfg'
          , tsFontName  = bcFontName cfg'
          , tsColorR    = bcTextColorR cfg'
          , tsColorG    = bcTextColorG cfg'
          , tsColorB    = bcTextColorB cfg'
          }
    modifyIORef' (bsText bs) (<> [ segment ])
  Gtk.widgetQueueDraw (bsDrawArea bs)

-- | Set font name.
setFontName :: BalloonState -> T.Text -> IO ()
setFontName bs name = do
  modifyIORef' (bsConfig bs) $ \cfg -> cfg { bcFontName = name }
  Gtk.widgetQueueDraw (bsDrawArea bs)

-- | Set font size.
setFontSize :: BalloonState -> Int -> IO ()
setFontSize bs size = do
  modifyIORef' (bsConfig bs) $ \cfg -> cfg { bcFontSize = size }
  Gtk.widgetQueueDraw (bsDrawArea bs)

-- | Get current font size.
getFontSize :: BalloonState -> IO Int
getFontSize bs = bcFontSize <$> readIORef (bsConfig bs)

-- | Set font color (RGB, 0.0-1.0).
setFontColor :: BalloonState -> Double -> Double -> Double -> IO ()
setFontColor bs r g b = do
  modifyIORef' (bsConfig bs) $ \cfg -> cfg { bcTextColorR = r, bcTextColorG = g, bcTextColorB = b }
  Gtk.widgetQueueDraw (bsDrawArea bs)

-- | Set font bold.
setFontBold :: BalloonState -> Bool -> IO ()
setFontBold bs bold = do
  modifyIORef' (bsConfig bs) $ \cfg -> cfg { bcFontBold = bold }
  Gtk.widgetQueueDraw (bsDrawArea bs)

-- | Set font italic.
setFontItalic :: BalloonState -> Bool -> IO ()
setFontItalic bs italic = do
  modifyIORef' (bsConfig bs) $ \cfg -> cfg { bcFontItalic = italic }
  Gtk.widgetQueueDraw (bsDrawArea bs)

-- | Set font underline.
setFontUnderline :: BalloonState -> Bool -> IO ()
setFontUnderline bs underline = do
  modifyIORef' (bsConfig bs) $ \cfg -> cfg { bcFontUnderline = underline }
  Gtk.widgetQueueDraw (bsDrawArea bs)

-- | Set font strikethrough.
setFontStrike :: BalloonState -> Bool -> IO ()
setFontStrike bs strike = do
  modifyIORef' (bsConfig bs) $ \cfg -> cfg { bcFontStrike = strike }
  Gtk.widgetQueueDraw (bsDrawArea bs)

-- | Set font subscript mode.
-- When enabled, text is rendered smaller and lower using Pango attributes.
setFontSub :: BalloonState -> Bool -> IO ()
setFontSub bs enabled = do
  modifyIORef' (bsConfig bs) $ \cfg -> if enabled
    then cfg { bcFontSub  = True
             , bcFontSup  = False  -- Can't be both
             , bcFontSize = (bcBaseFontSize cfg * 3) `div` 4  -- 75% size
             }
    else cfg { bcFontSub  = False
             , bcFontSize = bcBaseFontSize cfg  -- Restore base size
             }
  Gtk.widgetQueueDraw (bsDrawArea bs)

-- | Set font superscript mode.
-- When enabled, text is rendered smaller and higher using Pango attributes.
setFontSup :: BalloonState -> Bool -> IO ()
setFontSup bs enabled = do
  modifyIORef' (bsConfig bs) $ \cfg -> if enabled
    then cfg { bcFontSup  = True
             , bcFontSub  = False  -- Can't be both
             , bcFontSize = (bcBaseFontSize cfg * 3) `div` 4  -- 75% size
             }
    else cfg { bcFontSup  = False
             , bcFontSize = bcBaseFontSize cfg  -- Restore base size
             }
  Gtk.widgetQueueDraw (bsDrawArea bs)

-- | Reset font to default settings.
resetFont :: BalloonState -> IO ()
resetFont bs = do
  modifyIORef' (bsConfig bs) $ \cfg -> cfg
    { bcFontName      = bcFontName defaultBalloonConfig
    , bcFontSize      = bcBaseFontSize cfg  -- Use saved base size, not default
    , bcTextColorR    = bcTextColorR defaultBalloonConfig
    , bcTextColorG    = bcTextColorG defaultBalloonConfig
    , bcTextColorB    = bcTextColorB defaultBalloonConfig
    , bcFontBold      = False
    , bcFontItalic    = False
    , bcFontUnderline = False
    , bcFontStrike    = False
    , bcFontSub       = False
    , bcFontSup       = False
    }
  Gtk.widgetQueueDraw (bsDrawArea bs)

-- | Set the balloon surface image.
-- This converts the Pixbuf to a Cairo Surface for efficient drawing.
setBalloonSurface :: BalloonState -> Maybe Pixbuf.Pixbuf -> IO ()
setBalloonSurface bs mPixbuf = do
  writeIORef (bsSurface bs) mPixbuf
  -- Convert Pixbuf to Cairo Surface for drawing
  mCairoSurface <- case mPixbuf of
    Nothing     -> return Nothing
    Just pixbuf -> pixbufToCairoSurface pixbuf
  writeIORef (bsCairoSurface bs) mCairoSurface
  -- Resize window to match surface if available
  case mPixbuf of
    Just pixbuf -> do
      w <- Pixbuf.pixbufGetWidth pixbuf
      h <- Pixbuf.pixbufGetHeight pixbuf
      Gtk.windowSetDefaultSize (bsWindow bs) w h
    Nothing     -> return ()
  Gtk.widgetQueueDraw (bsDrawArea bs)

-- | Get the balloon directory path.
-- Returns Nothing if no balloon directory was set.
getBalloonDir :: BalloonState -> IO (Maybe FilePath)
getBalloonDir bs = readIORef (bsBalloonDir bs)

-- | Load a balloon surface from a balloon directory and set it.
-- Uses the balloon surface naming convention:
-- - "s" for sakura (balloons0.png, balloons1.png, ...)
-- - "k" for kero (balloonk0.png, balloonk1.png, ...)
-- - "c" for communicate box (balloonc0.png, ...)
--
-- Example: loadAndSetBalloonSurface bs "/path/to/balloon" "s" 0
-- loads and sets "balloons0.png"
loadAndSetBalloonSurface
  :: BalloonState
  -> FilePath     -- ^ Balloon directory path
  -> T.Text       -- ^ Character type: "s", "k", or "c"
  -> Int          -- ^ Surface index
  -> IO Bool      -- ^ Returns True if surface was loaded
loadAndSetBalloonSurface bs balloonDir charType index = do
  mPixbuf <- loadBalloonSurface balloonDir charType index
  setBalloonSurface bs mPixbuf
  case mPixbuf of
    Just pixbuf -> do
      putStrLn $ "[Balloon] Loaded surface: balloon" <> T.unpack charType <> show index
      -- Get image dimensions and update config based on descript
      imgWidth <- fromIntegral <$> Pixbuf.pixbufGetWidth pixbuf
      imgHeight <- fromIntegral <$> Pixbuf.pixbufGetHeight pixbuf
      mDescript <- readIORef (bsDescript bs)
      case mDescript of
        Just descript -> do
          let newConfig = configFromDescript descript imgWidth imgHeight
          writeIORef (bsConfig bs) newConfig
          putStrLn $ "[Balloon]   image size: " <> show imgWidth <> "x" <> show imgHeight
          putStrLn
            $ "[Balloon]   text area: origin=("
            <> show (bcOriginX newConfig)
            <> ","
            <> show (bcOriginY newConfig)
            <> ") size="
            <> show (bcValidWidth newConfig)
            <> "x"
            <> show (bcValidHeight newConfig)
        Nothing       ->
          -- No descript, keep default config but still log image size
          putStrLn
            $ "[Balloon]   image size: "
            <> show imgWidth
            <> "x"
            <> show imgHeight
            <> " (using default config)"
      return True
    Nothing     -> do
      putStrLn $ "[Balloon] Failed to load: balloon" <> T.unpack charType <> show index
      return False

-- | Set the balloon ID and load the corresponding surface.
-- Balloon ID determines which surface set to use:
--   -1 = hide balloon (per UKADOC: \b[-1] hides the balloon)
--   0 = default balloon (surface 0,1)
--   1 = choice/selection balloon (surface 2,3)
--   etc.
-- Surface index is calculated as: balloonId * 2 + direction
-- where direction is 0 (left) or 1 (right)
setBalloonId :: BalloonState -> Int -> IO ()
setBalloonId bs newBalloonId = do
  currentId <- readIORef (bsBalloonId bs)
  when (currentId /= newBalloonId) $ do
    writeIORef (bsBalloonId bs) newBalloonId
    -- Special case: -1 means hide the balloon
    if newBalloonId < 0
      then do
        hideBalloon bs
        putStrLn "[Balloon] Balloon hidden (ID < 0)"
      else do
        -- Reload surface with new balloon ID
        mBalloonDir <- readIORef (bsBalloonDir bs)
        case mBalloonDir of
          Nothing
            -> putStrLn $ "[Balloon] Cannot set balloon ID " <> show newBalloonId <> ": no balloon dir"
          Just balloonDir -> do
            charType <- readIORef (bsCharType bs)
            direction <- readIORef (bsDirection bs)
            let surfaceIndex = newBalloonId * 2 + direction
            _ <- loadAndSetBalloonSurface bs balloonDir charType surfaceIndex
            putStrLn
              $ "[Balloon] Set balloon ID to "
              <> show newBalloonId
              <> " (surface index "
              <> show surfaceIndex
              <> ")"

-- | Convert a GdkPixbuf to a Cairo ImageSurface.
-- GdkPixbuf stores data as RGB(A) with 8 bits per channel.
-- Cairo ARGB32 format expects pre-multiplied ARGB in native byte order.
pixbufToCairoSurface :: Pixbuf.Pixbuf -> IO (Maybe Cairo.Surface)
pixbufToCairoSurface pixbuf = do
  w <- Pixbuf.pixbufGetWidth pixbuf
  h <- Pixbuf.pixbufGetHeight pixbuf
  _hasAlpha <- Pixbuf.pixbufGetHasAlpha pixbuf
  rowstride <- Pixbuf.pixbufGetRowstride pixbuf
  nChannels <- Pixbuf.pixbufGetNChannels pixbuf
  pixels <- Pixbuf.pixbufGetPixels pixbuf

  let cairoStride = Cairo.formatStrideForWidth Cairo.FormatARGB32 (fromIntegral w)
      srcBytes = BS.unpack pixels

      -- Convert RGBA to ARGB32 (pre-multiplied)
      -- GdkPixbuf: R G B [A] at consecutive bytes
      -- Cairo ARGB32: B G R A on little-endian (stored as 32-bit word)
      convertPixels :: [ Word8 ] -> Int -> Int -> [ Word8 ]
      convertPixels srcData y srcRowStart
        | y >= fromIntegral h = []
        | otherwise
          = let
              rowData  = take (fromIntegral rowstride) (drop srcRowStart srcData)
              cairoRow = convertRow rowData 0
              -- Pad row to Cairo stride
              padding  = replicate (cairoStride - fromIntegral w * 4) 0
            in
              cairoRow
              ++ padding
              ++ convertPixels srcData (y + 1) (srcRowStart + fromIntegral rowstride)

      convertRow :: [ Word8 ] -> Int -> [ Word8 ]
      convertRow rowData x
        | x >= fromIntegral w = []
        | otherwise
          = let
              pixel = take (fromIntegral nChannels) rowData
              restRow = drop (fromIntegral nChannels) rowData
              ( r, g, b, a ) = case pixel of
                [ r', g', b', a'' ] -> ( r', g', b', a'' )
                [ r', g', b' ] -> ( r', g', b', 255 )  -- No alpha = fully opaque
                _ -> ( 0, 0, 0, 0 )       -- Invalid
              -- Pre-multiply alpha for Cairo
              a' = fromIntegral a / 255.0 :: Double
              premulR = round (fromIntegral r * a') :: Word8
              premulG = round (fromIntegral g * a') :: Word8
              premulB = round (fromIntegral b * a') :: Word8
            in
               -- Cairo ARGB32 on little-endian: B G R A in memory
              premulB : premulG : premulR : a : convertRow restRow (x + 1)

      cairoData = BS.pack $ convertPixels srcBytes 0 0

  -- Create Cairo surface from converted data
  -- We use imageSurfaceCreateFromPNG or createImageSurface and copy data
  -- But since withImageSurfaceForData requires keeping memory alive,
  -- we need to use a different approach

  -- Create a new surface and render the converted data to it
  surface <- Cairo.createImageSurface Cairo.FormatARGB32 (fromIntegral w) (fromIntegral h)

  -- Use withImageSurfaceForData to create a temporary surface with our data
  -- then copy it to the main surface using Cairo rendering
  BS.useAsCString cairoData $ \ptr -> do
    let pixelData = castPtr ptr
    Cairo.withImageSurfaceForData
      pixelData
      Cairo.FormatARGB32
      (fromIntegral w)
      (fromIntegral h)
      cairoStride
      $ \srcSurface -> do
        -- Create a context for the destination surface and draw source onto it
        Cairo.renderWith surface $ do
          Cairo.setSourceSurface srcSurface 0 0
          Cairo.paint

  return $ Just surface

-- | Set the balloon position (absolute).
-- This sets the balloon's base position and moves the window.
-- Use this for initial positioning or repositioning the balloon.
setBalloonPosition :: BalloonState -> Int -> Int -> IO ()
setBalloonPosition bs x y = do
  writeIORef (bsPosition bs) ( x, y )
  -- Move the window using unified Platform API
  _ <- setWindowPosition (bsWindow bs) (fromIntegral x) (fromIntegral y)
  putStrLn $ "[Balloon] Set position to: " <> show x <> ", " <> show y

-- | Update the balloon position relative to a character window.
-- Also auto-switches balloon surface based on position relative to character.
updateBalloonPosition :: BalloonState
                      -> Double           -- ^ Delta x
                      -> Double           -- ^ Delta y
                      -> IO ()
updateBalloonPosition bs dx dy = do
  ( baseX, baseY ) <- readIORef (bsPosition bs)
  let finalX = baseX + round dx
      finalY = baseY + round dy
  writeIORef (bsPosition bs) ( finalX, finalY )

  -- Move the window using unified Platform API
  success <- setWindowPosition (bsWindow bs) (fromIntegral finalX) (fromIntegral finalY)
  when success $ putStrLn $ "[Balloon] Moved to: " <> show finalX <> ", " <> show finalY

-- | Update balloon position with character position info for auto surface switching.
-- When balloon crosses to the other side of the character, automatically switch surface.
updateBalloonPositionWithChar
  :: BalloonState
  -> Double           -- ^ Delta x
  -> Double           -- ^ Delta y
  -> Int              -- ^ Character X position
  -> Int              -- ^ Character width
  -> IO ()
updateBalloonPositionWithChar bs dx dy charX charW = do
  ( baseX, baseY ) <- readIORef (bsPosition bs)
  let finalX = baseX + round dx
      finalY = baseY + round dy
  writeIORef (bsPosition bs) ( finalX, finalY )

  -- Get balloon size for center calculation
  ( balloonW, _ ) <- getBalloonSize bs
  let balloonCenterX = finalX + (balloonW `div` 2)
      charCenterX    = charX + (charW `div` 2)

  -- Direction: 0 = left of char (balloons0.png), 1 = right of char (balloons1.png)
  currentDir <- readIORef (bsDirection bs)
  let newDir
        = if balloonCenterX < charCenterX
          then 0  -- Balloon is to the left of character
          else 1  -- Balloon is to the right of character

  -- Switch surface if direction changed
  when (newDir /= currentDir) $ do
    writeIORef (bsDirection bs) newDir
    -- Surface index = balloonId * 2 + direction
    -- direction: 0 = left, 1 = right
    mBalloonDir <- readIORef (bsBalloonDir bs)
    charType <- readIORef (bsCharType bs)
    balloonId <- readIORef (bsBalloonId bs)
    let surfaceIndex = balloonId * 2 + newDir
    case mBalloonDir of
      Just balloonDir -> do
        _ <- loadAndSetBalloonSurface bs balloonDir charType surfaceIndex
        putStrLn
          $ "[Balloon] Direction changed to: "
          <> show newDir
          <> " (surface index "
          <> show surfaceIndex
          <> ")"
      Nothing         -> pure ()

  -- Move the window using unified Platform API
  success <- setWindowPosition (bsWindow bs) (fromIntegral finalX) (fromIntegral finalY)
  when success $ putStrLn $ "[Balloon] Moved to: " <> show finalX <> ", " <> show finalY

-- | Get the current balloon window size.
-- Returns (width, height) in pixels.
getBalloonSize :: BalloonState -> IO ( Int, Int )
getBalloonSize bs = do
  -- Try to get the size from the surface pixbuf first
  mPixbuf <- readIORef (bsSurface bs)
  case mPixbuf of
    Just pixbuf -> do
      w <- Pixbuf.pixbufGetWidth pixbuf
      h <- Pixbuf.pixbufGetHeight pixbuf
      return ( fromIntegral w, fromIntegral h )
    Nothing     -> do
      -- Fall back to config-based size
      config <- readIORef (bsConfig bs)
      return
        ( bcOriginX config * 2 + bcValidWidth config, bcOriginY config * 2 + bcValidHeight config )

-- | Scroll up by one line.
scrollUp :: BalloonState -> IO ()
scrollUp bs = do
  modifyIORef' (bsScrollLine bs) (\n -> max 0 (n - 1))
  Gtk.widgetQueueDraw (bsDrawArea bs)

-- | Scroll down by one line.
scrollDown :: BalloonState -> IO ()
scrollDown bs = do
  modifyIORef' (bsScrollLine bs) (+ 1)
  Gtk.widgetQueueDraw (bsDrawArea bs)

-- | Set whether auto-scroll is enabled.
-- When enabled (default), the balloon automatically scrolls to show the last
-- line of text when new text is added that would overflow the visible area.
setAutoScroll :: BalloonState -> Bool -> IO ()
setAutoScroll bs = writeIORef (bsAutoScroll bs)

-- | Get current auto-scroll setting.
getAutoScroll :: BalloonState -> IO Bool
getAutoScroll bs = readIORef (bsAutoScroll bs)

-- | Auto-scroll to show the last line of text.
-- This calculates the text height using Pango and adjusts the scroll position
-- so that the bottom of the text is visible within the valid area.
-- Called automatically when text is appended and auto-scroll is enabled.
autoScrollToLastLine :: BalloonState -> IO ()
autoScrollToLastLine bs = do
  config <- readIORef (bsConfig bs)
  segments <- readIORef (bsText bs)
  let fullText = T.concat (map tsText segments)

  -- Early return if no text
  unless (T.null fullText) $ do
    -- Get a Pango context from the drawing area widget
    pangoCtx <- Gtk.widgetGetPangoContext (bsDrawArea bs)
    layout <- Pango.layoutNew pangoCtx

    -- Set up layout same as in drawText
    Pango.layoutSetText layout fullText (-1)

    fontDesc <- Pango.fontDescriptionNew
    Pango.fontDescriptionSetFamily fontDesc (bcFontName config)
    Pango.fontDescriptionSetSize
      fontDesc
      (fromIntegral $ bcFontSize config * fromIntegral Pango.SCALE)
    Pango.layoutSetFontDescription layout (Just fontDesc)

    -- Use CHAR wrapping like in drawText
    Pango.layoutSetWidth layout (fromIntegral $ bcValidWidth config * fromIntegral Pango.SCALE)
    Pango.layoutSetWrap layout Pango.WrapModeChar
    Pango.layoutSetSpacing layout (fromIntegral $ bcLineSpacing config * fromIntegral Pango.SCALE)

    -- Get text height and calculate line height
    ( _, textHeight ) <- Pango.layoutGetPixelSize layout
    metrics <- Pango.contextGetMetrics pangoCtx (Just fontDesc) Nothing
    ascent <- Pango.fontMetricsGetAscent metrics
    descent <- Pango.fontMetricsGetDescent metrics
    let lineHeight
          = (fromIntegral ascent
             + fromIntegral descent
             + fromIntegral (bcLineSpacing config * fromIntegral Pango.SCALE))
            / fromIntegral Pango.SCALE :: Double

    -- Calculate how much text overflows the valid area
    let validHeight     = fromIntegral (bcValidHeight config) :: Double
        totalTextHeight = fromIntegral textHeight :: Double

    -- If text overflows, calculate scroll lines needed
    when (totalTextHeight > validHeight && lineHeight > 0) $ do
      let overflow          = totalTextHeight - validHeight
          scrollLinesNeeded = ceiling (overflow / lineHeight)
      writeIORef (bsScrollLine bs) scrollLinesNeeded

--------------------------------------------------------------------------------
-- Choice Support
--------------------------------------------------------------------------------

-- | Add a choice to the balloon.
-- Choices are displayed after the current text and can be clicked by the user.
addChoice :: BalloonState -> BalloonChoice -> IO ()
addChoice bs choice = do
  modifyIORef' (bsChoices bs) (<> [ choice ])
  Gtk.widgetQueueDraw (bsDrawArea bs)
  showBalloon bs

-- | Clear all choices from the balloon.
clearChoices :: BalloonState -> IO ()
clearChoices bs = do
  writeIORef (bsChoices bs) []
  writeIORef (bsChoiceRects bs) []
  Gtk.widgetQueueDraw (bsDrawArea bs)

-- | Check if balloon has any active choices
hasChoices :: BalloonState -> IO Bool
hasChoices bs = do
  choices <- readIORef (bsChoices bs)
  return (not (null choices))

-- | Set the callback to be called when a choice is selected.
setChoiceCallback :: BalloonState -> (BalloonChoice -> IO ()) -> IO ()
setChoiceCallback bs callback = writeIORef (bsChoiceCallback bs) (Just callback)

-- | Find which choice (if any) was clicked given coordinates.
findClickedChoice :: Double
                  -> Double
                  -> [ ( BalloonChoice, Double, Double, Double, Double ) ]
                  -> Maybe BalloonChoice
findClickedChoice x y rects = case filter isInside rects of
  [] -> Nothing
  (( c, _, _, _, _ ) : _) -> Just c
  where
    isInside ( _, rx, ry, rw, rh ) = x >= rx && x <= rx + rw && y >= ry && y <= ry + rh

--------------------------------------------------------------------------------
-- SakuraScript Text Extraction
--------------------------------------------------------------------------------

-- | Extract plain text from a SakuraScript.
-- This is a simple implementation that ignores most tags and just extracts
-- displayable text. For testing purposes only.
extractPlainText :: Script -> T.Text
extractPlainText = T.concat . map extractElement
  where
    extractElement :: SakuraScript -> T.Text
    extractElement (SSText txt) = txt
    extractElement (SSBalloon cmd) = extractBalloonCmd cmd
    extractElement (SSEscape c) = T.singleton c
    extractElement _ = ""  -- Ignore other commands for now

    extractBalloonCmd :: BalloonCmd -> T.Text
    extractBalloonCmd Newline = "\n"
    extractBalloonCmd NewlineHalf = "\n"
    extractBalloonCmd (NewlinePercent _) = "\n"
    extractBalloonCmd Clear = ""  -- Could return empty to trigger clear
    extractBalloonCmd _ = ""
