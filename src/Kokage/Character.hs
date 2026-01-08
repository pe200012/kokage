{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Character state management for multi-character ghosts (sakura, kero, char2, ...).
-- Each character has its own surface window and balloon window.
module Kokage.Character
  ( -- * Character State
    CharacterState(..)
  , CharacterMap
  , BalloonDirection(..)
    -- * Character Creation
  , createCharacter
  , destroyCharacter
    -- * Character Operations
  , showCharacter
  , hideCharacter
  , isCharacterVisible
  , tickCharacter
    -- * Surface Operations
  , setCharacterSurface
  , getCharacterSurface
    -- * Balloon Operations
  , getCharacterBalloon
  , initBalloonPosition
  , updateBalloonPosition
  , flipBalloonDirection
    -- * Position Operations
  , setCharacterPosition
  , getCharacterPosition
    -- * Utilities
  , getDefaultSurfaceId
  , getBalloonOffset
    -- * Surface Life Timer
  , startSurfaceLifeTimer
  , cancelSurfaceLifeTimer
  ) where

import           Control.Monad      ( unless, void, when )

import           Data.GI.Base       ( AttrOp((:=)), new )
import           Data.IORef         ( IORef, newIORef, readIORef, writeIORef )
import           Data.Int           ( Int32 )
import           Data.Map.Strict    ( Map )
import           Data.Maybe         ( fromMaybe )
import qualified Data.Text          as T
import           Data.Word          ( Word32 )

import qualified GI.GLib            as GLib
import qualified GI.Gdk             as Gdk
import qualified GI.GdkPixbuf       as Pixbuf
import qualified GI.Gio             as Gio
import qualified GI.Gtk             as Gtk

import           Kokage.Animation   ( AnimationState(..)
                                    , clearAnimations
                                    , compositeAnimation
                                    , invokeAlways
                                    , invokeRunonce
                                    , newAnimationState
                                    , tickAnimations
                                    )
import           Kokage.Balloon     ( BalloonDirection(..)
                                    , BalloonState
                                    , getBalloonDir
                                    , initBalloonAlwaysOnTop
                                    , loadAndSetBalloonSurface
                                    , newBalloonState
                                    , newBalloonStateWithSurface
                                    )
import qualified Kokage.Balloon     as Balloon
import           Kokage.InputRegion ( setInputRegionFromPixbuf )
import           Kokage.Platform    ( Layer(..)
                                    , initPlatformWindow
                                    , setWindowAlwaysOnTop
                                    , setWindowLayer
                                    , setWindowPosition
                                    )
import           Kokage.Surface     ( compositeSurface, findSurfaceById )

import           Types.Ghost        ( CharacterSettings(..)
                                    , GhostDescript(..)
                                    , Shell(..)
                                    , SurfaceDefinition(..)
                                    , getCharSettings
                                    )

-- | State for a single character (sakura=0, kero=1, char2, char3, ...).
data CharacterState
  = CharacterState
  { csWindow           :: !Gtk.Window             -- ^ Surface window
  , csPicture          :: !Gtk.Picture            -- ^ Picture widget displaying the surface
  , csCurrentSurface   :: !(IORef Int)            -- ^ Current surface ID
  , csSurfaceSize      :: !(IORef ( Int, Int ))     -- ^ Current surface size (width, height)
  , csBalloon          :: !BalloonState           -- ^ Balloon for this character
  , csPosition         :: !(IORef ( Int32, Int32 )) -- ^ Window position (x, y)
  , csBalloonDir       :: !(IORef BalloonDirection) -- ^ Balloon direction (left/right)
  , csDefaultSurface   :: !Int                    -- ^ Default surface ID for this character
  , csVisible          :: !(IORef Bool)           -- ^ Whether character is currently shown
  , csScopeId          :: !Int                    -- ^ Scope index (0=sakura, 1=kero, etc.)
  , csLayerShell       :: !(IORef Bool)           -- ^ Whether layer-shell is active
  , csAnimState        :: !AnimationState         -- ^ Animation state manager
  , csSurfaceLifeTimer :: !(IORef (Maybe Word32)) -- ^ Active surface_life timer source ID
  }

-- | Map from scope ID to character state.
type CharacterMap = Map Int CharacterState

-- | Create a character with its surface window and balloon.
-- Returns the CharacterState if successful.
--
-- The balloon directory is used to load the correct balloon surface.
-- Character type is determined by scope:
-- - Scope 0 (sakura): uses "s" -> balloons0.png, balloons1.png, etc.
-- - Scope 1+ (kero, etc.): uses "k" -> balloonk0.png, balloonk1.png, etc.
createCharacter :: Gtk.Application  -- ^ Parent application
                -> Shell            -- ^ Shell containing surface definitions
                -> GhostDescript    -- ^ Ghost descript for character names and defaults
                -> Int              -- ^ Scope ID (0=sakura, 1=kero, 2+=char*)
                -> Maybe FilePath   -- ^ Balloon directory (Nothing = no balloon surface)
                -> IO (Maybe CharacterState)
createCharacter app shell ghostDesc scopeId mBalloonDir = do
  -- Determine default surface ID and character name
  let ( defaultSurfId, charName ) = getDefaultSurfaceId ghostDesc scopeId
      surfaces = shellSurfaces shell

  -- Find and composite the default surface
  case findSurfaceById defaultSurfId surfaces of
    Nothing      -> do
      putStrLn
        $ "[Character " <> show scopeId <> "] Surface " <> show defaultSurfId <> " not found"
      return Nothing
    Just surfDef -> do
      mPixbuf <- compositeSurface (shellPath shell) surfDef
      case mPixbuf of
        Nothing     -> do
          putStrLn $ "[Character " <> show scopeId <> "] Failed to composite surface"
          return Nothing
        Just pixbuf -> do
          width <- Pixbuf.pixbufGetWidth pixbuf
          height <- Pixbuf.pixbufGetHeight pixbuf

          -- Create the surface window
          window <- new
            Gtk.Window
            [ #application := app
            , #title := charName
            , #defaultWidth := width
            , #defaultHeight := height
            , #resizable := False
            , #decorated := False
            ]

          -- Make window transparent
          cssProvider <- new Gtk.CssProvider []
          Gtk.cssProviderLoadFromString
            cssProvider
            "window.transparent { background-color: transparent; }"
          display <- Gdk.displayGetDefault
          case display of
            Nothing -> return ()
            Just d  -> Gtk.styleContextAddProviderForDisplay d cssProvider 800
          Gtk.widgetAddCssClass window "transparent"

          -- Create texture and picture
          texture <- Gdk.textureNewForPixbuf pixbuf
          picture <- new Gtk.Picture [ #paintable := texture, #canShrink := False ]
          Gtk.windowSetChild window (Just picture)

          -- Initialize state refs
          surfaceRef <- newIORef defaultSurfId
          surfaceSizeRef <- newIORef ( fromIntegral width, fromIntegral height )
          posRef <- newIORef ( 0, 0 )
          -- Default balloon direction: sakura=right, others=left
          let defaultDir
                = if scopeId == 0
                  then BalloonRight
                  else BalloonLeft
          dirRef <- newIORef defaultDir
          visibleRef <- newIORef False
          layerShellRef <- newIORef False

          -- Initialize animation state
          animState <- newAnimationState
          -- Set the initial base pixbuf
          writeIORef (asBasePixbuf animState) (Just pixbuf)

          -- Determine character type for balloon surface
          -- Scope 0 = sakura -> "s", Scope 1+ = kero -> "k"
          let charType
                = if scopeId == 0
                  then "s"
                  else "k"

          -- Create balloon for this character
          -- If balloon directory is provided, load the appropriate surface
          balloon <- case mBalloonDir of
            Just balloonDir -> do
              putStrLn
                $ "[Character "
                <> show scopeId
                <> "] Loading balloon surface from: "
                <> balloonDir
                <> " (type="
                <> T.unpack charType
                <> ")"
              newBalloonStateWithSurface app balloonDir charType
            Nothing         -> do
              putStrLn
                $ "[Character " <> show scopeId <> "] No balloon directory, using default balloon"
              newBalloonState app
          _ <- initBalloonAlwaysOnTop balloon

          -- Try to initialize platform (layer-shell on Wayland)
          layerShellSuccess <- initPlatformWindow window
          writeIORef layerShellRef layerShellSuccess

          -- Initialize surface life timer ref
          surfaceLifeTimerRef <- newIORef Nothing

          let charState
                = CharacterState
                { csWindow           = window
                , csPicture          = picture
                , csCurrentSurface   = surfaceRef
                , csSurfaceSize      = surfaceSizeRef
                , csBalloon          = balloon
                , csPosition         = posRef
                , csBalloonDir       = dirRef
                , csDefaultSurface   = defaultSurfId
                , csVisible          = visibleRef
                , csScopeId          = scopeId
                , csLayerShell       = layerShellRef
                , csAnimState        = animState
                , csSurfaceLifeTimer = surfaceLifeTimerRef
                }

          putStrLn
            $ "[Character "
            <> show scopeId
            <> "] Created: "
            <> T.unpack charName
            <> " (surface "
            <> show defaultSurfId
            <> ")"

          return $ Just charState

-- | Destroy a character's windows and resources.
destroyCharacter :: CharacterState -> IO ()
destroyCharacter cs = do
  Gtk.windowDestroy (csWindow cs)

  -- Note: Balloon destruction is handled by GTK when application closes

-- | Show a character's surface window.
-- Uses platform-specific always-on-top depending on backend.
showCharacter :: CharacterState -> IO ()
showCharacter cs = do
  visible <- readIORef (csVisible cs)
  unless visible $ do
    isLayerShell <- readIORef (csLayerShell cs)
    if isLayerShell
      then do
        setWindowLayer (csWindow cs) LayerTop
        Gtk.windowPresent (csWindow cs)
      else do
        Gtk.windowPresent (csWindow cs)
        _ <- setWindowAlwaysOnTop (csWindow cs) True
        return ()
    writeIORef (csVisible cs) True
    putStrLn $ "[Character " <> show (csScopeId cs) <> "] Shown"

-- | Hide a character's surface window.
hideCharacter :: CharacterState -> IO ()
hideCharacter cs = do
  visible <- readIORef (csVisible cs)
  when visible $ do
    Gtk.widgetSetVisible (csWindow cs) False
    writeIORef (csVisible cs) False
    putStrLn $ "[Character " <> show (csScopeId cs) <> "] Hidden"

-- | Check if a character is currently visible.
isCharacterVisible :: CharacterState -> IO Bool
isCharacterVisible = readIORef . csVisible

-- | Change a character's displayed surface.
-- This is thread-safe and schedules GTK operations on the main thread.
setCharacterSurface :: CharacterState -> Shell -> Int -> IO ()
setCharacterSurface cs shell newSurfId = do
  _currentId <- readIORef (csCurrentSurface cs)
  -- Allow reloading same surface to reset animations if needed (e.g., runonce)
  -- But for optimization we check if ID is different OR if forced refresh is desired.
  -- For now, always reload to support surface-specific animations correctly.
  do
    let surfaces = shellSurfaces shell
    case findSurfaceById newSurfId surfaces of
      Nothing      -> putStrLn
        $ "[Character " <> show (csScopeId cs) <> "] Surface " <> show newSurfId <> " not found"
      Just surfDef -> do
        mPixbuf <- compositeSurface (shellPath shell) surfDef
        case mPixbuf of
          Nothing     -> putStrLn
            $ "[Character "
            <> show (csScopeId cs)
            <> "] Failed to composite surface "
            <> show newSurfId
          Just pixbuf -> do
            -- Get surface dimensions for future use
            w <- Pixbuf.pixbufGetWidth pixbuf
            h <- Pixbuf.pixbufGetHeight pixbuf

            -- Update animation state
            -- Clear and reinitialize animations for the new surface
            let animState = csAnimState cs
            clearAnimations animState
            writeIORef (asBasePixbuf animState) (Just pixbuf)

            -- Invoke runonce and always animations for this surface
            activeAnims0 <- invokeRunonce surfDef []
            activeAnims1 <- invokeAlways surfDef activeAnims0
            writeIORef (asActiveAnims animState) activeAnims1

            -- Schedule GTK operations on main thread
            _ <- GLib.idleAdd GLib.PRIORITY_HIGH $ do
              texture <- Gdk.textureNewForPixbuf pixbuf
              Gtk.pictureSetPaintable (csPicture cs) (Just texture)
              -- Update input region
              mSurface <- Gtk.nativeGetSurface (csWindow cs)
              case mSurface of
                Nothing         -> return ()
                Just gdkSurface -> void $ setInputRegionFromPixbuf gdkSurface pixbuf
              writeIORef (csCurrentSurface cs) newSurfId
              writeIORef (csSurfaceSize cs) ( fromIntegral w, fromIntegral h )
              putStrLn
                $ "[Character " <> show (csScopeId cs) <> "] Surface changed to " <> show newSurfId
              return False
            return ()

-- | Tick animation for the character.
-- Should be called periodically (e.g. 50ms).
tickCharacter :: CharacterState -> Shell -> Int -> IO ()
tickCharacter cs shell delta = do
  -- Only process if visible
  visible <- readIORef (csVisible cs)
  when visible $ do
    let animState = csAnimState cs
    surfId <- readIORef (csCurrentSurface cs)

    -- Find current surface definition
    case findSurfaceById surfId (shellSurfaces shell) of
      Nothing      -> return ()
      Just surfDef -> do
        activeAnims <- readIORef (asActiveAnims animState)
        currentTimers <- readIORef (asPeriodicState animState)

        -- Tick animations
        ( newAnims, newTimers, needsRedraw )
          <- tickAnimations animState shell surfDef activeAnims currentTimers delta
        writeIORef (asActiveAnims animState) newAnims
        writeIORef (asPeriodicState animState) newTimers

        -- If visual state changed, composite and update
        when needsRedraw $ do
          mBasePixbuf <- readIORef (asBasePixbuf animState)
          case mBasePixbuf of
            Nothing         -> return ()
            Just basePixbuf -> do
              -- Composite active animations onto base (using the cache)
              mFinalPixbuf <- compositeAnimation shell (asImageCache animState) basePixbuf newAnims
              case mFinalPixbuf of
                Nothing          -> return ()
                Just finalPixbuf -> do
                  -- Update the picture (must be on main thread)
                  _ <- GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
                    texture <- Gdk.textureNewForPixbuf finalPixbuf
                    Gtk.pictureSetPaintable (csPicture cs) (Just texture)
                    return False
                  return ()

-- | Get the current surface ID for a character.
getCharacterSurface :: CharacterState -> IO Int
getCharacterSurface = readIORef . csCurrentSurface

-- | Get the balloon state for a character.
getCharacterBalloon :: CharacterState -> BalloonState
getCharacterBalloon = csBalloon

-- | Initialize the balloon position based on character position.
-- This should be called after the character's initial position is set.
--
-- The balloon is positioned relative to the character:
-- - BalloonRight: Balloon to the right of the character (surface index 0)
-- - BalloonLeft: Balloon to the left of the character (surface index 1)
--
-- Edge detection:
-- - If balloon would go out of screen bounds, flip direction
-- - Clamp final position to screen bounds
--
-- Takes into account:
-- - Character window position
-- - Character surface size
-- - Balloon size
-- - Balloon direction
-- - Shell-defined offsets (from getBalloonOffset)
-- - Monitor geometry for edge detection
initBalloonPosition :: CharacterState -> Shell -> IO ()
initBalloonPosition cs shell = do
  -- Get character position and size
  ( charX, charY ) <- readIORef (csPosition cs)
  ( surfW, surfH ) <- readIORef (csSurfaceSize cs)

  -- Get balloon size
  balloonSize <- Balloon.getBalloonSize (csBalloon cs)
  let ( balloonW, balloonH ) = balloonSize

  -- Get current surface and direction
  surfId <- readIORef (csCurrentSurface cs)
  dir <- readIORef (csBalloonDir cs)

  -- Get monitor geometry for edge detection (use character position to find correct monitor)
  ( monX, monY, monW, monH ) <- getMonitorGeometry (fromIntegral charX) (fromIntegral charY)

  -- Get offset from shell/surface definitions
  let ( offsetX, offsetY ) = getBalloonOffset shell (csScopeId cs) surfId dir

  -- BalloonLeft: balloon to the left of character (direction 0, uses balloons0.png)
  -- BalloonRight: balloon to the right of character (direction 1, uses balloons1.png)
  -- lx = sx - bw + ox, rx = sx + sw - ox
  let leftPosX  = fromIntegral charX - balloonW + offsetX
      rightPosX = fromIntegral charX + surfW - offsetX
      basePosY  = fromIntegral charY + offsetY

  -- Detect edge and determine final direction
  -- If current direction would place balloon out of bounds, try flipping
  ( finalDir, finalX ) <- case dir of
    BalloonRight -> if rightPosX + balloonW > monX + monW && leftPosX >= monX
      then do
        -- Right side overflows, left side fits -> flip to left
        putStrLn
          $ "[Character " <> show (csScopeId cs) <> "] Flipping balloon to left (edge detection)"
        return ( BalloonLeft, leftPosX )
      else 
        -- Keep right, clamp if needed
        return ( BalloonRight, max monX (min rightPosX (monX + monW - balloonW)) )
    BalloonLeft  -> if leftPosX < monX && rightPosX + balloonW <= monX + monW
      then do
        -- Left side overflows, right side fits -> flip to right
        putStrLn
          $ "[Character " <> show (csScopeId cs) <> "] Flipping balloon to right (edge detection)"
        return ( BalloonRight, rightPosX )
      else 
        -- Keep left, clamp if needed
        return ( BalloonLeft, max monX (min leftPosX (monX + monW - balloonW)) )

  -- Clamp Y position to screen bounds
  let finalY = max monY (min basePosY (monY + monH - balloonH))

  -- If direction changed, update state and switch balloon surface
  when (finalDir /= dir) $ do
    writeIORef (csBalloonDir cs) finalDir
    -- Switch balloon surface based on direction
    -- BalloonLeft uses index 0 (balloons0.png), BalloonRight uses index 1 (balloons1.png)
    let charType
          = if csScopeId cs == 0
            then "s"
            else "k"
        surfIndex = case finalDir of
          BalloonLeft  -> 0
          BalloonRight -> 1
    mBalloonDir <- getBalloonDir (csBalloon cs)
    case mBalloonDir of
      Nothing   -> return ()
      Just bDir -> void $ loadAndSetBalloonSurface (csBalloon cs) bDir charType surfIndex

  putStrLn
    $ "[Character "
    <> show (csScopeId cs)
    <> "] Balloon position:"
    <> " char=("
    <> show charX
    <> ","
    <> show charY
    <> ")"
    <> " surfSize=("
    <> show surfW
    <> ","
    <> show surfH
    <> ")"
    <> " balloonSize="
    <> show balloonSize
    <> " dir="
    <> show finalDir
    <> " monitor=("
    <> show monX
    <> ","
    <> show monY
    <> ","
    <> show monW
    <> ","
    <> show monH
    <> ")"
    <> " -> balloon=("
    <> show finalX
    <> ","
    <> show finalY
    <> ")"

  -- Set the balloon position
  Balloon.setBalloonPosition (csBalloon cs) finalX finalY

-- | Get the geometry of the monitor containing the given point.
-- Returns (x, y, width, height) of the monitor workarea.
-- Falls back to a default 1920x1080 if detection fails.
getMonitorGeometry :: Int -> Int -> IO ( Int, Int, Int, Int )
getMonitorGeometry pointX pointY = do
  mDisplay <- Gdk.displayGetDefault
  case mDisplay of
    Nothing      -> return ( 0, 0, 1920, 1080 )  -- Fallback
    Just display -> do
      monitors <- Gdk.displayGetMonitors display
      n <- Gio.listModelGetNItems monitors
      -- Debug: print all monitors
      putStrLn
        $ "[MonitorDebug] Looking for point ("
        <> show pointX
        <> ","
        <> show pointY
        <> ") in "
        <> show n
        <> " monitors"
      allMonitors <- collectAllMonitors monitors n 0 []
      mapM_ (\( i, mx, my, mw, mh ) -> putStrLn
             $ "[MonitorDebug]   Monitor "
             <> show i
             <> ": ("
             <> show mx
             <> ","
             <> show my
             <> ") "
             <> show mw
             <> "x"
             <> show mh) allMonitors
      findMonitorContainingPoint monitors n 0
  where
    collectAllMonitors :: Gio.ListModel
                       -> Word32
                       -> Word32
                       -> [ ( Int, Int, Int, Int, Int ) ]
                       -> IO [ ( Int, Int, Int, Int, Int ) ]
    collectAllMonitors monitors total idx acc
      | idx >= total = return (reverse acc)
      | otherwise = do
        mObj <- Gio.listModelGetItem monitors idx
        case mObj of
          Nothing  -> collectAllMonitors monitors total (idx + 1) acc
          Just obj -> do
            monitor <- Gdk.unsafeCastTo Gdk.Monitor obj
            rect <- Gdk.monitorGetGeometry monitor
            mx <- Gdk.getRectangleX rect
            my <- Gdk.getRectangleY rect
            mw <- Gdk.getRectangleWidth rect
            mh <- Gdk.getRectangleHeight rect
            collectAllMonitors
              monitors
              total
              (idx + 1)
              (( fromIntegral idx
               , fromIntegral mx
               , fromIntegral my
               , fromIntegral mw
               , fromIntegral mh
               )
               : acc)

    -- Find the monitor that contains the given point
    findMonitorContainingPoint :: Gio.ListModel -> Word32 -> Word32 -> IO ( Int, Int, Int, Int )
    findMonitorContainingPoint monitors total idx
      | idx >= total = return ( 0, 0, 1920, 1080 )  -- Fallback if no monitor found
      | otherwise = do
        mObj <- Gio.listModelGetItem monitors idx
        case mObj of
          Nothing  -> findMonitorContainingPoint monitors total (idx + 1)
          Just obj -> do
            monitor <- Gdk.unsafeCastTo Gdk.Monitor obj
            rect <- Gdk.monitorGetGeometry monitor
            mx <- Gdk.getRectangleX rect
            my <- Gdk.getRectangleY rect
            mw <- Gdk.getRectangleWidth rect
            mh <- Gdk.getRectangleHeight rect
            let monX = fromIntegral mx
                monY = fromIntegral my
                monW = fromIntegral mw
                monH = fromIntegral mh
            -- Check if point is within this monitor's bounds
            if pointX >= monX && pointX < monX + monW && pointY >= monY && pointY < monY + monH
              then return ( monX, monY, monW, monH )
              else findMonitorContainingPoint monitors total (idx + 1)

-- | Update the balloon position based on character surface position.
-- Takes into account balloon direction and shell-defined offsets.
-- Also auto-switches balloon surface when balloon crosses to other side of character.
updateBalloonPosition :: CharacterState -> Double -> Double -> IO ()
updateBalloonPosition cs dx dy = do
  -- Get character position and size for relative position detection
  ( charX, _charY ) <- readIORef (csPosition cs)
  ( charW, _charH ) <- readIORef (csSurfaceSize cs)

  -- Use the Balloon module's updateBalloonPositionWithChar function
  -- which handles position update and auto surface switching
  Balloon.updateBalloonPositionWithChar (csBalloon cs) dx dy (fromIntegral charX) charW

-- | Flip the balloon direction for a character.
flipBalloonDirection :: CharacterState -> IO ()
flipBalloonDirection cs = do
  dir <- readIORef (csBalloonDir cs)
  let newDir = case dir of
        BalloonLeft  -> BalloonRight
        BalloonRight -> BalloonLeft
  writeIORef (csBalloonDir cs) newDir
  putStrLn $ "[Character " <> show (csScopeId cs) <> "] Balloon direction: " <> show newDir

-- | Set a character's window position.
setCharacterPosition :: CharacterState -> Int32 -> Int32 -> IO ()
setCharacterPosition cs x y = do
  writeIORef (csPosition cs) ( x, y )
  -- Use unified platform positioning
  _ <- setWindowPosition (csWindow cs) x y
  return ()

-- | Get a character's current window position.
getCharacterPosition :: CharacterState -> IO ( Int32, Int32 )
getCharacterPosition = readIORef . csPosition

-- | Get the default surface ID and character name for a scope.
getDefaultSurfaceId :: GhostDescript -> Int -> ( Int, T.Text )
getDefaultSurfaceId desc scopeId = case scopeId of
  0 -> ( descriptSakuraSerikoDefaultSurface desc, descriptSakuraName desc )
  1 -> ( descriptKeroSerikoDefaultSurface desc, descriptKeroName desc )
  n -> ( 10 + n * 10, "char" <> T.pack (show n) )  -- Default: char2=30, char3=40, etc.

-- | Get balloon offset for a character/surface combination.
-- Checks surface-specific offsets first, then falls back to shell descript.
getBalloonOffset :: Shell -> Int -> Int -> BalloonDirection -> ( Int, Int )
getBalloonOffset shell scopeId surfId dir
  = let
      surfaces      = shellSurfaces shell
      mSurfDef      = findSurfaceById surfId surfaces
      shellDesc     = shellDescript shell
      charSettings  = getCharSettings scopeId shellDesc

      -- Get offset from surface definition
      surfOffset    = case mSurfDef of
        Nothing -> ( Nothing, Nothing )
        Just sd -> case scopeId of
          0 -> ( sdSakuraBalloonOffsetX sd, sdSakuraBalloonOffsetY sd )
          1 -> ( sdKeroBalloonOffsetX sd, sdKeroBalloonOffsetY sd )
          _ -> ( sdBalloonOffsetX sd, sdBalloonOffsetY sd )

      -- Get offset from shell character settings (fallback)
      charOffset    = case dir of
        BalloonLeft  -> ( csBalloonOffsetXL charSettings, csBalloonOffsetYL charSettings )
        BalloonRight -> ( csBalloonOffsetXR charSettings, csBalloonOffsetYR charSettings )

      -- Generic offset (second fallback)
      genericOffset = ( csBalloonOffsetX charSettings, csBalloonOffsetY charSettings )

      -- Resolve with fallback chain
      resolveX      = case fst surfOffset of
        Just x  -> x
        Nothing -> case fst charOffset of
          Just x  -> x
          Nothing -> fromMaybe 0 (fst genericOffset)

      resolveY      = case snd surfOffset of
        Just y  -> y
        Nothing -> case snd charOffset of
          Just y  -> y
          Nothing -> fromMaybe 0 (snd genericOffset)
    in 
      ( resolveX, resolveY )

-- | Start a surface life timer for OnSurfaceRestore event.
-- After the specified delay, the callback will be invoked to restore the default surface.
-- Any existing timer is cancelled before starting a new one.
startSurfaceLifeTimer :: CharacterState -> Word32 -> IO () -> IO ()
startSurfaceLifeTimer cs delayMs callback = do
  -- Cancel any existing timer first
  cancelSurfaceLifeTimer cs
  -- Start new timer
  sourceId <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT delayMs $ do
    callback
    return False  -- One-shot timer
  writeIORef (csSurfaceLifeTimer cs) (Just sourceId)

-- | Cancel the surface life timer if one is active.
cancelSurfaceLifeTimer :: CharacterState -> IO ()
cancelSurfaceLifeTimer cs = do
  mSourceId <- readIORef (csSurfaceLifeTimer cs)
  case mSourceId of
    Nothing       -> return ()
    Just sourceId -> do
      _ <- GLib.sourceRemove sourceId
      writeIORef (csSurfaceLifeTimer cs) Nothing
