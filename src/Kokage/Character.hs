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
  ) where

import           Control.Monad              ( void, when )
import           Data.Int                   ( Int32 )
import           Data.IORef                 ( IORef, newIORef, readIORef, writeIORef )
import           Data.Map.Strict            ( Map )
import qualified Data.Text                  as T

import           Data.GI.Base               ( AttrOp((:=)), new )
import qualified GI.Gdk                     as Gdk
import qualified GI.GdkPixbuf               as Pixbuf
import qualified GI.GLib                    as GLib
import qualified GI.Gtk                     as Gtk

import           Kokage.Balloon             ( BalloonState, BalloonDirection(..)
                                            , newBalloonState
                                            , newBalloonStateWithSurface
                                            , initBalloonAlwaysOnTop )
import qualified Kokage.Balloon             as Balloon
import           Kokage.InputRegion         ( setInputRegionFromPixbuf )
import           Kokage.Platform            ( initPlatformWindow, setWindowAlwaysOnTop
                                            , setWindowPosition, setWindowLayer, Layer(..) )
import           Kokage.Surface             ( compositeSurface, findSurfaceById )

import           Types.Ghost                ( Shell(..)
                                            , SurfaceDefinition(..)
                                            , CharacterSettings(..)
                                            , GhostDescript(..)
                                            , getCharSettings )

-- | State for a single character (sakura=0, kero=1, char2, char3, ...).
data CharacterState = CharacterState
  { csWindow         :: !Gtk.Window             -- ^ Surface window
  , csPicture        :: !Gtk.Picture            -- ^ Picture widget displaying the surface
  , csCurrentSurface :: !(IORef Int)            -- ^ Current surface ID
  , csSurfaceSize    :: !(IORef (Int, Int))     -- ^ Current surface size (width, height)
  , csBalloon        :: !BalloonState           -- ^ Balloon for this character
  , csPosition       :: !(IORef (Int32, Int32)) -- ^ Window position (x, y)
  , csBalloonDir     :: !(IORef BalloonDirection) -- ^ Balloon direction (left/right)
  , csDefaultSurface :: !Int                    -- ^ Default surface ID for this character
  , csVisible        :: !(IORef Bool)           -- ^ Whether character is currently shown
  , csScopeId        :: !Int                    -- ^ Scope index (0=sakura, 1=kero, etc.)
  , csLayerShell     :: !(IORef Bool)           -- ^ Whether layer-shell is active
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
createCharacter
  :: Gtk.Application  -- ^ Parent application
  -> Shell            -- ^ Shell containing surface definitions
  -> GhostDescript    -- ^ Ghost descript for character names and defaults
  -> Int              -- ^ Scope ID (0=sakura, 1=kero, 2+=char*)
  -> Maybe FilePath   -- ^ Balloon directory (Nothing = no balloon surface)
  -> IO (Maybe CharacterState)
createCharacter app shell ghostDesc scopeId mBalloonDir = do
  -- Determine default surface ID and character name
  let (defaultSurfId, charName) = getDefaultSurfaceId ghostDesc scopeId
      surfaces = shellSurfaces shell

  -- Find and composite the default surface
  case findSurfaceById defaultSurfId surfaces of
    Nothing -> do
      putStrLn $ "[Character " <> show scopeId <> "] Surface "
              <> show defaultSurfId <> " not found"
      return Nothing
    Just surfDef -> do
      mPixbuf <- compositeSurface (shellPath shell) surfDef
      case mPixbuf of
        Nothing -> do
          putStrLn $ "[Character " <> show scopeId <> "] Failed to composite surface"
          return Nothing
        Just pixbuf -> do
          width <- Pixbuf.pixbufGetWidth pixbuf
          height <- Pixbuf.pixbufGetHeight pixbuf

          -- Create the surface window
          window <- new Gtk.Window
            [ #application := app
            , #title := charName
            , #defaultWidth := width
            , #defaultHeight := height
            , #resizable := False
            , #decorated := False
            ]

          -- Make window transparent
          cssProvider <- new Gtk.CssProvider []
          Gtk.cssProviderLoadFromString cssProvider
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
          surfaceSizeRef <- newIORef (fromIntegral width, fromIntegral height)
          posRef <- newIORef (0, 0)
          -- Default balloon direction: sakura=right, others=left
          let defaultDir = if scopeId == 0 then BalloonRight else BalloonLeft
          dirRef <- newIORef defaultDir
          visibleRef <- newIORef False
          layerShellRef <- newIORef False

          -- Determine character type for balloon surface
          -- Scope 0 = sakura -> "s", Scope 1+ = kero -> "k"
          let charType = if scopeId == 0 then "s" else "k"

          -- Create balloon for this character
          -- If balloon directory is provided, load the appropriate surface
          balloon <- case mBalloonDir of
            Just balloonDir -> do
              putStrLn $ "[Character " <> show scopeId <> "] Loading balloon surface from: "
                      <> balloonDir <> " (type=" <> T.unpack charType <> ")"
              newBalloonStateWithSurface app balloonDir charType
            Nothing -> do
              putStrLn $ "[Character " <> show scopeId <> "] No balloon directory, using default balloon"
              newBalloonState app
          _ <- initBalloonAlwaysOnTop balloon

          -- Try to initialize platform (layer-shell on Wayland)
          layerShellSuccess <- initPlatformWindow window
          writeIORef layerShellRef layerShellSuccess

          let charState = CharacterState
                { csWindow = window
                , csPicture = picture
                , csCurrentSurface = surfaceRef
                , csSurfaceSize = surfaceSizeRef
                , csBalloon = balloon
                , csPosition = posRef
                , csBalloonDir = dirRef
                , csDefaultSurface = defaultSurfId
                , csVisible = visibleRef
                , csScopeId = scopeId
                , csLayerShell = layerShellRef
                }

          putStrLn $ "[Character " <> show scopeId <> "] Created: "
                  <> T.unpack charName <> " (surface " <> show defaultSurfId <> ")"

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
  when (not visible) $ do
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
  currentId <- readIORef (csCurrentSurface cs)
  when (currentId /= newSurfId) $ do
    let surfaces = shellSurfaces shell
    case findSurfaceById newSurfId surfaces of
      Nothing -> putStrLn $ "[Character " <> show (csScopeId cs)
                         <> "] Surface " <> show newSurfId <> " not found"
      Just surfDef -> do
        mPixbuf <- compositeSurface (shellPath shell) surfDef
        case mPixbuf of
          Nothing -> putStrLn $ "[Character " <> show (csScopeId cs)
                             <> "] Failed to composite surface " <> show newSurfId
          Just pixbuf -> do
            -- Get surface dimensions for future use
            w <- Pixbuf.pixbufGetWidth pixbuf
            h <- Pixbuf.pixbufGetHeight pixbuf
            -- Schedule GTK operations on main thread
            _ <- GLib.idleAdd GLib.PRIORITY_HIGH $ do
              texture <- Gdk.textureNewForPixbuf pixbuf
              Gtk.pictureSetPaintable (csPicture cs) (Just texture)
              -- Update input region
              mSurface <- Gtk.nativeGetSurface (csWindow cs)
              case mSurface of
                Nothing -> return ()
                Just gdkSurface -> void $ setInputRegionFromPixbuf gdkSurface pixbuf
              writeIORef (csCurrentSurface cs) newSurfId
              writeIORef (csSurfaceSize cs) (fromIntegral w, fromIntegral h)
              putStrLn $ "[Character " <> show (csScopeId cs)
                      <> "] Surface changed to " <> show newSurfId
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
-- - BalloonLeft: To the left of the character
-- - BalloonRight: To the right of the character
--
-- Takes into account:
-- - Character window position
-- - Character surface size
-- - Balloon size
-- - Balloon direction
-- - Shell-defined offsets (from getBalloonOffset)
initBalloonPosition :: CharacterState -> Shell -> IO ()
initBalloonPosition cs shell = do
  -- Get character position and size
  (charX, charY) <- readIORef (csPosition cs)
  (surfW, surfH) <- readIORef (csSurfaceSize cs)
  
  -- Get balloon size
  balloonSize <- Balloon.getBalloonSize (csBalloon cs)
  let (balloonW, _balloonH) = balloonSize
  
  -- Get current surface and direction
  surfId <- readIORef (csCurrentSurface cs)
  dir <- readIORef (csBalloonDir cs)
  
  -- Get offset from shell/surface definitions
  let (offsetX, offsetY) = getBalloonOffset shell (csScopeId cs) surfId dir
  
  -- Calculate balloon position based on direction
  -- BalloonRight: balloon to the right of character (sakura default)
  -- BalloonLeft: balloon to the left of character (kero default)
  let balloonX = case dir of
        BalloonRight -> fromIntegral charX + surfW + offsetX
        BalloonLeft  -> fromIntegral charX - balloonW + offsetX
      balloonY = fromIntegral charY + offsetY
  
  putStrLn $ "[Character " <> show (csScopeId cs) <> "] Balloon initial position:"
          <> " char=(" <> show charX <> "," <> show charY <> ")"
          <> " surfSize=(" <> show surfW <> "," <> show surfH <> ")"
          <> " balloonSize=" <> show balloonSize
          <> " dir=" <> show dir
          <> " offset=(" <> show offsetX <> "," <> show offsetY <> ")"
          <> " -> balloon=(" <> show balloonX <> "," <> show balloonY <> ")"
  
  -- Set the balloon position
  Balloon.setBalloonPosition (csBalloon cs) balloonX balloonY

-- | Update the balloon position based on character surface position.
-- Takes into account balloon direction and shell-defined offsets.
updateBalloonPosition :: CharacterState -> Double -> Double -> IO ()
updateBalloonPosition cs dx dy = do
  -- Use the Balloon module's updateBalloonPosition function
  -- which handles X11 window positioning and drag offset
  Balloon.updateBalloonPosition
    (csBalloon cs)
    dx
    dy

-- | Flip the balloon direction for a character.
flipBalloonDirection :: CharacterState -> IO ()
flipBalloonDirection cs = do
  dir <- readIORef (csBalloonDir cs)
  let newDir = case dir of
        BalloonLeft  -> BalloonRight
        BalloonRight -> BalloonLeft
  writeIORef (csBalloonDir cs) newDir
  putStrLn $ "[Character " <> show (csScopeId cs)
          <> "] Balloon direction: " <> show newDir

-- | Set a character's window position.
setCharacterPosition :: CharacterState -> Int32 -> Int32 -> IO ()
setCharacterPosition cs x y = do
  writeIORef (csPosition cs) (x, y)
  -- Use unified platform positioning
  _ <- setWindowPosition (csWindow cs) x y
  return ()

-- | Get a character's current window position.
getCharacterPosition :: CharacterState -> IO (Int32, Int32)
getCharacterPosition = readIORef . csPosition

-- | Get the default surface ID and character name for a scope.
getDefaultSurfaceId :: GhostDescript -> Int -> (Int, T.Text)
getDefaultSurfaceId desc scopeId = case scopeId of
  0 -> (descriptSakuraSerikoDefaultSurface desc, descriptSakuraName desc)
  1 -> (descriptKeroSerikoDefaultSurface desc, descriptKeroName desc)
  n -> (10 + n * 10, "char" <> T.pack (show n))  -- Default: char2=30, char3=40, etc.

-- | Get balloon offset for a character/surface combination.
-- Checks surface-specific offsets first, then falls back to shell descript.
getBalloonOffset :: Shell -> Int -> Int -> BalloonDirection -> (Int, Int)
getBalloonOffset shell scopeId surfId dir =
  let surfaces = shellSurfaces shell
      mSurfDef = findSurfaceById surfId surfaces
      shellDesc = shellDescript shell
      charSettings = getCharSettings scopeId shellDesc

      -- Get offset from surface definition
      surfOffset = case mSurfDef of
        Nothing -> (Nothing, Nothing)
        Just sd -> case scopeId of
          0 -> (sdSakuraBalloonOffsetX sd, sdSakuraBalloonOffsetY sd)
          1 -> (sdKeroBalloonOffsetX sd, sdKeroBalloonOffsetY sd)
          _ -> (sdBalloonOffsetX sd, sdBalloonOffsetY sd)

      -- Get offset from shell character settings (fallback)
      charOffset = case dir of
        BalloonLeft  -> (csBalloonOffsetXL charSettings, csBalloonOffsetYL charSettings)
        BalloonRight -> (csBalloonOffsetXR charSettings, csBalloonOffsetYR charSettings)

      -- Generic offset (second fallback)
      genericOffset = (csBalloonOffsetX charSettings, csBalloonOffsetY charSettings)

      -- Resolve with fallback chain
      resolveX = case fst surfOffset of
        Just x  -> x
        Nothing -> case fst charOffset of
          Just x  -> x
          Nothing -> maybe 0 id (fst genericOffset)

      resolveY = case snd surfOffset of
        Just y  -> y
        Nothing -> case snd charOffset of
          Just y  -> y
          Nothing -> maybe 0 id (snd genericOffset)

  in (resolveX, resolveY)
