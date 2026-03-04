{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Platform-specific window operations for Wayland and X11.
--
-- This module provides a unified interface for window management operations
-- that differ between Wayland (using layer-shell) and X11.
--
-- == Runtime Detection
--
-- The module automatically detects the display backend at runtime:
--
-- * On Wayland with layer-shell support: Uses zwlr_layer_shell_v1 protocol
--   for always-on-top and positioning.
--
-- * On X11: Uses X11 atoms and XMoveWindow for window management.
--
-- * On unsupported backends: Operations gracefully return False/no-op.
--
-- == Usage
--
-- @
-- -- Initialize window for platform-specific features
-- success <- initPlatformWindow window
--
-- -- Set always-on-top (works on both X11 and Wayland)
-- setWindowAlwaysOnTop window True
--
-- -- Set window position
-- setWindowPosition window 100 200
-- @
module Kokage.Platform
  ( -- * Platform Detection
    Backend(..)
  , detectBackend
  , isLayerShellSupported
    -- * Window Initialization
  , initPlatformWindow
  , isPlatformInitialized
    -- * Always On Top
  , setWindowAlwaysOnTop
    -- * Window Positioning
  , setWindowPosition
  , getWindowPosition
    -- * Layer-Shell Specific (Wayland)
  , Layer(..)
  , Edge(..)
  , setWindowLayer
  ) where

import           Control.Exception        ( try )

import           Data.Bits                ( (.|.) )

import           Foreign.C.Types          ( CInt )

import qualified GI.Gdk                   as Gdk
-- X11 imports (always available)
import qualified GI.GdkX11                as GdkX11
import qualified GI.Gio                   as Gio
import qualified GI.Gtk                   as Gtk
import qualified GI.Gtk4LayerShell        as LayerShell
import           GI.Gtk4LayerShell        ( Edge(..), Layer(..) )

import           Graphics.X11.Xlib        ( allocaXEvent
                                          , clientMessage
                                          , closeDisplay
                                          , defaultScreen
                                          , flush
                                          , internAtom
                                          , openDisplay
                                          , rootWindow
                                          , sendEvent
                                          , substructureNotifyMask
                                          , substructureRedirectMask
                                          )
import qualified Graphics.X11.Xlib        as X11
import           Graphics.X11.Xlib.Extras ( setClientMessageEvent', setEventType )

import           Prelude                  ()

import           Relude

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | The detected display backend.
data Backend
  = BackendWayland    -- ^ Wayland with layer-shell support
  | BackendX11        -- ^ X11/Xorg
  | BackendUnknown    -- ^ Unknown or unsupported backend
  deriving ( Show, Eq )

--------------------------------------------------------------------------------
-- Platform Detection
--------------------------------------------------------------------------------

-- | Detect the current display backend.
-- Returns 'BackendWayland' if layer-shell is supported, 'BackendX11' if
-- running on X11, or 'BackendUnknown' otherwise.
detectBackend :: IO Backend
detectBackend = do
  layerShell <- isLayerShellSupported
  if layerShell
    then return BackendWayland
    else do
      x11Available <- checkX11Available
      return
        $ if x11Available
          then BackendX11
          else BackendUnknown

-- | Check if layer-shell is supported on the current platform.
-- Returns True only if running on Wayland and the compositor supports
-- the zwlr_layer_shell_v1 protocol.
isLayerShellSupported :: IO Bool
isLayerShellSupported = do
  result <- try LayerShell.isSupported
  case result of
    Left (_ :: SomeException) -> return False
    Right supported           -> return supported

-- | Check if X11 is available (internal helper).
checkX11Available :: IO Bool
checkX11Available = do
  result <- try $ do
    dpy <- openDisplay ""
    closeDisplay dpy
  case result of
    Left (_ :: SomeException) -> return False
    Right () -> return True

--------------------------------------------------------------------------------
-- Window Initialization
--------------------------------------------------------------------------------

-- | Initialize a window for platform-specific features.
--
-- On Wayland with layer-shell: Initializes the window as a layer-shell surface.
-- This MUST be called BEFORE the window is realized (shown).
--
-- On X11: No initialization needed, but this still returns True for consistency.
--
-- Returns True if initialization was successful (or not needed), False on failure.
initPlatformWindow :: Gtk.Window -> IO Bool
initPlatformWindow window = do
  backend <- detectBackend
  case backend of
    BackendWayland -> initLayerShellWindow window
    BackendX11     -> return True
    BackendUnknown -> return False

-- | Initialize a window as a layer-shell surface (internal).
initLayerShellWindow :: Gtk.Window -> IO Bool
initLayerShellWindow window = do
  supported <- isLayerShellSupported
  if supported
    then do
      result <- try $ do
        LayerShell.initForWindow window
        LayerShell.setAnchor window EdgeLeft True
        LayerShell.setAnchor window EdgeTop True
        LayerShell.setAnchor window EdgeRight False
        LayerShell.setAnchor window EdgeBottom False
        LayerShell.setExclusiveZone window 0
      case result of
        Left (_ :: SomeException) -> return False
        Right () -> return True
    else return False

-- | Check if a window has been initialized for platform-specific features.
isPlatformInitialized :: Gtk.Window -> IO Bool
isPlatformInitialized window = do
  backend <- detectBackend
  case backend of
    BackendWayland -> LayerShell.isLayerWindow window
    BackendX11     -> return True
    BackendUnknown -> return False

--------------------------------------------------------------------------------
-- Always On Top
--------------------------------------------------------------------------------

-- | Set a window to be always on top.
--
-- On Wayland: Uses layer-shell to set the window layer to 'LayerTop'.
-- On X11: Sets the _NET_WM_STATE_ABOVE hint.
--
-- Returns True if successful, False if the operation failed or is not supported.
setWindowAlwaysOnTop :: Gtk.Window -> Bool -> IO Bool
setWindowAlwaysOnTop window setAbove = do
  backend <- detectBackend
  case backend of
    BackendWayland -> do
      if setAbove
        then setWindowLayer window LayerTop >> return True
        else setWindowLayer window LayerBottom >> return True
    BackendX11     -> setWindowAboveX11 window setAbove
    BackendUnknown -> return False

-- | Set always-on-top via X11 (internal).
setWindowAboveX11 :: Gtk.Window -> Bool -> IO Bool
setWindowAboveX11 gtkWindow setAbove = do
  result <- runMaybeT $ do
    surface <- MaybeT $ Gtk.nativeGetSurface gtkWindow
    x11Surface <- MaybeT $ Gdk.castTo GdkX11.X11Surface surface
    xid <- MaybeT $ Just <$> GdkX11.x11SurfaceGetXid x11Surface
    MaybeT $ Just <$> setWindowAboveRaw (fromIntegral xid) setAbove

  return $ result == Just True

-- | Set window above state via raw X11 (internal).
setWindowAboveRaw :: X11.Window -> Bool -> IO Bool
setWindowAboveRaw win setAbove = do
  result <- try $ do
    dpy <- openDisplay ""

    atomNetWmState <- internAtom dpy "_NET_WM_STATE" False
    atomNetWmStateAbove <- internAtom dpy "_NET_WM_STATE_ABOVE" False

    let screen = defaultScreen dpy
    root <- rootWindow dpy screen

    let action :: CInt
        action
          = if setAbove
            then 1
            else 0

    allocaXEvent $ \ev -> do
      setEventType ev clientMessage
      setClientMessageEvent'
        ev
        win
        atomNetWmState
        32
        [ action, fromIntegral atomNetWmStateAbove, 0, 1, 0 ]
      sendEvent dpy root False (substructureNotifyMask .|. substructureRedirectMask) ev

    flush dpy
    closeDisplay dpy

  case result of
    Left (_ :: SomeException) -> return False
    Right () -> return True

--------------------------------------------------------------------------------
-- Window Positioning
--------------------------------------------------------------------------------

-- | Set the position of a window.
--
-- On Wayland: Uses layer-shell margins to position the window.
-- On X11: Uses XMoveWindow to reposition the window.
--
-- Returns True if successful, False if the operation failed.
setWindowPosition :: Gtk.Window -> Int32 -> Int32 -> IO Bool
setWindowPosition window x y = do
  backend <- detectBackend
  case backend of
    BackendWayland -> setPositionLayerShell window x y >> return True
    BackendX11     -> setPositionX11 window x y
    BackendUnknown -> return False

-- | Set position via layer-shell margins (internal).
-- Layer-shell margins are relative to the monitor, not global screen coordinates.
-- We need to find which monitor the window is on and subtract its origin.
setPositionLayerShell :: Gtk.Window -> Int32 -> Int32 -> IO ()
setPositionLayerShell window x y = do
  -- Get the monitor origin to convert global coords to monitor-relative
  ( monX, monY ) <- getMonitorOriginForPoint x y
  let relX = x - monX
      relY = y - monY
  LayerShell.setMargin window EdgeLeft relX
  LayerShell.setMargin window EdgeTop relY

-- | Get the origin of the monitor containing the given point.
getMonitorOriginForPoint :: Int32 -> Int32 -> IO ( Int32, Int32 )
getMonitorOriginForPoint px py = do
  display <- Gdk.displayGetDefault
  case display of
    Nothing   -> return ( 0, 0 )
    Just disp -> do
      monitors <- Gdk.displayGetMonitors disp
      n <- Gio.listModelGetNItems monitors
      findContainingMonitor monitors n 0
  where
    findContainingMonitor :: Gio.ListModel -> Word32 -> Word32 -> IO ( Int32, Int32 )
    findContainingMonitor monitors total idx
      | idx >= total = return ( 0, 0 )  -- fallback if no monitor contains point
      | otherwise = do
        mObj <- Gio.listModelGetItem monitors idx
        case mObj of
          Nothing  -> findContainingMonitor monitors total (idx + 1)
          Just obj -> do
            mMon <- Gdk.castTo Gdk.Monitor obj
            case mMon of
              Nothing  -> findContainingMonitor monitors total (idx + 1)
              Just mon -> do
                rect <- Gdk.monitorGetGeometry mon
                mx <- Gdk.getRectangleX rect
                my <- Gdk.getRectangleY rect
                mw <- Gdk.getRectangleWidth rect
                mh <- Gdk.getRectangleHeight rect
                -- Check if point is inside this monitor
                if px >= mx && px < mx + mw && py >= my && py < my + mh
                  then return ( mx, my )
                  else findContainingMonitor monitors total (idx + 1)

-- | Set position via X11 (internal).
setPositionX11 :: Gtk.Window -> Int32 -> Int32 -> IO Bool
setPositionX11 gtkWindow x y = do
  result <- runMaybeT $ do
    surface <- MaybeT $ Gtk.nativeGetSurface gtkWindow
    x11Surface <- MaybeT $ Gdk.castTo GdkX11.X11Surface surface
    xid <- MaybeT $ Just <$> GdkX11.x11SurfaceGetXid x11Surface
    MaybeT $ Just <$> moveWindowRaw (fromIntegral xid) x y

  return $ result == Just True

-- | Move window via raw X11 (internal).
moveWindowRaw :: X11.Window -> Int32 -> Int32 -> IO Bool
moveWindowRaw win x y = do
  result <- try $ do
    dpy <- openDisplay ""
    X11.moveWindow dpy win x y
    flush dpy
    closeDisplay dpy

  case result of
    Left (_ :: SomeException) -> return False
    Right () -> return True

-- | Get the current position of a window.
--
-- On Wayland: Returns the layer-shell margins.
-- On X11: Returns (0, 0) as X11 doesn't have a simple way to query position.
getWindowPosition :: Gtk.Window -> IO ( Int32, Int32 )
getWindowPosition window = do
  backend <- detectBackend
  case backend of
    BackendWayland -> getPositionLayerShell window
    _ -> return ( 0, 0 )

-- | Get position from layer-shell margins (internal).
getPositionLayerShell :: Gtk.Window -> IO ( Int32, Int32 )
getPositionLayerShell window = do
  x <- LayerShell.getMargin window EdgeLeft
  y <- LayerShell.getMargin window EdgeTop
  return ( x, y )

--------------------------------------------------------------------------------
-- Layer-Shell Specific
--------------------------------------------------------------------------------

-- | Set the layer for a layer-shell window.
--
-- Only has effect on Wayland with layer-shell support.
-- On X11, this is a no-op (returns silently).
--
-- Layers from bottom to top:
--   * 'LayerBackground' - Below everything, even wallpaper
--   * 'LayerBottom' - Below normal windows
--   * 'LayerTop' - Above normal windows (good for desktop mascots)
--   * 'LayerOverlay' - Above everything including fullscreen apps
setWindowLayer :: Gtk.Window -> Layer -> IO ()
setWindowLayer window layer = do
  backend <- detectBackend
  case backend of
    BackendWayland -> LayerShell.setLayer window layer
    _ -> return ()
