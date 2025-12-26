{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Platform-specific window operations for Wayland and X11.
--
-- This module provides a unified interface for window management operations
-- that differ between Wayland (using layer-shell) and X11.
--
-- == Compile-time Configuration
--
-- The module's behavior is controlled by the LAYER_SHELL CPP flag:
--
-- * With LAYER_SHELL: Wayland layer-shell support is enabled. On Wayland,
--   windows use zwlr_layer_shell_v1 protocol for always-on-top and positioning.
--   Falls back to X11 if not running on Wayland.
--
-- * Without LAYER_SHELL: Only X11 support. Layer-shell functions are stubs.
--
-- To enable layer-shell support, build with:
--
-- @
-- stack build --flag kokage:layer-shell
-- @
--
-- Or add to stack.yaml:
--
-- @
-- flags:
--   kokage:
--     layer-shell: true
-- @
--
-- == Usage
--
-- The module automatically detects the platform and uses the appropriate
-- backend. Callers don't need to know which platform they're on:
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

import           Control.Exception          ( try, SomeException )
import           Control.Monad.Trans.Maybe  ( MaybeT(..), runMaybeT )
import           Data.Bits                  ( (.|.) )
import           Data.Int                   ( Int32 )
import           Foreign.C.Types            ( CInt )

import qualified GI.Gdk                     as Gdk
import qualified GI.Gtk                     as Gtk

#ifdef LAYER_SHELL
import qualified GI.Gtk4LayerShell          as LayerShell
import           GI.Gtk4LayerShell          ( Layer(..), Edge(..) )
#endif

-- X11 imports (always available)
import qualified GI.GdkX11                  as GdkX11
import           Graphics.X11.Xlib          ( openDisplay, closeDisplay
                                            , defaultScreen, rootWindow
                                            , internAtom, flush, allocaXEvent
                                            , sendEvent, substructureNotifyMask
                                            , substructureRedirectMask
                                            , clientMessage )
import qualified Graphics.X11.Xlib          as X11
import           Graphics.X11.Xlib.Extras   ( setEventType, setClientMessageEvent' )

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | The detected display backend.
data Backend
  = BackendWayland    -- ^ Wayland with layer-shell support
  | BackendX11        -- ^ X11/Xorg
  | BackendUnknown    -- ^ Unknown or unsupported backend
  deriving ( Show, Eq )

#ifndef LAYER_SHELL
-- | Layer type for layer-shell windows.
-- This is a stub when layer-shell support is not compiled in.
data Layer
  = LayerBackground  -- ^ Below everything, even wallpaper
  | LayerBottom      -- ^ Below normal windows
  | LayerTop         -- ^ Above normal windows (good for desktop mascots)
  | LayerOverlay     -- ^ Above everything including fullscreen apps
  deriving ( Show, Eq, Ord, Enum, Bounded )

-- | Edge type for layer-shell anchoring.
-- This is a stub when layer-shell support is not compiled in.
data Edge
  = EdgeLeft
  | EdgeRight
  | EdgeTop
  | EdgeBottom
  deriving ( Show, Eq, Ord, Enum, Bounded )
#endif

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
      -- Check if X11 is available
      x11Available <- checkX11Available
      return $ if x11Available then BackendX11 else BackendUnknown

-- | Check if layer-shell is supported on the current platform.
-- Returns True only if:
--   1. Compiled with LAYER_SHELL flag
--   2. Running on Wayland (not X11)
--   3. The compositor supports the zwlr_layer_shell_v1 protocol
isLayerShellSupported :: IO Bool
#ifdef LAYER_SHELL
isLayerShellSupported = do
  result <- try LayerShell.isSupported
  case result of
    Left (_ :: SomeException) -> return False
    Right supported           -> return supported
#else
isLayerShellSupported = return False
#endif

-- | Check if X11 is available (internal helper).
checkX11Available :: IO Bool
checkX11Available = do
  result <- try $ do
    dpy <- openDisplay ""
    closeDisplay dpy
  case result of
    Left (_ :: SomeException) -> return False
    Right ()                  -> return True

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
    BackendX11     -> return True  -- No init needed for X11
    BackendUnknown -> return False

-- | Initialize a window as a layer-shell surface (internal).
initLayerShellWindow :: Gtk.Window -> IO Bool
#ifdef LAYER_SHELL
initLayerShellWindow window = do
  supported <- isLayerShellSupported
  if supported
    then do
      result <- try $ do
        LayerShell.initForWindow window
        -- Anchor to top-left corner for positioning via margins
        LayerShell.setAnchor window EdgeLeft True
        LayerShell.setAnchor window EdgeTop True
        LayerShell.setAnchor window EdgeRight False
        LayerShell.setAnchor window EdgeBottom False
        -- No exclusive zone (don't push other windows)
        LayerShell.setExclusiveZone window 0
      case result of
        Left (_ :: SomeException) -> return False
        Right ()                  -> return True
    else return False
#else
initLayerShellWindow _ = return False
#endif

-- | Check if a window has been initialized for platform-specific features.
isPlatformInitialized :: Gtk.Window -> IO Bool
isPlatformInitialized window = do
  backend <- detectBackend
  case backend of
    BackendWayland -> isLayerShellWindow window
    BackendX11     -> return True  -- Always "initialized" on X11
    BackendUnknown -> return False

-- | Check if a window is a layer-shell window (internal).
isLayerShellWindow :: Gtk.Window -> IO Bool
#ifdef LAYER_SHELL
isLayerShellWindow = LayerShell.isLayerWindow
#else
isLayerShellWindow _ = return False
#endif

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
    BackendX11 -> setWindowAboveX11 window setAbove
    BackendUnknown -> return False

-- | Set always-on-top via X11 (internal).
setWindowAboveX11 :: Gtk.Window -> Bool -> IO Bool
setWindowAboveX11 gtkWindow setAbove = do
  result <- runMaybeT $ do
    -- Get the GDK surface from the GTK window
    surface <- MaybeT $ Gtk.nativeGetSurface gtkWindow
    
    -- Try to cast to X11Surface (will fail on Wayland)
    x11Surface <- MaybeT $ Gdk.castTo GdkX11.X11Surface surface
    
    -- Get the X11 window ID (XID)
    xid <- MaybeT $ Just <$> GdkX11.x11SurfaceGetXid x11Surface
    
    -- Set the window above state
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
        action = if setAbove then 1 else 0
    
    allocaXEvent $ \ev -> do
      setEventType ev clientMessage
      setClientMessageEvent' ev win atomNetWmState 32
        [ action
        , fromIntegral atomNetWmStateAbove
        , 0, 1, 0
        ]
      sendEvent dpy root False 
        (substructureNotifyMask .|. substructureRedirectMask) ev
    
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
setPositionLayerShell :: Gtk.Window -> Int32 -> Int32 -> IO ()
#ifdef LAYER_SHELL
setPositionLayerShell window x y = do
  LayerShell.setMargin window EdgeLeft x
  LayerShell.setMargin window EdgeTop y
#else
setPositionLayerShell _ _ _ = return ()
#endif

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
    X11.moveWindow dpy win (fromIntegral x) (fromIntegral y)
    flush dpy
    closeDisplay dpy
    
  case result of
    Left (_ :: SomeException) -> return False
    Right () -> return True

-- | Get the current position of a window.
--
-- On Wayland: Returns the layer-shell margins.
-- On X11: Returns (0, 0) as X11 doesn't have a simple way to query position.
getWindowPosition :: Gtk.Window -> IO (Int32, Int32)
getWindowPosition window = do
  backend <- detectBackend
  case backend of
    BackendWayland -> getPositionLayerShell window
    _              -> return (0, 0)

-- | Get position from layer-shell margins (internal).
getPositionLayerShell :: Gtk.Window -> IO (Int32, Int32)
#ifdef LAYER_SHELL
getPositionLayerShell window = do
  x <- LayerShell.getMargin window EdgeLeft
  y <- LayerShell.getMargin window EdgeTop
  return (x, y)
#else
getPositionLayerShell _ = return (0, 0)
#endif

--------------------------------------------------------------------------------
-- Layer-Shell Specific
--------------------------------------------------------------------------------

-- | Set the layer for a layer-shell window.
--
-- Only has effect on Wayland with layer-shell support.
-- On X11 or without layer-shell, this is a no-op.
--
-- Layers from bottom to top:
--   * 'LayerBackground' - Below everything, even wallpaper
--   * 'LayerBottom' - Below normal windows
--   * 'LayerTop' - Above normal windows (good for desktop mascots)
--   * 'LayerOverlay' - Above everything including fullscreen apps
setWindowLayer :: Gtk.Window -> Layer -> IO ()
#ifdef LAYER_SHELL
setWindowLayer = LayerShell.setLayer
#else
setWindowLayer _ _ = return ()
#endif
