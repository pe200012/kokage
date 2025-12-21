{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wayland layer-shell support for always-on-top windows.
-- This module provides functions to use the zwlr_layer_shell_v1 protocol
-- on Wayland compositors (wlroots-based like Sway/Hyprland, and KDE Plasma).
--
-- IMPORTANT: Layer shell initialization must happen BEFORE the window is realized.
-- Call 'initLayerShell' right after creating the window, before showing it.
--
-- == Positioning Layer Shell Windows
--
-- Layer shell windows don't support the standard toplevel move operation.
-- Instead, they are positioned using anchors and margins:
--
-- * For a floating window (like a desktop mascot), anchor to top-left corner
--   and use margins to position the window.
-- * Call 'setLayerShellPosition' to set the window position via margins.
--
-- This module is conditionally compiled:
-- - With LAYER_SHELL defined: Full layer-shell support
-- - Without LAYER_SHELL: Stub implementations that always return False/do nothing
--
-- To enable layer-shell support, build with:
--   stack build --flag kokage:layer-shell
-- or add to stack.yaml:
--   flags:
--     kokage:
--       layer-shell: true
--
-- Requires the gtk4-layer-shell C library to be installed on the system.
module Kokage.LayerShell
  ( -- * Layer Shell Support
    isLayerShellSupported
  , initLayerShell
  , isLayerShellWindow
  , setWindowLayer
    -- * Positioning
  , setLayerShellPosition
  , getLayerShellPosition
    -- * Layer Types
  , Layer(..)
  , Edge(..)
  ) where

#ifdef LAYER_SHELL
import           Control.Exception          ( try, SomeException )
import           Data.Int                   ( Int32 )
import qualified GI.Gtk                     as Gtk
import qualified GI.Gtk4LayerShell          as LayerShell
import           GI.Gtk4LayerShell          ( Layer(..), Edge(..) )
#else
import           Data.Int                   ( Int32 )
import qualified GI.Gtk                     as Gtk
#endif

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

-- | Check if layer-shell is supported on the current platform.
-- This returns True only if:
--   1. Compiled with LAYER_SHELL flag
--   2. Running on Wayland (not X11)
--   3. The compositor supports the zwlr_layer_shell_v1 protocol
--
-- May block for a Wayland roundtrip the first time it's called.
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

-- | Initialize a GTK window as a layer-shell surface.
-- This MUST be called BEFORE the window is realized (shown).
--
-- After calling this, the window will be anchored to the top-left corner
-- by default, allowing positioning via 'setLayerShellPosition'.
--
-- Returns True if successful, False if layer-shell is not supported
-- or not compiled in.
--
-- Usage:
-- @
-- window <- new Gtk.Window [...]
-- success <- initLayerShell window
-- when success $ do
--   setWindowLayer window LayerTop
--   setLayerShellPosition window 100 100  -- Position at (100, 100)
-- Gtk.windowPresent window  -- Now show the window
-- @
initLayerShell :: Gtk.Window -> IO Bool
#ifdef LAYER_SHELL
initLayerShell window = do
  supported <- isLayerShellSupported
  if supported
    then do
      result <- try $ do
        LayerShell.initForWindow window
        -- Anchor to top-left corner for positioning via margins
        -- This allows us to set position using left/top margins
        LayerShell.setAnchor window EdgeLeft True
        LayerShell.setAnchor window EdgeTop True
        -- Don't anchor to right/bottom so window floats freely
        LayerShell.setAnchor window EdgeRight False
        LayerShell.setAnchor window EdgeBottom False
        -- No exclusive zone (don't push other windows)
        LayerShell.setExclusiveZone window 0
      case result of
        Left (_ :: SomeException) -> return False
        Right ()                  -> return True
    else return False
#else
initLayerShell _ = return False
#endif

-- | Check if a window is a layer-shell window.
isLayerShellWindow :: Gtk.Window -> IO Bool
#ifdef LAYER_SHELL
isLayerShellWindow = LayerShell.isLayerWindow
#else
isLayerShellWindow _ = return False
#endif

-- | Set the layer for a layer-shell window.
-- The window must have been initialized with 'initLayerShell' first.
--
-- Layers from bottom to top:
--   * 'LayerBackground' - Below everything, even wallpaper
--   * 'LayerBottom' - Below normal windows
--   * 'LayerTop' - Above normal windows (good for desktop mascots)
--   * 'LayerOverlay' - Above everything including fullscreen apps
--
-- For a desktop mascot (Ukagaka ghost), 'LayerTop' is appropriate.
-- 'LayerOverlay' would make the ghost appear over fullscreen games/videos.
setWindowLayer :: Gtk.Window -> Layer -> IO ()
#ifdef LAYER_SHELL
setWindowLayer = LayerShell.setLayer
#else
setWindowLayer _ _ = return ()
#endif

-- | Set the position of a layer-shell window.
-- This sets the left and top margins to position the window.
-- The window must have been initialized with 'initLayerShell' first,
-- which anchors it to the top-left corner.
--
-- @x@ and @y@ are the pixel coordinates from the top-left of the screen.
setLayerShellPosition :: Gtk.Window -> Int32 -> Int32 -> IO ()
#ifdef LAYER_SHELL
setLayerShellPosition window x y = do
  LayerShell.setMargin window EdgeLeft x
  LayerShell.setMargin window EdgeTop y
#else
setLayerShellPosition _ _ _ = return ()
#endif

-- | Get the current position of a layer-shell window.
-- Returns the left and top margins as (x, y) coordinates.
getLayerShellPosition :: Gtk.Window -> IO (Int32, Int32)
#ifdef LAYER_SHELL
getLayerShellPosition window = do
  x <- LayerShell.getMargin window EdgeLeft
  y <- LayerShell.getMargin window EdgeTop
  return (x, y)
#else
getLayerShellPosition _ = return (0, 0)
#endif
