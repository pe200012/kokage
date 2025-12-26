{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | X11-specific window operations.
-- This module provides functions for X11 window hints that are not
-- available in GTK4's cross-platform API.
module Kokage.X11
  ( -- * Always On Top
    setWindowAbove
  , setWindowAboveFromGtk
    -- * Window Positioning
  , moveWindow
  , moveWindowFromGtk
    -- * Re-exports
  , X11.Window
  ) where

import           Control.Exception          ( try, SomeException )
import           Control.Monad.Trans.Maybe  ( MaybeT(..), runMaybeT )
import           Data.Bits                  ( (.|.) )
import           Foreign.C.Types            ( CInt )

import qualified GI.Gdk                     as Gdk
import qualified GI.GdkX11                  as GdkX11
import qualified GI.Gtk                     as Gtk

import           Graphics.X11.Xlib          ( openDisplay, closeDisplay
                                            , defaultScreen, rootWindow
                                            , internAtom, flush, allocaXEvent
                                            , sendEvent, substructureNotifyMask
                                            , substructureRedirectMask
                                            , clientMessage )
import qualified Graphics.X11.Xlib          as X11
import           Graphics.X11.Xlib.Extras   ( setEventType, setClientMessageEvent' )

import           Data.Int                   ( Int32 )

-- | Set a window to be always on top (or remove the hint).
-- This sends a _NET_WM_STATE client message to the root window
-- as per the EWMH specification.
--
-- Returns True if successful, False if the X11 operation failed.
setWindowAbove :: X11.Window  -- ^ The X11 window ID
               -> Bool        -- ^ True to set above, False to remove
               -> IO Bool
setWindowAbove win setAbove = do
  result <- try $ do
    -- Open connection to X server
    dpy <- openDisplay ""
    
    -- Get atoms for _NET_WM_STATE and _NET_WM_STATE_ABOVE
    atomNetWmState <- internAtom dpy "_NET_WM_STATE" False
    atomNetWmStateAbove <- internAtom dpy "_NET_WM_STATE_ABOVE" False
    
    -- Get root window
    let screen = defaultScreen dpy
    root <- rootWindow dpy screen
    
    -- Action: 1 = _NET_WM_STATE_ADD, 0 = _NET_WM_STATE_REMOVE
    let action :: CInt
        action = if setAbove then 1 else 0
    
    -- Send ClientMessage event to root window
    allocaXEvent $ \ev -> do
      setEventType ev clientMessage
      -- Format 32 means data is 32-bit values
      -- Data: [action, property1, property2, source_indication, unused]
      setClientMessageEvent' ev win atomNetWmState 32
        [ action
        , fromIntegral atomNetWmStateAbove
        , 0  -- second property (unused for single state change)
        , 1  -- source indication: 1 = normal application
        , 0  -- unused
        ]
      sendEvent dpy root False 
        (substructureNotifyMask .|. substructureRedirectMask) ev
    
    flush dpy
    closeDisplay dpy
    
  case result of
    Left (_ :: SomeException) -> return False
    Right () -> return True

-- | Set a GTK window to be always on top using X11.
-- This extracts the X11 window ID from a GTK window and calls setWindowAbove.
-- Only works on X11; silently does nothing on Wayland.
--
-- Returns True if successful, False if not on X11 or operation failed.
setWindowAboveFromGtk :: Gtk.Window -> Bool -> IO Bool
setWindowAboveFromGtk gtkWindow setAbove = do
  result <- runMaybeT $ do
    -- Get the GDK surface from the GTK window
    surface <- MaybeT $ Gtk.nativeGetSurface gtkWindow
    
    -- Try to cast to X11Surface (will fail on Wayland)
    x11Surface <- MaybeT $ Gdk.castTo GdkX11.X11Surface surface
    
    -- Get the X11 window ID (XID)
    xid <- MaybeT $ Just <$> GdkX11.x11SurfaceGetXid x11Surface
    
    -- Set the window above state
    MaybeT $ Just <$> setWindowAbove (fromIntegral xid) setAbove
    
  return $ result == Just True

-- | Move a window to a specific position on X11.
-- This uses XMoveWindow to reposition the window.
--
-- Returns True if successful, False if the X11 operation failed.
moveWindow :: X11.Window  -- ^ The X11 window ID
           -> Int32       -- ^ X position
           -> Int32       -- ^ Y position
           -> IO Bool
moveWindow win x y = do
  result <- try $ do
    -- Open connection to X server
    dpy <- openDisplay ""
    
    -- Move the window
    X11.moveWindow dpy win (fromIntegral x) (fromIntegral y)
    
    flush dpy
    closeDisplay dpy
    
  case result of
    Left (_ :: SomeException) -> return False
    Right () -> return True

-- | Move a GTK window to a specific position using X11.
-- This extracts the X11 window ID from a GTK window and calls moveWindow.
-- Only works on X11; silently does nothing on Wayland.
--
-- Returns True if successful, False if not on X11 or operation failed.
moveWindowFromGtk :: Gtk.Window -> Int32 -> Int32 -> IO Bool
moveWindowFromGtk gtkWindow x y = do
  result <- runMaybeT $ do
    -- Get the GDK surface from the GTK window
    surface <- MaybeT $ Gtk.nativeGetSurface gtkWindow
    
    -- Try to cast to X11Surface (will fail on Wayland)
    x11Surface <- MaybeT $ Gdk.castTo GdkX11.X11Surface surface
    
    -- Get the X11 window ID (XID)
    xid <- MaybeT $ Just <$> GdkX11.x11SurfaceGetXid x11Surface
    
    -- Move the window
    MaybeT $ Just <$> moveWindow (fromIntegral xid) x y
    
  return $ result == Just True
