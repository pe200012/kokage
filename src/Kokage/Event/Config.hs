{-# LANGUAGE StrictData #-}

-- | Configuration types for Kokage FRP network.
module Kokage.Event.Config
  ( -- * Input handlers
    InputHandlers(..)
  , TimerHandlers(..)
    -- * Move modes
  , MoveMode(..)
  , BalloonMoveMode(..)
    -- * SHIORI configuration
  , ShioriConfig(..)
  , ScriptHandler
    -- * Network configurations
  , NetworkConfig(..)
  , CharacterNetworkConfig(..)
  , GlobalNetworkConfig(..)
  ) where

import qualified Data.Text                  as T
import           Data.Time.Clock            ( UTCTime )
import           Data.Time.LocalTime        ( LocalTime )

import qualified GI.Gtk                     as Gtk

import           Kokage.Shiori.WineBridge   ( WineShiori )

import           Prelude                    ()

import           Reactive.Banana.Frameworks ( AddHandler )

import           Relude

import           Types.Ghost                ( CollisionRegion )

-- | Input event handlers from GTK gestures.
-- We use only GestureDrag for both click and drag detection.
-- A click is detected as a drag that ends without exceeding the threshold.
data InputHandlers
  = InputHandlers
  { ihDragBegin  :: AddHandler ( Double, Double )  -- ^ Drag started at (x, y)
  , ihDragUpdate :: AddHandler ( Double, Double )  -- ^ Drag offset (dx, dy)
  , ihDragEnd    :: AddHandler ( Double, Double )  -- ^ Drag ended with offset (dx, dy)
  , ihMotion     :: AddHandler ( Double, Double )  -- ^ Mouse motion at (x, y)
  , ihRightClick :: AddHandler ( Double, Double )  -- ^ Right-click at (x, y)
  , ihLeftClick  :: AddHandler ( Int, Double, Double )  -- ^ Left-click with (n_press, x, y)
  }

-- | Timer event handlers.
-- These are fired by GLib timeout sources.
data TimerHandlers
  = TimerHandlers
  { thSecondTick :: AddHandler LocalTime  -- ^ Fires every second with current time
  , thMinuteTick :: AddHandler LocalTime  -- ^ Fires every minute with current time
  , thHourTick   :: AddHandler LocalTime  -- ^ Fires every hour with current time
  , thMotionTick :: AddHandler ()         -- ^ Fires every 100ms for mouse motion sampling
  }

-- | Window move mode.
-- Different platforms require different approaches to window movement.
data MoveMode
  = MoveToplevel (Double -> Double -> IO ())
    -- ^ Standard toplevel move: call once when drag starts, compositor handles the rest.
    -- Used on X11 and regular Wayland windows.
    -- The function is called with the pointer position (x, y) when drag starts.
  | MoveLayerShell (Double -> Double -> IO ())
-- ^ Layer-shell margin-based move: update position on every drag update.
-- Used for Wayland layer-shell surfaces which don't support toplevel moves.
-- The function is called with the offset (dx, dy) on each drag update.

-- | Balloon window move mode.
-- Uses absolute positioning (basePos + cumulativeOffset) for layer-shell.
data BalloonMoveMode
  = BalloonMoveToplevel (Double -> Double -> IO ())
    -- ^ Standard toplevel move (X11): call once when drag starts.
  | BalloonMoveLayerShell !(Double -> Double -> IO ())      -- ^ Function to set layer-shell position

-- | SHIORI configuration for the FRP network.
-- This is optional - ghosts can run without SHIORI.
data ShioriConfig
  = ShioriConfig { scShiori    :: !WineShiori      -- ^ Wine bridge handle
                 , scSurfaceId :: !Int             -- ^ Current surface ID (for mouse events)
                 , scStartTime :: !UTCTime         -- ^ When the ghost was started (for uptime)
                 , scGhostPath :: !FilePath        -- ^ Path to ghost directory (for HISTORY)
                 }

-- | Handler for executing SHIORI response scripts.
type ScriptHandler = Maybe T.Text -> IO ()

-- | Legacy configuration for single-window FRP network.
-- Kept for backwards compatibility during transition.
data NetworkConfig
  = NetworkConfig
  { ncWindow     :: !Gtk.Window                -- ^ The main window
  , ncInputs     :: !InputHandlers             -- ^ Input event handlers
  , ncTimers     :: !TimerHandlers             -- ^ Timer event handlers
  , ncCollisions :: ![ CollisionRegion ]       -- ^ Collision regions for hit testing
  , ncMoveMode   :: !MoveMode                  -- ^ How to handle window movement
  , ncShiori     :: !(Maybe ShioriConfig)      -- ^ Optional SHIORI config
  }

-- | Configuration for a single character's FRP network.
-- Each character window has its own FRP network for input handling.
-- Since each character has exactly one balloon, the balloon config is integrated here.
data CharacterNetworkConfig
  = CharacterNetworkConfig
  { cncWindow :: !Gtk.Window                -- ^ The character's surface window
  , cncInputs :: !InputHandlers             -- ^ Input event handlers for this window
  , cncCollisions :: ![ CollisionRegion ]       -- ^ Collision regions for hit testing
  , cncMoveMode :: !MoveMode                  -- ^ How to handle window movement
  , cncScopeId :: !Int                       -- ^ Character scope ID (0=sakura, 1=kero, etc.)
  , cncShiori :: !(Maybe ShioriConfig)      -- ^ Optional SHIORI config (shared)
  , cncScriptHandler :: !ScriptHandler          -- ^ Handler for SHIORI scripts
  , cncContextMenu :: !Gtk.PopoverMenu          -- ^ Context menu for right-click
  , cncMotionTick :: !(AddHandler ())           -- ^ Motion tick for throttled mouse events
    -- Balloon integration (one balloon per character)
  , cncBalloonWindow :: !Gtk.Window           -- ^ The balloon window
  , cncBalloonInputs :: !InputHandlers        -- ^ Input event handlers for balloon
  , cncBalloonMoveMode :: !BalloonMoveMode      -- ^ How to handle balloon window movement
    -- Time-critical mode (blocks mouse events during \t sections)
  , cncTimeCriticalHandler :: !(AddHandler Bool)  -- ^ Handler for time-critical state changes
  }

-- | Configuration for the global FRP network (timers).
-- This is shared across all character windows.
data GlobalNetworkConfig
  = GlobalNetworkConfig
  { gncTimers        :: !TimerHandlers             -- ^ Timer event handlers (shared)
  , gncShiori        :: !(Maybe ShioriConfig)      -- ^ Optional SHIORI config (shared)
  , gncScriptHandler :: !ScriptHandler       -- ^ Handler for SHIORI scripts
  }
