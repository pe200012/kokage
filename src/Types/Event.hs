{-# LANGUAGE StrictData #-}

-- | Event types for Kokage FRP network.
module Types.Event
  ( -- * Click events
    ClickEvent(..)
  , CollisionHit(..)
    -- * Drag events
  , DragEvent(..)
  , DragPhase(..)
    -- * Timer events
  , TimerEvent(..)
    -- * Constants
  , dragThreshold
    -- * Utilities
  , isDragSignificant
  ) where

import           Data.Time.LocalTime ( LocalTime )

import           Types.Ghost         ( CollisionRegion )

-- | Minimum distance (in pixels) to consider a drag vs a click.
-- Movements below this threshold are treated as clicks.
dragThreshold :: Double
dragThreshold = 5.0

-- | A click event with coordinates.
data ClickEvent = ClickEvent { clickX :: !Int, clickY :: !Int }
  deriving ( Show, Eq )

-- | Result of a collision hit test.
data CollisionHit
  = HitRegion !ClickEvent !CollisionRegion  -- ^ Hit a named region
  | HitNothing !ClickEvent                  -- ^ Clicked but hit nothing
  deriving ( Show, Eq )

-- | Phase of a drag operation.
data DragPhase
  = DragStart   -- ^ Drag just started
  | DragMove    -- ^ Drag is in progress
  | DragEnd     -- ^ Drag ended
  deriving ( Show, Eq )

-- | A drag event with start position and current offset.
data DragEvent
  = DragEvent { dragPhase   :: !DragPhase  -- ^ Current phase of the drag
              , dragStartX  :: !Double     -- ^ X coordinate where drag started
              , dragStartY  :: !Double     -- ^ Y coordinate where drag started
              , dragOffsetX :: !Double     -- ^ X offset from start (0 for DragStart)
              , dragOffsetY :: !Double     -- ^ Y offset from start (0 for DragStart)
              }
  deriving ( Show, Eq )

-- | A timer event with the current local time.
newtype TimerEvent
  = TimerEvent { teTime :: LocalTime  -- ^ Current local time when timer fired
               }
  deriving ( Show, Eq )

-- | Check if a drag offset exceeds the threshold.
isDragSignificant :: Double -> Double -> Bool
isDragSignificant ox oy = sqrt (ox * ox + oy * oy) >= dragThreshold
