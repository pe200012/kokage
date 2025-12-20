{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | FRP event network and event types for Kokage.
-- Handles GTK event to FRP event conversion and network setup.
module Kokage.Event
  ( -- * Event Types
    ClickEvent(..)
  , CollisionHit(..)
  , DragEvent(..)
  , DragPhase(..)
    -- * Network Configuration
  , NetworkConfig(..)
  , InputHandlers(..)
    -- * Network Setup
  , setupNetwork
    -- * Event Handlers
  , handleClick
    -- * Configuration
  , dragThreshold
  ) where

import qualified Data.Text                  as T

import qualified GI.Gtk                     as Gtk

import           Kokage.Collision           ( findCollisionAt )

import           Reactive.Banana            ( (<@), (<@>), Behavior, filterE, stepper )
import           Reactive.Banana.Frameworks ( AddHandler, MomentIO, fromAddHandler, reactimate )
import           Reactive.Banana.GI.Gtk     ( signalE0R )

import           Types.Ghost                ( CollisionRegion(..) )

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

-- | Input event handlers from GTK gestures.
-- We use only GestureDrag for both click and drag detection.
-- A click is detected as a drag that ends without exceeding the threshold.
data InputHandlers
  = InputHandlers
  { ihDragBegin  :: AddHandler ( Double, Double )  -- ^ Drag started at (x, y)
  , ihDragUpdate :: AddHandler ( Double, Double )  -- ^ Drag offset (dx, dy)
  , ihDragEnd    :: AddHandler ( Double, Double )  -- ^ Drag ended with offset (dx, dy)
  }

-- | Configuration for the FRP network.
-- Extensible record for all network inputs.
data NetworkConfig
  = NetworkConfig { ncWindow     :: !Gtk.Window           -- ^ The main window
                  , ncInputs     :: !InputHandlers        -- ^ Input event handlers
                  , ncCollisions :: ![ CollisionRegion ]  -- ^ Collision regions for hit testing
                  , ncBeginMove  :: Double -> Double -> IO ()  -- ^ Action to begin window move
                  }

-- | Check if a drag offset exceeds the threshold.
isDragSignificant :: Double -> Double -> Bool
isDragSignificant ox oy
  = let
      dist = sqrt (ox * ox + oy * oy)
    in 
      dist >= dragThreshold

-- | Process a click event against collision regions.
handleClick :: [ CollisionRegion ] -> ( Double, Double ) -> CollisionHit
handleClick collisions ( x, y )
  = let
      ix  = floor x :: Int
      iy  = floor y :: Int
      evt = ClickEvent ix iy
    in 
      case findCollisionAt ix iy collisions of
        Just cr -> HitRegion evt cr
        Nothing -> HitNothing evt

-- | Log a collision hit for debugging.
logCollisionHit :: CollisionHit -> IO ()
logCollisionHit (HitRegion evt cr)
  = putStrLn
  $ "[Click] Hit collision region '"
  <> T.unpack (crName cr)
  <> "' (index "
  <> show (crIndex cr)
  <> ") at ("
  <> show (clickX evt)
  <> ", "
  <> show (clickY evt)
  <> ")"
logCollisionHit (HitNothing evt)
  = putStrLn $ "[Click] No collision at (" <> show (clickX evt) <> ", " <> show (clickY evt) <> ")"

-- | Log a drag event for debugging.
logDragEvent :: DragEvent -> IO ()
logDragEvent evt
  = putStrLn
  $ "[Drag] "
  <> show (dragPhase evt)
  <> " at ("
  <> show (round (dragStartX evt) :: Int)
  <> ", "
  <> show (round (dragStartY evt) :: Int)
  <> ")"
  <> " offset ("
  <> show (round (dragOffsetX evt) :: Int)
  <> ", "
  <> show (round (dragOffsetY evt) :: Int)
  <> ")"

-- | Set up the FRP network for the window.
-- Handles window close, click events (via drag), drag events, and window movement.
-- Click is detected as a drag that ends without exceeding the threshold.
setupNetwork :: NetworkConfig -> MomentIO ()
setupNetwork config = do
  let window     = ncWindow config
      inputs     = ncInputs config
      collisions = ncCollisions config
      beginMove  = ncBeginMove config

  -- Create close event - closeRequest returns Bool, we return False to allow close
  closeE <- signalE0R window #closeRequest False
  reactimate $ (return () :: IO ()) <$ closeE

  -- Get input events from drag gesture
  dragBeginE <- fromAddHandler (ihDragBegin inputs)
  dragUpdateE <- fromAddHandler (ihDragUpdate inputs)
  dragEndE <- fromAddHandler (ihDragEnd inputs)

  -- Track drag start position using Behavior
  -- Updated on each dragBegin, used to compute click position
  dragStartB :: Behavior ( Double, Double ) <- stepper ( 0, 0 ) dragBeginE

  -- Helper to check if offset exceeds threshold
  let exceedsThreshold ( ox, oy ) = isDragSignificant ox oy

  -- Detect click vs drag based on dragEnd offset
  -- dragEnd contains the final offset (ox, oy) from start position
  -- If offset < threshold, it's a click; otherwise it's a drag
  let dragEndWithStart = (,) <$> dragStartB <@> dragEndE
      -- (startPos, endOffset)

      -- Click: offset doesn't exceed threshold, use start position
      clickE           = fst <$> filterE (not . exceedsThreshold . snd) dragEndWithStart

      -- Suppressed (was a drag): offset exceeds threshold
      suppressedE      = filterE (exceedsThreshold . snd) dragEndWithStart

  -- Process clicks against collision regions
  let hitE = handleClick collisions <$> clickE
  reactimate $ logCollisionHit <$> hitE

  -- Log suppressed clicks
  reactimate $ (putStrLn "[Click] Suppressed (drag exceeded threshold)") <$ suppressedE

  -- Create DragEvents for logging
  let mkDragStart ( x, y ) = DragEvent DragStart x y 0 0
      mkDragMove ( ox, oy ) = DragEvent DragMove 0 0 ox oy
      mkDragEnd ( ox, oy ) = DragEvent DragEnd 0 0 ox oy

  let dragStartE = mkDragStart <$> dragBeginE
      dragMoveE  = mkDragMove <$> filterE exceedsThreshold dragUpdateE
      dragEndE'  = mkDragEnd <$> filterE exceedsThreshold dragEndE

  -- Log drag events
  reactimate $ logDragEvent <$> dragStartE
  reactimate $ logDragEvent <$> dragMoveE
  reactimate $ logDragEvent <$> dragEndE'

  -- Initiate window move only when drag first exceeds threshold
  -- We pass the start position (from behavior) to beginMove
  let firstExceedE = filterE exceedsThreshold dragUpdateE
      moveE        = dragStartB <@ firstExceedE
  reactimate $ uncurry beginMove <$> moveE
