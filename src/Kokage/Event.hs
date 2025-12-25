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
  , TimerEvent(..)
    -- * Network Configuration
  , NetworkConfig(..)
  , ShioriConfig(..)
  , InputHandlers(..)
  , TimerHandlers(..)
  , MoveMode(..)
    -- * Network Setup
  , setupNetwork
    -- * Event Handlers
  , handleClick
    -- * SHIORI Helpers
  , sendShioriAndLog
  , sendShioriWithCallback
    -- * Configuration
  , dragThreshold
  ) where

import           Data.Time.Clock             ( UTCTime, diffUTCTime, getCurrentTime )
import           Data.Time.LocalTime        ( LocalTime(..), TimeOfDay(..) )
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T

import qualified GI.Gtk                     as Gtk

import           Kokage.Collision           ( findCollisionAt )
import           Kokage.Shiori.WineBridge   ( WineShiori, sendEvent )

import           Reactive.Banana            ( (<@), (<@>), Behavior, filterE, stepper )
import           Reactive.Banana.Frameworks ( AddHandler, MomentIO, fromAddHandler, reactimate )
import           Reactive.Banana.GI.Gtk     ( signalE0R )

import           Types.Ghost                ( CollisionRegion(..) )
import           Types.Shiori               ( ShioriEvent(..), ShioriResponse(..), srsValue )

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
data TimerEvent
  = TimerEvent { teTime :: !LocalTime  -- ^ Current local time when timer fired
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
  , ihMotion     :: AddHandler ( Double, Double )  -- ^ Mouse motion at (x, y)
  }

-- | Timer event handlers.
-- These are fired by GLib timeout sources.
data TimerHandlers
  = TimerHandlers
  { thSecondTick :: AddHandler LocalTime  -- ^ Fires every second with current time
  , thMinuteTick :: AddHandler LocalTime  -- ^ Fires every minute with current time
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

-- | SHIORI configuration for the FRP network.
-- This is optional - ghosts can run without SHIORI.
data ShioriConfig
  = ShioriConfig
  { scShiori    :: !WineShiori      -- ^ Wine bridge handle
  , scSurfaceId :: !Int             -- ^ Current surface ID (for mouse events)
  , scStartTime :: !UTCTime         -- ^ When the ghost was started (for uptime)
  , scGhostPath :: !FilePath        -- ^ Path to ghost directory (for HISTORY)
  }

-- | Configuration for the FRP network.
-- Extensible record for all network inputs.
data NetworkConfig
  = NetworkConfig { ncWindow     :: !Gtk.Window                -- ^ The main window
                  , ncInputs     :: !InputHandlers             -- ^ Input event handlers
                  , ncTimers     :: !TimerHandlers             -- ^ Timer event handlers
                  , ncCollisions :: ![ CollisionRegion ]       -- ^ Collision regions for hit testing
                  , ncMoveMode   :: !MoveMode                  -- ^ How to handle window movement
                  , ncShiori     :: !(Maybe ShioriConfig)      -- ^ Optional SHIORI config
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

-- | Format a TimeOfDay as HH:MM:SS.
formatTime :: TimeOfDay -> String
formatTime tod
  = let
      h = todHour tod
      m = todMin tod
      s = floor (todSec tod) :: Int
    in
      pad h <> ":" <> pad m <> ":" <> pad s
  where
    pad n = if n < 10 then "0" <> show n else show n

-- | Calculate uptime in hours from start time to now.
getUptimeHours :: UTCTime -> UTCTime -> Int
getUptimeHours startTime now = 
  let diffSeconds = diffUTCTime now startTime
      hours = floor (realToFrac diffSeconds / 3600 :: Double) :: Int
  in max 0 hours  -- Ensure non-negative

-- | Log a second timer event.
logSecondTick :: LocalTime -> IO ()
logSecondTick lt
  = putStrLn $ "[Timer] Second tick: " <> formatTime (localTimeOfDay lt)

-- | Log a minute timer event.
logMinuteTick :: LocalTime -> IO ()
logMinuteTick lt
  = putStrLn $ "[Timer] Minute tick: " <> formatTime (localTimeOfDay lt)

-- | Log a SHIORI response for debugging.
logShioriResponse :: ShioriEvent -> Either String ShioriResponse -> IO ()
logShioriResponse event result = case result of
  Left err -> putStrLn $ "[SHIORI] " <> show event <> " error: " <> err
  Right resp -> case srsValue resp of
    Nothing -> putStrLn $ "[SHIORI] " <> show event <> " -> (no content)"
    Just val -> do
      putStrLn $ "[SHIORI] " <> show event <> " -> script:"
      -- Print first 200 chars of script for debugging
      let preview = T.take 200 val
      putStrLn $ "  " <> T.unpack preview <> if T.length val > 200 then "..." else ""

-- | Send a SHIORI event and log the response.
-- Does nothing if SHIORI is not configured.
sendShioriAndLog :: Maybe ShioriConfig -> ShioriEvent -> Map.Map Int T.Text -> IO ()
sendShioriAndLog Nothing _ _ = return ()  -- No SHIORI, skip
sendShioriAndLog (Just sc) event refs = do
  result <- sendEvent (scShiori sc) event refs
  logShioriResponse event result

-- | Send a SHIORI event with a callback for the response.
-- The callback receives Just the script text on success, Nothing on failure.
-- Does nothing if SHIORI is not configured.
sendShioriWithCallback :: Maybe ShioriConfig 
                       -> ShioriEvent 
                       -> Map.Map Int T.Text 
                       -> (Maybe T.Text -> IO ())  -- ^ Callback with response script
                       -> IO ()
sendShioriWithCallback Nothing _ _ _ = return ()  -- No SHIORI, skip
sendShioriWithCallback (Just sc) event refs callback = do
  result <- sendEvent (scShiori sc) event refs
  logShioriResponse event result
  case result of
    Left _ -> callback Nothing
    Right resp -> callback (srsValue resp)

-- | Set up the FRP network for the window.
-- Handles window close, click events (via drag), drag events, window movement,
-- timer events, and SHIORI event dispatch.
-- Click is detected as a drag that ends without exceeding the threshold.
setupNetwork :: NetworkConfig -> MomentIO ()
setupNetwork config = do
  let window     = ncWindow config
      inputs     = ncInputs config
      timers     = ncTimers config
      collisions = ncCollisions config
      moveMode   = ncMoveMode config
      mShiori    = ncShiori config

  -- Create close event - closeRequest returns Bool, we return False to allow close
  closeE <- signalE0R window #closeRequest False
  -- Send OnClose event when window closes
  reactimate $ sendShioriAndLog mShiori OnClose Map.empty <$ closeE

  -- Get input events from drag gesture
  dragBeginE <- fromAddHandler (ihDragBegin inputs)
  dragUpdateE <- fromAddHandler (ihDragUpdate inputs)
  dragEndE <- fromAddHandler (ihDragEnd inputs)
  motionE <- fromAddHandler (ihMotion inputs)

  -- Get timer events
  secondTickE <- fromAddHandler (thSecondTick timers)
  minuteTickE <- fromAddHandler (thMinuteTick timers)

  -- Handle timer events - send to SHIORI with uptime
  -- OnSecondChange: Reference0=uptime(hours), Reference1=mikire, Reference2=kasanari, Reference3=cantalk
  -- OnMinuteChange: Reference0=uptime(hours), Reference1=mikire, Reference2=kasanari, Reference3=cantalk
  let handleSecondTick lt = do
        logSecondTick lt
        now <- getCurrentTime
        let uptime = case mShiori of
              Just sc -> getUptimeHours (scStartTime sc) now
              Nothing -> 0
            -- TODO: Reference1-3 need actual overlap/cantalk tracking
            refs = Map.fromList
              [ (0, T.pack $ show uptime)  -- Reference0: uptime in hours
              , (1, "0")                   -- Reference1: mikire (overlap count) - TODO
              , (2, "0")                   -- Reference2: kasanari (overlap) - TODO  
              , (3, "1")                   -- Reference3: cantalk (1=can talk)
              ]
        sendShioriAndLog mShiori OnSecondChange refs

  let handleMinuteTick lt = do
        logMinuteTick lt
        now <- getCurrentTime
        let uptime = case mShiori of
              Just sc -> getUptimeHours (scStartTime sc) now
              Nothing -> 0
            refs = Map.fromList
              [ (0, T.pack $ show uptime)  -- Reference0: uptime in hours
              , (1, "0")                   -- Reference1: mikire - TODO
              , (2, "0")                   -- Reference2: kasanari - TODO
              , (3, "1")                   -- Reference3: cantalk
              ]
        sendShioriAndLog mShiori OnMinuteChange refs

  reactimate $ handleSecondTick <$> secondTickE
  reactimate $ handleMinuteTick <$> minuteTickE

  -- Handle mouse motion events - send OnMouseMove to SHIORI
  -- OnMouseMove: Reference0=x, Reference1=y, Reference2="", Reference3=side(0=sakura), Reference4=part
  -- We throttle this to avoid spamming SHIORI - only send when over a collision region
  let handleMotion ( x, y ) = do
        let ix = floor x :: Int
            iy = floor y :: Int
            surfId = maybe 0 scSurfaceId mShiori
        case findCollisionAt ix iy collisions of
          Just cr -> do
            let refs = Map.fromList
                  [ (0, T.pack $ show ix)      -- Reference0: x
                  , (1, T.pack $ show iy)      -- Reference1: y
                  , (2, "")                    -- Reference2: wheel (empty for move)
                  , (3, "0")                   -- Reference3: side (0=sakura)
                  , (4, crName cr)             -- Reference4: part name
                  , (5, T.pack $ show surfId)  -- Reference5: surface id (SSP extension)
                  ]
            sendShioriAndLog mShiori OnMouseMove refs
          Nothing -> return ()  -- Don't send if not over a collision region

  reactimate $ handleMotion <$> motionE

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
  
  -- Handle click with collision info and send to SHIORI
  let handleCollisionHit hit = do
        logCollisionHit hit
        case hit of
          HitRegion evt cr -> do
            -- Send OnMouseClick with collision info
            -- Reference0: x coordinate
            -- Reference1: y coordinate  
            -- Reference2: 0 (left button)
            -- Reference3: collision region name
            -- Reference4: character id (0 for sakura)
            -- Reference5: surface id
            let surfId = maybe 0 scSurfaceId mShiori
                refs = Map.fromList
                  [ (0, T.pack $ show $ clickX evt)
                  , (1, T.pack $ show $ clickY evt)
                  , (2, "0")  -- Left button
                  , (3, crName cr)
                  , (4, "0")  -- Character 0 (sakura)
                  , (5, T.pack $ show surfId)
                  ]
            sendShioriAndLog mShiori OnMouseClick refs
          HitNothing evt -> do
            -- Click on transparent area - still send event but with empty region
            let surfId = maybe 0 scSurfaceId mShiori
                refs = Map.fromList
                  [ (0, T.pack $ show $ clickX evt)
                  , (1, T.pack $ show $ clickY evt)
                  , (2, "0")  -- Left button
                  , (3, "")   -- No collision region
                  , (4, "0")  -- Character 0
                  , (5, T.pack $ show surfId)
                  ]
            sendShioriAndLog mShiori OnMouseClick refs
  
  reactimate $ handleCollisionHit <$> hitE

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

  -- Handle window movement based on mode
  case moveMode of
    MoveToplevel beginMove -> do
      -- Initiate window move only when drag first exceeds threshold
      -- We pass the start position (from behavior) to beginMove
      let firstExceedE = filterE exceedsThreshold dragUpdateE
          moveE        = dragStartB <@ firstExceedE
      reactimate $ uncurry beginMove <$> moveE

    MoveLayerShell updatePosition -> do
      -- For layer-shell, update position on every drag update that exceeds threshold
      let significantUpdateE = filterE exceedsThreshold dragUpdateE
      reactimate $ uncurry updatePosition <$> significantUpdateE
