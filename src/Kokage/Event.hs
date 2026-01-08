{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | FRP event network for Kokage.
-- Handles GTK event to FRP event conversion and network setup.
--
-- This module re-exports types from submodules for convenience:
--
-- * "Types.Event" - Event types (ClickEvent, DragEvent, etc.)
-- * "Kokage.Event.Config" - Configuration types (NetworkConfig, ShioriConfig, etc.)
-- * "Kokage.Event.Shiori" - SHIORI dispatch helpers
module Kokage.Event
  ( -- * Re-exports
    module Types.Event
  , module Kokage.Event.Config
  , module Kokage.Event.Shiori
    -- * Network Setup
  , setupNetwork
  , setupCharacterNetwork
  , setupGlobalNetwork
    -- * Event Handlers
  , handleClick
  ) where

import           Control.Monad              ( when )

import           Data.IORef                 ( newIORef, readIORef, writeIORef )
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import           Data.Time.Clock            ( UTCTime, diffUTCTime, getCurrentTime )

import qualified GI.Gdk                     as Gdk
import qualified GI.Gtk                     as Gtk

import           Kokage.Collision           ( findCollisionAt )
import           Kokage.Event.Config
import           Kokage.Event.Shiori
-- Re-export submodules
import           Types.Event

import           Reactive.Banana            ( (<@)
                                            , (<@>)
                                            , Behavior
                                            , Event
                                            , filterE
                                            , stepper
                                            , unionWith
                                            )
import           Reactive.Banana.Frameworks ( MomentIO, fromAddHandler, liftIO, reactimate )
import           Reactive.Banana.GI.Gtk     ( signalE0R )

import           Types.Ghost                ( CollisionRegion(..) )
import           Types.Shiori               ( ShioriEvent(..) )

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
  hourTickE <- fromAddHandler (thHourTick timers)
  motionTickE <- fromAddHandler (thMotionTick timers)

  -- Handle timer events
  let handleSecondTick lt = do
        logSecondTick lt
        now <- getCurrentTime
        let uptime = case mShiori of
              Just sc -> getUptimeHours (scStartTime sc) now
              Nothing -> 0
            refs
              = Map.fromList [ ( 0, T.pack $ show uptime ), ( 1, "0" ), ( 2, "0" ), ( 3, "1" ) ]
        sendShioriAndLog mShiori OnSecondChange refs

  let handleMinuteTick lt = do
        logMinuteTick lt
        now <- getCurrentTime
        let uptime = case mShiori of
              Just sc -> getUptimeHours (scStartTime sc) now
              Nothing -> 0
            refs
              = Map.fromList [ ( 0, T.pack $ show uptime ), ( 1, "0" ), ( 2, "0" ), ( 3, "1" ) ]
        sendShioriAndLog mShiori OnMinuteChange refs

  let handleHourTick lt = do
        logHourTick lt
        now <- getCurrentTime
        let uptime = case mShiori of
              Just sc -> getUptimeHours (scStartTime sc) now
              Nothing -> 0
            refs
              = Map.fromList [ ( 0, T.pack $ show uptime ), ( 1, "0" ), ( 2, "0" ), ( 3, "1" ) ]
        sendShioriAndLog mShiori OnHourTimeSignal refs

  reactimate $ handleSecondTick <$> secondTickE
  reactimate $ handleMinuteTick <$> minuteTickE
  reactimate $ handleHourTick <$> hourTickE

  -- Handle mouse motion events with throttling via Behavior + sampling
  -- Instead of sending OnMouseMove on every motion event, we:
  -- 1. Store the latest motion position in a Behavior
  -- 2. Track the last motion timestamp in an IORef
  -- 3. Sample this Behavior on motionTickE (every 100ms)
  -- 4. Only send OnMouseMove when sampled AND motion is fresh (< 1s old)

  -- IORef to track last motion timestamp
  lastMotionTimeRef <- liftIO $ newIORef (Nothing :: Maybe UTCTime)

  -- Create a Behavior that holds the latest mouse position
  motionB :: Behavior (Maybe ( Double, Double )) <- stepper Nothing (Just <$> motionE)

  -- Update timestamp on every motion
  let recordMotionTime _ = writeIORef lastMotionTimeRef . Just =<< getCurrentTime
  reactimate $ recordMotionTime <$> motionE

  -- Sample the motion position on each tick
  let sampledMotionE = motionB <@ motionTickE

  -- Motion expiry threshold
  let motionExpirySeconds :: Double
      motionExpirySeconds = 0.1

  let handleSampledMotion mPos = case mPos of
        Nothing       -> return ()  -- No motion yet
        Just ( x, y ) -> do
          mLastTime <- readIORef lastMotionTimeRef
          case mLastTime of
            Nothing       -> return ()
            Just lastTime -> do
              now <- getCurrentTime
              let age = realToFrac (diffUTCTime now lastTime) :: Double
              -- Only send if motion is fresh
              when (age < motionExpirySeconds) $ do
                let ix     = floor x :: Int
                    iy     = floor y :: Int
                    surfId = maybe 0 scSurfaceId mShiori
                case findCollisionAt ix iy collisions of
                  Just cr -> do
                    let refs
                          = Map.fromList
                            [ ( 0, T.pack $ show ix )
                            , ( 1, T.pack $ show iy )
                            , ( 2, "" )
                            , ( 3, "0" )
                            , ( 4, crName cr )
                            , ( 5, T.pack $ show surfId )
                            ]
                    sendShioriAndLog mShiori OnMouseMove refs
                  Nothing -> return ()

  reactimate $ handleSampledMotion <$> sampledMotionE

  -- Track drag start position
  dragStartB :: Behavior ( Double, Double ) <- stepper ( 0, 0 ) dragBeginE

  let exceedsThreshold ( ox, oy ) = isDragSignificant ox oy
      dragEndWithStart = (,) <$> dragStartB <@> dragEndE
      clickE = fst <$> filterE (not . exceedsThreshold . snd) dragEndWithStart
      suppressedE = filterE (exceedsThreshold . snd) dragEndWithStart

  let hitE = handleClick collisions <$> clickE

  let handleCollisionHit hit = do
        logCollisionHit hit
        case hit of
          HitRegion evt cr -> do
            let surfId = maybe 0 scSurfaceId mShiori
                refs
                  = Map.fromList
                    [ ( 0, T.pack $ show $ clickX evt )
                    , ( 1, T.pack $ show $ clickY evt )
                    , ( 2, "0" )
                    , ( 3, crName cr )
                    , ( 4, "0" )
                    , ( 5, T.pack $ show surfId )
                    ]
            sendShioriAndLog mShiori OnMouseClick refs
          HitNothing evt   -> do
            let surfId = maybe 0 scSurfaceId mShiori
                refs
                  = Map.fromList
                    [ ( 0, T.pack $ show $ clickX evt )
                    , ( 1, T.pack $ show $ clickY evt )
                    , ( 2, "0" )
                    , ( 3, "" )
                    , ( 4, "0" )
                    , ( 5, T.pack $ show surfId )
                    ]
            sendShioriAndLog mShiori OnMouseClick refs

  reactimate $ handleCollisionHit <$> hitE
  reactimate $ putStrLn "[Click] Suppressed (drag exceeded threshold)" <$ suppressedE

  -- Drag event logging
  let mkDragStart ( x, y ) = DragEvent DragStart x y 0 0
      mkDragMove ( ox, oy ) = DragEvent DragMove 0 0 ox oy
      mkDragEnd ( ox, oy ) = DragEvent DragEnd 0 0 ox oy

  let dragStartE = mkDragStart <$> dragBeginE
      dragMoveE  = mkDragMove <$> filterE exceedsThreshold dragUpdateE
      dragEndE'  = mkDragEnd <$> filterE exceedsThreshold dragEndE

  reactimate $ logDragEvent <$> dragStartE
  reactimate $ logDragEvent <$> dragMoveE
  reactimate $ logDragEvent <$> dragEndE'

  -- Handle window movement
  case moveMode of
    MoveToplevel beginMove        -> do
      let firstExceedE = filterE exceedsThreshold dragUpdateE
          moveE        = dragStartB <@ firstExceedE
      reactimate $ uncurry beginMove <$> moveE
    MoveLayerShell updatePosition -> do
      let significantUpdateE = filterE exceedsThreshold dragUpdateE
      reactimate $ uncurry updatePosition <$> significantUpdateE

-- | Set up the FRP network for a single character window.
setupCharacterNetwork :: CharacterNetworkConfig -> MomentIO ()
setupCharacterNetwork config = do
  let window      = cncWindow config
      inputs      = cncInputs config
      collisions  = cncCollisions config
      moveMode    = cncMoveMode config
      scopeId     = cncScopeId config
      mShiori     = cncShiori config
      handler     = cncScriptHandler config
      contextMenu = cncContextMenu config
      motionTick  = cncMotionTick config

  closeE <- signalE0R window #closeRequest False
  reactimate $ sendShioriWithCallback mShiori OnClose Map.empty handler <$ closeE

  dragBeginE <- fromAddHandler (ihDragBegin inputs)
  dragUpdateE <- fromAddHandler (ihDragUpdate inputs)
  dragEndE <- fromAddHandler (ihDragEnd inputs)
  motionE <- fromAddHandler (ihMotion inputs)
  rightClickE <- fromAddHandler (ihRightClick inputs)
  motionTickE <- fromAddHandler motionTick

  -- Right-click context menu
  let handleRightClick ( x, y ) = do
        putStrLn $ "[Menu] Right-click at (" <> show x <> ", " <> show y <> ")"
        rect <- Gdk.newZeroRectangle
        Gdk.setRectangleX rect (round x)
        Gdk.setRectangleY rect (round y)
        Gdk.setRectangleWidth rect 1
        Gdk.setRectangleHeight rect 1
        Gtk.popoverSetPointingTo contextMenu (Just rect)
        Gtk.popoverPopup contextMenu

  reactimate $ handleRightClick <$> rightClickE

  -- Mouse motion with cursor change and throttled OnMouseMove
  -- Use Behavior + sampling to throttle OnMouseMove events (every 100ms)
  -- Also check that the motion event is fresh (< 1 second old)

  -- IORef to track last motion timestamp
  lastMotionTimeRef <- liftIO $ newIORef (Nothing :: Maybe UTCTime)

  motionB :: Behavior (Maybe ( Double, Double )) <- stepper Nothing (Just <$> motionE)
  let sampledMotionE = motionB <@ motionTickE

  -- Motion expiry threshold (1 second)
  let motionExpirySeconds :: Double
      motionExpirySeconds = 1.0

  -- Update cursor on every motion (immediate feedback) and record timestamp
  let updateCursor ( x, y ) = do
        writeIORef lastMotionTimeRef . Just =<< getCurrentTime
        let ix = floor x :: Int
            iy = floor y :: Int
        case findCollisionAt ix iy collisions of
          Just _cr -> do
            mCursor <- Gdk.cursorNewFromName "pointer" (Nothing :: Maybe Gdk.Cursor)
            Gtk.widgetSetCursor window mCursor
          Nothing  -> Gtk.widgetSetCursor window (Nothing :: Maybe Gdk.Cursor)

  reactimate $ updateCursor <$> motionE

  -- Send OnMouseMove only on sampled ticks (throttled) and if fresh
  let handleSampledMotion mPos = case mPos of
        Nothing       -> return ()
        Just ( x, y ) -> do
          mLastTime <- readIORef lastMotionTimeRef
          case mLastTime of
            Nothing       -> return ()
            Just lastTime -> do
              now <- getCurrentTime
              let age = realToFrac (diffUTCTime now lastTime) :: Double
              -- Only send if motion is fresh (< 1 second old)
              when (age < motionExpirySeconds) $ do
                let ix     = floor x :: Int
                    iy     = floor y :: Int
                    surfId = maybe 0 scSurfaceId mShiori
                case findCollisionAt ix iy collisions of
                  Just cr -> do
                    let refs
                          = Map.fromList
                            [ ( 0, T.pack $ show ix )
                            , ( 1, T.pack $ show iy )
                            , ( 2, "" )
                            , ( 3, T.pack $ show scopeId )
                            , ( 4, crName cr )
                            , ( 5, T.pack $ show surfId )
                            ]
                    sendShioriWithCallback mShiori OnMouseMove refs handler
                  Nothing -> return ()

  reactimate $ handleSampledMotion <$> sampledMotionE

  dragStartB :: Behavior ( Double, Double ) <- stepper ( 0, 0 ) dragBeginE

  let exceedsThreshold ( ox, oy ) = isDragSignificant ox oy
      resetToFalse = False <$ dragBeginE
      exceedsUpdate = filterE exceedsThreshold dragUpdateE
      setToTrue = True <$ exceedsUpdate
      dragExceededE = unionWith (||) resetToFalse setToTrue

  dragExceededB :: Behavior Bool <- stepper False dragExceededE

  let dragEndWithState = (,) <$> dragStartB <*> dragExceededB <@ dragEndE
      suppressedE      = filterE snd dragEndWithState

  leftClickE <- fromAddHandler (ihLeftClick inputs)

  let leftClickWithDragState = (,) <$> dragExceededB <@> leftClickE
      validClickE = snd <$> filterE (not . fst) leftClickWithDragState
      singleClickE = (\( _, x, y ) -> ( x, y )) <$> filterE (\( n, _, _ ) -> n == 1) validClickE
      doubleClickE = (\( _, x, y ) -> ( x, y )) <$> filterE (\( n, _, _ ) -> n == 2) validClickE

  let singleHitE = handleClick collisions <$> singleClickE
      doubleHitE = handleClick collisions <$> doubleClickE

  -- Unified click handlers using handleMouseClick from Event.Shiori
  reactimate $ (\hit -> handleMouseClick mShiori OnMouseClick scopeId hit handler) <$> singleHitE
  reactimate
    $ (\hit -> handleMouseClick mShiori OnMouseDoubleClick scopeId hit handler) <$> doubleHitE
  reactimate $ putStrLn "[Click] Suppressed (drag exceeded threshold)" <$ suppressedE

  -- Drag logging
  let mkDragStart ( x, y ) = DragEvent DragStart x y 0 0
      mkDragMove ( ox, oy ) = DragEvent DragMove 0 0 ox oy
      mkDragEnd ( ox, oy ) = DragEvent DragEnd 0 0 ox oy

  let dragStartE = mkDragStart <$> dragBeginE
      dragMoveE  = mkDragMove <$> filterE exceedsThreshold dragUpdateE
      dragEndE'  = mkDragEnd <$> filterE exceedsThreshold dragEndE

  reactimate $ logDragEvent <$> dragStartE
  reactimate $ logDragEvent <$> dragMoveE
  reactimate $ logDragEvent <$> dragEndE'

  -- Window movement
  case moveMode of
    MoveToplevel beginMove        -> do
      let firstExceedE = filterE exceedsThreshold dragUpdateE
          moveE        = dragStartB <@ firstExceedE
      reactimate $ uncurry beginMove <$> moveE
    MoveLayerShell updatePosition -> do
      let significantUpdateE = filterE exceedsThreshold dragUpdateE
      reactimate $ uncurry updatePosition <$> significantUpdateE

  -- ============ BALLOON EVENTS ============
  let balloonWindow   = cncBalloonWindow config
      balloonInputs   = cncBalloonInputs config
      balloonMoveMode = cncBalloonMoveMode config

  balloonCloseE <- signalE0R balloonWindow #closeRequest True
  reactimate $ putStrLn "[Balloon] Close request (hidden)" <$ balloonCloseE

  balloonDragBeginE <- fromAddHandler (ihDragBegin balloonInputs)
  balloonDragUpdateE <- fromAddHandler (ihDragUpdate balloonInputs)
  _balloonDragEndE <- fromAddHandler (ihDragEnd balloonInputs)

  case balloonMoveMode of
    BalloonMoveToplevel beginBalloonMove     -> do
      let balloonExceedsThreshold ( ox, oy ) = isDragSignificant ox oy
          balloonFirstExceedE = filterE balloonExceedsThreshold balloonDragUpdateE
      balloonDragStartB <- stepper ( 0, 0 ) balloonDragBeginE
      let balloonMoveE = balloonDragStartB <@ balloonFirstExceedE
      reactimate $ uncurry beginBalloonMove <$> balloonMoveE
    BalloonMoveLayerShell setBalloonPosition
      -> reactimate $ uncurry setBalloonPosition <$> balloonDragUpdateE

-- | Set up the global FRP network for timers.
setupGlobalNetwork :: GlobalNetworkConfig -> MomentIO ()
setupGlobalNetwork config = do
  let timers  = gncTimers config
      mShiori = gncShiori config
      handler = gncScriptHandler config

  secondTickE <- fromAddHandler (thSecondTick timers)
  minuteTickE <- fromAddHandler (thMinuteTick timers)
  hourTickE <- fromAddHandler (thHourTick timers)

  let handleSecondTick lt = do
        logSecondTick lt
        now <- getCurrentTime
        let uptime = case mShiori of
              Just sc -> getUptimeHours (scStartTime sc) now
              Nothing -> 0
            refs
              = Map.fromList [ ( 0, T.pack $ show uptime ), ( 1, "0" ), ( 2, "0" ), ( 3, "1" ) ]
        sendShioriWithCallback mShiori OnSecondChange refs handler

  let handleMinuteTick lt = do
        logMinuteTick lt
        now <- getCurrentTime
        let uptime = case mShiori of
              Just sc -> getUptimeHours (scStartTime sc) now
              Nothing -> 0
            refs
              = Map.fromList [ ( 0, T.pack $ show uptime ), ( 1, "0" ), ( 2, "0" ), ( 3, "1" ) ]
        sendShioriAndLog mShiori OnMinuteChange refs

  let handleHourTick lt = do
        logHourTick lt
        now <- getCurrentTime
        let uptime = case mShiori of
              Just sc -> getUptimeHours (scStartTime sc) now
              Nothing -> 0
            refs
              = Map.fromList [ ( 0, T.pack $ show uptime ), ( 1, "0" ), ( 2, "0" ), ( 3, "1" ) ]
        sendShioriAndLog mShiori OnHourTimeSignal refs

  reactimate $ handleSecondTick <$> secondTickE
  reactimate $ handleMinuteTick <$> minuteTickE
  reactimate $ handleHourTick <$> hourTickE
