{-# LANGUAGE OverloadedStrings #-}

-- | SHIORI event dispatch helpers for Kokage FRP network.
module Kokage.Event.Shiori
  ( -- * SHIORI communication
    sendShioriAndLog
  , sendShioriWithCallback
    -- * Reference builders
  , buildMouseClickRefs
  , buildMouseMoveRefs
  , handleMouseClick
    -- * Logging helpers
  , logShioriResponse
  , logCollisionHit
  , logDragEvent
  , logSecondTick
  , logMinuteTick
  , logHourTick
    -- * Time utilities
  , formatTime
  , getUptimeHours
  ) where

import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import           Data.Time.Clock          ( UTCTime, diffUTCTime )
import           Data.Time.LocalTime      ( LocalTime(..), TimeOfDay(..) )

import           Kokage.Event.Config      ( ShioriConfig(..) )
import           Kokage.Shiori.WineBridge ( sendEvent )

import           Prelude                  ()

import           Relude

import           Types.Event              ( ClickEvent(..), CollisionHit(..), DragEvent(..) )
import           Types.Ghost              ( CollisionRegion(..) )
import           Types.Shiori             ( ShioriEvent, ShioriResponse(..), srsValue )

-- | Log a SHIORI response for debugging.
logShioriResponse :: ShioriEvent -> Either String ShioriResponse -> IO ()
logShioriResponse event result = case result of
  Left err   -> putStrLn $ "[SHIORI] " <> show event <> " error: " <> err
  Right resp -> case srsValue resp of
    Nothing  -> putStrLn $ "[SHIORI] " <> show event <> " -> (no content)"
    Just val -> do
      putStrLn $ "[SHIORI] " <> show event <> " -> script:"
      -- Print first 200 chars of script for debugging
      -- let preview = T.take 200 val
      -- putStrLn $ "  " <> T.unpack preview <> if T.length val > 200 then "..." else ""
      putStrLn $ T.unpack val

-- | Send a SHIORI event and log the response.
-- Does nothing if SHIORI is not configured.
sendShioriAndLog :: Maybe ShioriConfig -> ShioriEvent -> Map.Map Int T.Text -> IO ()
sendShioriAndLog Nothing _ _          = return ()  -- No SHIORI, skip
sendShioriAndLog (Just sc) event refs = do
  result <- sendEvent (scShiori sc) event refs
  logShioriResponse event result

-- | Send a SHIORI event with a callback for the response.
-- The callback receives Just the script text on success, Nothing on failure.
-- Does nothing if SHIORI is not configured.
sendShioriWithCallback
  :: Maybe ShioriConfig
  -> ShioriEvent
  -> Map.Map Int T.Text
  -> (Maybe T.Text -> IO ())  -- ^ Callback with response script
  -> IO ()
sendShioriWithCallback Nothing _ _ _ = return ()  -- No SHIORI, skip
sendShioriWithCallback (Just sc) event refs callback = do
  result <- sendEvent (scShiori sc) event refs
  logShioriResponse event result
  case result of
    Left _     -> callback Nothing
    Right resp -> callback (srsValue resp)

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
    pad n
      = if n < 10
        then "0" <> show n
        else show n

-- | Calculate uptime in hours from start time to now.
getUptimeHours :: UTCTime -> UTCTime -> Int
getUptimeHours startTime now
  = let
      diffSeconds = diffUTCTime now startTime
      hours       = floor (realToFrac diffSeconds / 3600 :: Double) :: Int
    in 
      max 0 hours  -- Ensure non-negative

-- | Log a second timer event.
logSecondTick :: LocalTime -> IO ()
logSecondTick lt = putStrLn $ "[Timer] Second tick: " <> formatTime (localTimeOfDay lt)

-- | Log a minute timer event.
logMinuteTick :: LocalTime -> IO ()
logMinuteTick lt = putStrLn $ "[Timer] Minute tick: " <> formatTime (localTimeOfDay lt)

-- | Log an hour timer event.
logHourTick :: LocalTime -> IO ()
logHourTick lt = putStrLn $ "[Timer] Hour tick: " <> formatTime (localTimeOfDay lt)

-- | Build SHIORI Reference map for mouse click events.
-- Reference format:
--   0: x coordinate
--   1: y coordinate
--   2: wheel (always "0" for clicks)
--   3: scope ID (side)
--   4: collision region name (part), empty if no hit
--   5: button ("0"=left, "1"=right, "2"=middle)
--   6: input type (always "mouse")
buildMouseClickRefs :: ClickEvent -> Int -> Maybe T.Text -> Int -> Map.Map Int T.Text
buildMouseClickRefs evt scopeId mCollisionName button
  = Map.fromList
    [ ( 0, T.pack $ show $ clickX evt )
    , ( 1, T.pack $ show $ clickY evt )
    , ( 2, "0" )  -- wheel
    , ( 3, T.pack $ show scopeId )
    , ( 4, fromMaybe "" mCollisionName )
    , ( 5, T.pack $ show button )
    , ( 6, "mouse" )
    ]

-- | Build SHIORI Reference map for mouse move events.
-- Reference format:
--   0: x coordinate
--   1: y coordinate
--   2: wheel (empty for move)
--   3: scope ID
--   4: collision region name
--   5: surface ID
buildMouseMoveRefs :: Int -> Int -> Int -> T.Text -> Int -> Map.Map Int T.Text
buildMouseMoveRefs x y scopeId collisionName surfaceId
  = Map.fromList
    [ ( 0, T.pack $ show x )
    , ( 1, T.pack $ show y )
    , ( 2, "" )
    , ( 3, T.pack $ show scopeId )
    , ( 4, collisionName )
    , ( 5, T.pack $ show surfaceId )
    ]

-- | Unified handler for mouse click events (single or double click).
-- Logs the hit and sends the appropriate SHIORI event.
handleMouseClick :: Maybe ShioriConfig
                 -> ShioriEvent           -- ^ OnMouseClick or OnMouseDoubleClick
                 -> Int                   -- ^ Scope ID
                 -> CollisionHit          -- ^ Hit test result
                 -> (Maybe T.Text -> IO ()) -- ^ Script handler callback
                 -> IO ()
handleMouseClick mShiori event scopeId hit handler = do
  logCollisionHit hit
  let ( evt, mCollisionName ) = case hit of
        HitRegion e cr -> ( e, Just $ crName cr )
        HitNothing e   -> ( e, Nothing )
      refs = buildMouseClickRefs evt scopeId mCollisionName 0  -- 0 = left button
  sendShioriWithCallback mShiori event refs handler
