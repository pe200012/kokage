{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SHIORI Bridge Test Program
--
-- This program tests the Wine bridge by:
-- 1. Loading a SHIORI DLL
-- 2. Sending OnBoot event
-- 3. Sending OnSecondChange and OnMinuteChange events
-- 4. Sending OnClose event
-- 5. Unloading the DLL
--
-- Usage: shiori-test <dll-path> <ghost-path>
-- Example: shiori-test test-fdr/ghost/emily4/ghost/master/yaya.dll test-fdr/ghost/emily4/ghost/master

module Main ( main ) where

import           Control.Concurrent         ( threadDelay )
import           Control.Monad              ( forM_ )

import qualified Data.Map.Strict            as Map
import           Data.Text                  ( Text )
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO

import           Data.Time.Clock            ( getCurrentTime )
import           Data.Time.LocalTime        ( LocalTime(..)
                                            , TimeOfDay(..)
                                            , getCurrentTimeZone
                                            , utcToLocalTime
                                            )

import           System.Environment         ( getArgs )
import           System.Exit                ( exitFailure )
import           System.FilePath            ( takeDirectory, (</>) )
import           System.Directory           ( makeAbsolute )

import           Kokage.Shiori.WineBridge   ( WineBridgeConfig(..)
                                            , WineShiori
                                            , defaultWineBridgeConfig
                                            , loadShiori
                                            , sendEvent
                                            , startWineBridge
                                            , stopWineBridge
                                            , unloadShiori
                                            )
import           Types.Shiori               ( ShioriEvent(..)
                                            , ShioriResponse(..)
                                            , ShioriStatusCode(..)
                                            )

--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dllPath, ghostPath] -> runTest dllPath ghostPath
    [dllPath]            -> runTest dllPath (takeDirectory dllPath)
    []                   -> runDefaultTest
    _                    -> usage

usage :: IO ()
usage = do
  putStrLn "SHIORI Bridge Test Program"
  putStrLn ""
  putStrLn "Usage: shiori-test [dll-path] [ghost-path]"
  putStrLn ""
  putStrLn "Arguments:"
  putStrLn "  dll-path    Path to SHIORI DLL (e.g., yaya.dll)"
  putStrLn "  ghost-path  Path to ghost/master directory"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  shiori-test test-fdr/ghost/emily4/ghost/master/yaya.dll"
  putStrLn "  shiori-test test-fdr/ghost/emily4/ghost/master/yaya.dll test-fdr/ghost/emily4/ghost/master"
  putStrLn ""
  putStrLn "If no arguments given, uses default emily4 ghost from test-fdr."

runDefaultTest :: IO ()
runDefaultTest = do
  let defaultDll   = "test-fdr/ghost/emily4/ghost/master/yaya.dll"
      defaultGhost = "test-fdr/ghost/emily4/ghost/master"
  logInfo "Using default test ghost: emily4"
  runTest defaultDll defaultGhost

--------------------------------------------------------------------------------
-- Test Runner
--------------------------------------------------------------------------------

runTest :: FilePath -> FilePath -> IO ()
runTest dllPath ghostPath = do
  -- Convert to absolute paths
  absDllPath   <- makeAbsolute dllPath
  absGhostPath <- makeAbsolute ghostPath
  
  logInfo $ "DLL Path: " <> T.pack absDllPath
  logInfo $ "Ghost Path: " <> T.pack absGhostPath
  logInfo ""
  
  -- Configure the bridge - use 32-bit bridge for 32-bit DLLs (most SHIORI DLLs are 32-bit)
  let config = defaultWineBridgeConfig
        { wbcBridgePath   = "wine-helper" </> "shiori_bridge32.exe"
        , wbcBridgeSoPath = "wine-helper" </> "shiori_bridge32.exe.so"
        , wbcSenderName   = "Kokage"
        , wbcTimeoutMs    = 60000  -- 60 seconds for testing
        }
  
  logInfo "Starting Wine bridge..."
  result <- startWineBridge config
  
  case result of
    Left err -> do
      logError $ "Failed to start bridge: " <> T.pack err
      exitFailure
    Right bridge -> do
      logSuccess "Bridge started!"
      logInfo ""
      
      -- Run test sequence
      runTestSequence bridge absDllPath absGhostPath
      
      -- Cleanup
      logInfo ""
      logInfo "Stopping bridge..."
      stopWineBridge bridge
      logSuccess "Done!"

runTestSequence :: WineShiori -> FilePath -> FilePath -> IO ()
runTestSequence bridge dllPath ghostPath = do
  -- Load SHIORI DLL
  logInfo "Loading SHIORI DLL..."
  loadResult <- loadShiori bridge dllPath ghostPath
  
  case loadResult of
    Left err -> do
      logError $ "Failed to load SHIORI: " <> T.pack err
      return ()
    Right bridge' -> do
      logSuccess "SHIORI loaded!"
      logInfo ""
      
      -- Get current time info for events
      now <- getCurrentTime
      tz  <- getCurrentTimeZone
      let localTime = utcToLocalTime tz now
          timeOfDay = localTimeOfDay localTime
          hour      = todHour timeOfDay
          minute    = todMin timeOfDay
          second    = floor (todSec timeOfDay) :: Int
      
      -- Send OnBoot event
      logInfo "=== Sending OnBoot ==="
      sendOnBoot bridge'
      logInfo ""
      
      -- Wait a bit
      threadDelay 500000  -- 500ms
      
      -- Send OnSecondChange events (a few times)
      logInfo "=== Sending OnSecondChange (3 times) ==="
      forM_ [0..2] $ \i -> do
        let sec = (second + i) `mod` 60
        sendOnSecondChange bridge' hour minute sec
        threadDelay 1000000  -- 1 second
      logInfo ""
      
      -- Send OnMinuteChange event
      logInfo "=== Sending OnMinuteChange ==="
      sendOnMinuteChange bridge' hour minute
      logInfo ""
      
      -- Wait a bit
      threadDelay 500000  -- 500ms
      
      -- Send OnClose event
      logInfo "=== Sending OnClose ==="
      sendOnClose bridge'
      logInfo ""
      
      -- Unload SHIORI
      logInfo "Unloading SHIORI..."
      unloadResult <- unloadShiori bridge'
      case unloadResult of
        Left err -> logError $ "Failed to unload: " <> T.pack err
        Right _  -> logSuccess "SHIORI unloaded!"

--------------------------------------------------------------------------------
-- Event Senders
--------------------------------------------------------------------------------

-- | Send OnBoot event
-- Reference0: Shell name at startup
-- Reference6: "halt" if crashed last time
-- Reference7: Ghost name that crashed (if crashed)
sendOnBoot :: WineShiori -> IO ()
sendOnBoot bridge = do
  let refs = Map.fromList
        [ (0, "master")           -- Current shell name
        ]
  sendEventAndLog bridge OnBoot refs

-- | Send OnClose event
-- Reference0: Close reason (user/system)
-- Reference1: Scope that initiated close
-- Reference2: Window scope that initiated close
sendOnClose :: WineShiori -> IO ()
sendOnClose bridge = do
  let refs = Map.fromList
        [ (0, "user")    -- Close reason
        , (1, "0")       -- Scope number (sakura side)
        , (2, "0")       -- Window scope
        ]
  sendEventAndLog bridge OnClose refs

-- | Send OnSecondChange event
-- Reference0: OS uptime in hours
-- Reference1: 1 if offscreen, 0 otherwise
-- Reference2: 1 if overlapping, 0 otherwise
-- Reference3: 1 if can talk, 0 otherwise
-- Reference4: Idle time in seconds
sendOnSecondChange :: WineShiori -> Int -> Int -> Int -> IO ()
sendOnSecondChange bridge _hour _minute _second = do
  let refs = Map.fromList
        [ (0, "0")       -- OS uptime hours
        , (1, "0")       -- Not offscreen
        , (2, "0")       -- Not overlapping
        , (3, "1")       -- Can talk
        , (4, "0")       -- Idle time
        ]
  sendEventAndLog bridge OnSecondChange refs

-- | Send OnMinuteChange event
-- Same references as OnSecondChange
sendOnMinuteChange :: WineShiori -> Int -> Int -> IO ()
sendOnMinuteChange bridge _hour _minute = do
  let refs = Map.fromList
        [ (0, "0")       -- OS uptime hours
        , (1, "0")       -- Not offscreen
        , (2, "0")       -- Not overlapping
        , (3, "1")       -- Can talk
        , (4, "0")       -- Idle time
        ]
  sendEventAndLog bridge OnMinuteChange refs

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Send event and log the response
sendEventAndLog :: WineShiori -> ShioriEvent -> Map.Map Int Text -> IO ()
sendEventAndLog bridge event refs = do
  logInfo $ "Sending event: " <> T.pack (show event)
  result <- sendEvent bridge event refs
  
  case result of
    Left err -> logError $ "  Error: " <> T.pack err
    Right response -> logResponse response

-- | Log a SHIORI response
logResponse :: ShioriResponse -> IO ()
logResponse response = do
  let status = srsStatus response
  case status of
    Status200 -> do
      logSuccess $ "  Status: 200 OK"
      case srsValue response of
        Nothing  -> logInfo "  Value: (empty)"
        Just val -> do
          logInfo "  Value:"
          -- Print each line of the script indented
          let scriptLines = T.lines val
          forM_ scriptLines $ \line ->
            TIO.putStrLn $ "    " <> line
    Status204 -> do
      logInfo "  Status: 204 No Content (no response script)"
    Status310 -> do
      logInfo "  Status: 310 Communicate"
      case srsValue response of
        Nothing  -> return ()
        Just val -> logInfo $ "  Value: " <> val
    Status311 -> do
      logInfo "  Status: 311 Not Enough"
    Status312 -> do
      logInfo "  Status: 312 Advice"
    Status400 -> do
      logError "  Status: 400 Bad Request"
    Status500 -> do
      logError "  Status: 500 Internal Server Error"
    StatusOther code msg -> do
      logInfo $ "  Status: " <> T.pack (show code) <> " " <> msg

--------------------------------------------------------------------------------
-- Logging Helpers
--------------------------------------------------------------------------------

logInfo :: Text -> IO ()
logInfo msg = TIO.putStrLn msg

logSuccess :: Text -> IO ()
logSuccess msg = TIO.putStrLn $ "[OK] " <> msg

logError :: Text -> IO ()
logError msg = TIO.putStrLn $ "[ERROR] " <> msg
