{-# LANGUAGE OverloadedStrings #-}

-- | SHIORI initialization and cleanup utilities.
module Kokage.Init
  ( -- * SHIORI lifecycle
    initializeShiori
  , cleanupShiori
  ) where

import Prelude ()
import Relude

import           Control.Exception        ( try )

import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import           Data.Time.Clock          ( UTCTime, diffUTCTime, getCurrentTime )
import           Data.Time.Format         ( defaultTimeLocale, formatTime )
import           Data.Time.LocalTime      ( getZonedTime
                                          , localDay
                                          , localTimeOfDay
                                          , todHour
                                          , todMin
                                          , todSec
                                          , zonedTimeToLocalTime
                                          )

import           Kokage.Config            ( GhostHistory(..)
                                          , defaultGhostHistory
                                          , loadGhostHistory
                                          , saveGhostHistory
                                          )
import           Kokage.Shiori.WineBridge ( WineShiori, sendRequest )

import           Types.Ghost              ( Ghost(..), descriptSakuraName, ghostDescript )
import           Types.Shiori             ( ShioriEvent(..)
                                          , ShioriResponse(..)
                                          , eventToId
                                          , mkRequest
                                          )

-- | Initialize SHIORI with boot events.
-- Sends OnFirstBoot or OnBoot depending on history.
-- Returns the start time for uptime calculation.
initializeShiori :: WineShiori -> Ghost -> IO UTCTime
initializeShiori shiori ghost = do
  startTime <- getCurrentTime
  let gPath = ghostPath ghost

  -- Load history
  mHist <- loadGhostHistory gPath
  let hist    = fromMaybe defaultGhostHistory mHist
      isFirst = ghBootCount hist == 0

  -- Get current time info
  zt <- getZonedTime
  let lt     = zonedTimeToLocalTime zt
      day    = localDay lt
      tod    = localTimeOfDay lt
      hour   = todHour tod
      minute = todMin tod
      sec    = floor (todSec tod) :: Int

  -- Build reference map
  let refs
        = Map.fromList
          [ ( 0, T.pack $ show $ ghTotalRuntime hist )  -- Total runtime in seconds
          , ( 1
            , T.pack
              $ show
              $ if isFirst
                then 0
                else ghBootCount hist
            )  -- Boot count
          , ( 2, descriptSakuraName (ghostDescript ghost) )  -- Ghost name
          , ( 3, "" )  -- Shell name (empty for now)
          , ( 4, T.pack $ show hour )
          , ( 5, T.pack $ show minute )
          , ( 6, T.pack $ show sec )
          ]

  -- Send boot event using mkRequest helper
  let event
        = if isFirst
          then OnFirstBoot
          else OnBoot
      eventId = eventToId event
      req     = mkRequest "Kokage" eventId refs

  result <- try (sendRequest shiori req)
    :: IO (Either SomeException (Either String ShioriResponse))
  case result of
    Left err           -> putStrLn $ "[SHIORI] Boot error: " <> show err
    Right (Left err)   -> putStrLn $ "[SHIORI] Boot error: " <> err
    Right (Right resp) -> let
        val = fromMaybe "" (srsValue resp)
      in 
        putStrLn $ "[SHIORI] Boot response: " <> T.unpack val

  -- Update history
  let newHist
        = hist { ghBootCount = ghBootCount hist + 1
               , ghLastBoot  = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" zt
               }
  saveGhostHistory gPath newHist

  -- Suppress unused warning for day
  let _ = day

  return startTime

-- | Cleanup SHIORI with close event.
-- Sends OnClose and updates runtime history.
cleanupShiori :: WineShiori -> FilePath -> UTCTime -> IO ()
cleanupShiori shiori gPath startTime = do
  -- Calculate runtime
  endTime <- getCurrentTime
  let runtimeSeconds = floor (realToFrac (diffUTCTime endTime startTime) :: Double) :: Int

  -- Send OnClose using mkRequest helper
  let eventId = eventToId OnClose
      req     = mkRequest "Kokage" eventId (Map.fromList [ ( 0, "user" ) ])

  result <- try (sendRequest shiori req)
    :: IO (Either SomeException (Either String ShioriResponse))
  case result of
    Left err           -> putStrLn $ "[SHIORI] Close error: " <> show err
    Right (Left err)   -> putStrLn $ "[SHIORI] Close error: " <> err
    Right (Right resp) -> let
        val = fromMaybe "" (srsValue resp)
      in 
        putStrLn $ "[SHIORI] Close response: " <> T.unpack val

  -- Update history with runtime
  mHist <- loadGhostHistory gPath
  let hist    = fromMaybe defaultGhostHistory mHist
      newHist = hist { ghTotalRuntime = ghTotalRuntime hist + runtimeSeconds }
  saveGhostHistory gPath newHist
