{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Kokage ( kokageMain, KokageConfig(..), defaultConfig, loadLastGhost )

import System.IO ( hSetBuffering, stdout, BufferMode(..) )

main :: IO ()
main = do
  -- Set line buffering for stdout to ensure logs appear immediately
  hSetBuffering stdout LineBuffering
  
  -- Get default configuration (uses cwd for ghost/balloon discovery)
  config <- defaultConfig
  
  -- Load last ghost from persistent storage
  mLastGhost <- loadLastGhost config
  
  -- Run Kokage with loaded configuration
  -- Ghost and balloon are auto-discovered from cwd/ghost and cwd/balloon
  kokageMain config
    { configLastGhost = mLastGhost
    , configSurfaceId = 0  -- Default surface
    }
