{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Kokage ( kokageMain, KokageConfig(..), defaultConfig, loadLastGhost )

main :: IO ()
main = do
  -- Get default configuration
  config <- defaultConfig
  
  -- Load last ghost from persistent storage
  mLastGhost <- loadLastGhost config
  
  -- Run Kokage with loaded configuration
  -- The ghost path can be overridden via command line args in the future
  kokageMain config
    { configLastGhost = mLastGhost
    -- Uncomment to explicitly specify a ghost:
    -- , configGhostPath = Just "/path/to/ghost"
    , configSurfaceId = 0  -- Default surface
    }
