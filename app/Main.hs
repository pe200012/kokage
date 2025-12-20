{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Kokage ( kokageMain, KokageConfig(..), defaultConfig )

main :: IO ()
main = kokageMain defaultConfig
  { configGhostPath = "test-fdr/ghost/emily4"
  , configSurfaceId = 80  -- Surface with collision regions
  }
