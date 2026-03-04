{-# LANGUAGE OverloadedStrings #-}

module Kokage.Balloon.LayoutSpec ( spec ) where

import           Kokage.Balloon ( BalloonConfig(..), configFromDescript )

import           Prelude        ()

import           Relude

import           Test.Hspec

import           Types.Balloon  ( BalloonDescript(..), emptyBalloonDescript, readBalloonDescript )

spec :: Spec
spec = describe "Balloon text area / wrap calculation" $ do
  it "matches SSP default balloon (origin + wordwrappoint.x + validrect.bottom)" $ do
    bd <- readBalloonDescript "test-fdr/balloon/ssp/descript.txt"
    let cfg = configFromDescript bd 326 96

    bcOriginX cfg `shouldBe` 14
    bcOriginY cfg `shouldBe` 14
    bcValidWidth cfg `shouldBe` 282
    bcValidHeight cfg `shouldBe` 70

  it "falls back to validrect.* when origin.* is missing" $ do
    let bd
          = emptyBalloonDescript
          { bdOriginX         = Nothing
          , bdOriginY         = Nothing
          , bdValidRectLeft   = Just 14
          , bdValidRectTop    = Just 14
          , bdValidRectRight  = Just (-14)
          , bdValidRectBottom = Just (-14)
          , bdWordWrapPointX  = Nothing
          }
    let cfg = configFromDescript bd 320 200

    bcOriginX cfg `shouldBe` 14
    bcOriginY cfg `shouldBe` 14
    bcValidWidth cfg `shouldBe` 292
    bcValidHeight cfg `shouldBe` 172

  it "interprets positive wordwrappoint.x as absolute X coordinate" $ do
    let bd
          = emptyBalloonDescript
          { bdOriginX = Just 14, bdOriginY = Just 14, bdWordWrapPointX = Just 200 }
    let cfg = configFromDescript bd 320 200

    bcValidWidth cfg `shouldBe` (200 - 14)
