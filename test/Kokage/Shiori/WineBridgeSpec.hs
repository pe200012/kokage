{-# LANGUAGE OverloadedStrings #-}

module Kokage.Shiori.WineBridgeSpec ( spec ) where

import           Test.Hspec
import           System.FilePath            ( (</>) )

import           Kokage.Shiori.WineBridge

-- | Main spec
spec :: Spec
spec = do
  describe "WineBridge" $ do
    configSpec
    pathConversionSpec

--------------------------------------------------------------------------------
-- Configuration Tests
--------------------------------------------------------------------------------

configSpec :: Spec
configSpec = describe "Configuration" $ do
  it "defaultWineBridgeConfig has correct wine command" $
    wbcWineCommand defaultWineBridgeConfig `shouldBe` "wine"

  it "defaultWineBridgeConfig has correct bridge path" $
    wbcBridgePath defaultWineBridgeConfig `shouldBe` ("wine-helper" </> "shiori_bridge.exe")

  it "defaultWineBridgeConfig has correct bridge .so path" $
    wbcBridgeSoPath defaultWineBridgeConfig `shouldBe` ("wine-helper" </> "shiori_bridge.exe.so")

  it "defaultWineBridgeConfig has correct sender name" $
    wbcSenderName defaultWineBridgeConfig `shouldBe` "Kokage"

  it "defaultWineBridgeConfig has 30 second timeout" $
    wbcTimeoutMs defaultWineBridgeConfig `shouldBe` 30000

--------------------------------------------------------------------------------
-- Path Conversion Tests
--------------------------------------------------------------------------------

pathConversionSpec :: Spec
pathConversionSpec = describe "toWinePath" $ do
  it "converts absolute Unix path to Z: drive path" $
    toWinePath "/home/user/ghost" `shouldBe` "Z:\\home\\user\\ghost"

  it "converts root path" $
    toWinePath "/" `shouldBe` "Z:\\"

  it "preserves relative paths without drive letter" $
    toWinePath "relative/path" `shouldBe` "relative\\path"

  it "converts nested paths correctly" $
    toWinePath "/mnt/data/ghosts/emily/ghost/master" 
      `shouldBe` "Z:\\mnt\\data\\ghosts\\emily\\ghost\\master"

  it "handles paths with multiple slashes" $
    toWinePath "/usr/local/share" `shouldBe` "Z:\\usr\\local\\share"

  it "handles single directory" $
    toWinePath "/tmp" `shouldBe` "Z:\\tmp"

  it "converts empty relative path" $
    toWinePath "" `shouldBe` ""

  it "handles path with spaces" $
    toWinePath "/home/user/My Documents/ghost" 
      `shouldBe` "Z:\\home\\user\\My Documents\\ghost"
