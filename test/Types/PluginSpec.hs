{-# LANGUAGE OverloadedStrings #-}

module Types.PluginSpec ( spec ) where

import           Test.Hspec
import           System.IO.Temp             ( withSystemTempDirectory )
import           System.FilePath            ( (</>) )
import qualified Data.Text.IO               as TIO

import           Types.Plugin

-- | Main spec
spec :: Spec
spec = do
  describe "Plugin Descript Parser" $ do
    emptyPluginSpec
    basicParsingSpec
    otherGhostTalkSpec
    secondChangeIntervalSpec

--------------------------------------------------------------------------------
-- Empty Plugin Defaults
--------------------------------------------------------------------------------

emptyPluginSpec :: Spec
emptyPluginSpec = describe "emptyPluginDescript" $ do
  it "has empty name" $
    pdName emptyPluginDescript `shouldBe` ""

  it "has empty id" $
    pdId emptyPluginDescript `shouldBe` ""

  it "has empty filename" $
    pdFilename emptyPluginDescript `shouldBe` ""

  it "has type 'plugin'" $
    pdType emptyPluginDescript `shouldBe` "plugin"

  it "has default readme filename" $
    pdReadme emptyPluginDescript `shouldBe` "readme.txt"

  it "has default secondChangeInterval of 1" $
    pdSecondChangeInterval emptyPluginDescript `shouldBe` 1

  it "has otherGhostTalk disabled by default" $
    pdOtherGhostTalk emptyPluginDescript `shouldBe` OtherGhostTalkDisabled

--------------------------------------------------------------------------------
-- Basic Parsing
--------------------------------------------------------------------------------

basicParsingSpec :: Spec
basicParsingSpec = describe "basic parsing" $ do
  it "parses required fields" $ withSystemTempDirectory "plugin-test" $ \tmpDir -> do
    let testFile = tmpDir </> "descript.txt"
    TIO.writeFile testFile "charset,UTF-8\nname,TestPlugin\nid,12345678-1234-1234-1234-123456789ABC\nfilename,test.dll\n"
    pd <- readPluginDescript testFile
    pdCharset pd `shouldBe` "UTF-8"
    pdName pd `shouldBe` "TestPlugin"
    pdId pd `shouldBe` "12345678-1234-1234-1234-123456789ABC"
    pdFilename pd `shouldBe` "test.dll"

  it "parses author metadata" $ withSystemTempDirectory "plugin-test" $ \tmpDir -> do
    let testFile = tmpDir </> "descript.txt"
    TIO.writeFile testFile "name,Test\nid,TEST-ID\nfilename,t.dll\ncraftman,JohnDoe\ncraftmanw,山田太郎\ncraftmanurl,http://example.com\n"
    pd <- readPluginDescript testFile
    pdCraftman pd `shouldBe` Just "JohnDoe"
    pdCraftmanw pd `shouldBe` Just "山田太郎"
    pdCraftmanUrl pd `shouldBe` Just "http://example.com"

  it "parses homeurl" $ withSystemTempDirectory "plugin-test" $ \tmpDir -> do
    let testFile = tmpDir </> "descript.txt"
    TIO.writeFile testFile "name,Test\nid,TEST-ID\nfilename,t.dll\nhomeurl,http://update.example.com/plugin\n"
    pd <- readPluginDescript testFile
    pdHomeUrl pd `shouldBe` Just "http://update.example.com/plugin"

  it "parses readme settings" $ withSystemTempDirectory "plugin-test" $ \tmpDir -> do
    let testFile = tmpDir </> "descript.txt"
    TIO.writeFile testFile "name,Test\nid,TEST-ID\nfilename,t.dll\nreadme,INSTALL.txt\nreadme.charset,Shift_JIS\n"
    pd <- readPluginDescript testFile
    pdReadme pd `shouldBe` "INSTALL.txt"
    pdReadmeCharset pd `shouldBe` Just "Shift_JIS"

  it "parses type field" $ withSystemTempDirectory "plugin-test" $ \tmpDir -> do
    let testFile = tmpDir </> "descript.txt"
    TIO.writeFile testFile "name,Test\nid,TEST-ID\nfilename,t.dll\ntype,plugin\n"
    pd <- readPluginDescript testFile
    pdType pd `shouldBe` "plugin"

  it "handles case-insensitive keys" $ withSystemTempDirectory "plugin-test" $ \tmpDir -> do
    let testFile = tmpDir </> "descript.txt"
    TIO.writeFile testFile "NAME,TestPlugin\nID,TEST-ID\nFILENAME,test.dll\n"
    pd <- readPluginDescript testFile
    pdName pd `shouldBe` "TestPlugin"
    pdId pd `shouldBe` "TEST-ID"
    pdFilename pd `shouldBe` "test.dll"

--------------------------------------------------------------------------------
-- OtherGhostTalk Option Parsing
--------------------------------------------------------------------------------

otherGhostTalkSpec :: Spec
otherGhostTalkSpec = describe "otherghosttalk option" $ do
  it "parses 'false' as disabled" $ withSystemTempDirectory "plugin-test" $ \tmpDir -> do
    let testFile = tmpDir </> "descript.txt"
    TIO.writeFile testFile "name,Test\nid,TEST-ID\nfilename,t.dll\notherghosttalk,false\n"
    pd <- readPluginDescript testFile
    pdOtherGhostTalk pd `shouldBe` OtherGhostTalkDisabled

  it "parses '0' as disabled" $ withSystemTempDirectory "plugin-test" $ \tmpDir -> do
    let testFile = tmpDir </> "descript.txt"
    TIO.writeFile testFile "name,Test\nid,TEST-ID\nfilename,t.dll\notherghosttalk,0\n"
    pd <- readPluginDescript testFile
    pdOtherGhostTalk pd `shouldBe` OtherGhostTalkDisabled

  it "parses 'true' as enabled" $ withSystemTempDirectory "plugin-test" $ \tmpDir -> do
    let testFile = tmpDir </> "descript.txt"
    TIO.writeFile testFile "name,Test\nid,TEST-ID\nfilename,t.dll\notherghosttalk,true\n"
    pd <- readPluginDescript testFile
    pdOtherGhostTalk pd `shouldBe` OtherGhostTalkAfter

  it "parses '1' as enabled" $ withSystemTempDirectory "plugin-test" $ \tmpDir -> do
    let testFile = tmpDir </> "descript.txt"
    TIO.writeFile testFile "name,Test\nid,TEST-ID\nfilename,t.dll\notherghosttalk,1\n"
    pd <- readPluginDescript testFile
    pdOtherGhostTalk pd `shouldBe` OtherGhostTalkAfter

  it "parses 'after' as after" $ withSystemTempDirectory "plugin-test" $ \tmpDir -> do
    let testFile = tmpDir </> "descript.txt"
    TIO.writeFile testFile "name,Test\nid,TEST-ID\nfilename,t.dll\notherghosttalk,after\n"
    pd <- readPluginDescript testFile
    pdOtherGhostTalk pd `shouldBe` OtherGhostTalkAfter

  it "parses 'before' as before" $ withSystemTempDirectory "plugin-test" $ \tmpDir -> do
    let testFile = tmpDir </> "descript.txt"
    TIO.writeFile testFile "name,Test\nid,TEST-ID\nfilename,t.dll\notherghosttalk,before\n"
    pd <- readPluginDescript testFile
    pdOtherGhostTalk pd `shouldBe` OtherGhostTalkBefore

--------------------------------------------------------------------------------
-- SecondChangeInterval Parsing
--------------------------------------------------------------------------------

secondChangeIntervalSpec :: Spec
secondChangeIntervalSpec = describe "secondchangeinterval" $ do
  it "parses integer value" $ withSystemTempDirectory "plugin-test" $ \tmpDir -> do
    let testFile = tmpDir </> "descript.txt"
    TIO.writeFile testFile "name,Test\nid,TEST-ID\nfilename,t.dll\nsecondchangeinterval,5\n"
    pd <- readPluginDescript testFile
    pdSecondChangeInterval pd `shouldBe` 5

  it "parses 0 to disable notifications" $ withSystemTempDirectory "plugin-test" $ \tmpDir -> do
    let testFile = tmpDir </> "descript.txt"
    TIO.writeFile testFile "name,Test\nid,TEST-ID\nfilename,t.dll\nsecondchangeinterval,0\n"
    pd <- readPluginDescript testFile
    pdSecondChangeInterval pd `shouldBe` 0

  it "defaults to 1 for invalid values" $ withSystemTempDirectory "plugin-test" $ \tmpDir -> do
    let testFile = tmpDir </> "descript.txt"
    TIO.writeFile testFile "name,Test\nid,TEST-ID\nfilename,t.dll\nsecondchangeinterval,invalid\n"
    pd <- readPluginDescript testFile
    pdSecondChangeInterval pd `shouldBe` 1
