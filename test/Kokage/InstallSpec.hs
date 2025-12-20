{-# LANGUAGE OverloadedStrings #-}

module Kokage.InstallSpec ( spec ) where

import           Test.Hspec
import           System.IO.Temp             ( withSystemTempDirectory )
import           System.FilePath            ( (</>) )
import           System.Directory           ( doesDirectoryExist, doesFileExist )
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Codec.Archive.Zip          ( Archive, emptyArchive, addEntryToArchive, toEntry, fromArchive )

import           Kokage.Install
import           Types.Install              ( InstallType(..) )

-- | Main spec
spec :: Spec
spec = do
  describe "NAR Installation" $ do
    baseDirSpec
    installGhostSpec
    installBalloonSpec
    installShellSpec
    errorHandlingSpec

--------------------------------------------------------------------------------
-- BaseDir Tests
--------------------------------------------------------------------------------

baseDirSpec :: Spec
baseDirSpec = describe "getInstallDir" $ do
  let base = BaseDir
        { bdGhost = "/ghost"
        , bdBalloon = "/balloon"
        , bdPlugin = "/plugin"
        , bdHeadline = "/headline"
        , bdCalendar = "/calendar"
        , bdCalendarSkin = "/calendar/skin"
        }

  it "returns ghost directory for InstallGhost" $
    getInstallDir base InstallGhost "TestGhost" `shouldBe` "/ghost/TestGhost"

  it "returns ghost directory for InstallShell" $
    getInstallDir base InstallShell "master" `shouldBe` "/ghost/master"

  it "returns balloon directory for InstallBalloon" $
    getInstallDir base InstallBalloon "TestBalloon" `shouldBe` "/balloon/TestBalloon"

  it "returns plugin directory for InstallPlugin" $
    getInstallDir base InstallPlugin "TestPlugin" `shouldBe` "/plugin/TestPlugin"

  it "returns headline directory for InstallHeadline" $
    getInstallDir base InstallHeadline "TestHeadline" `shouldBe` "/headline/TestHeadline"

  it "returns calendar directory for InstallCalendar" $
    getInstallDir base InstallCalendar "TestCalendar" `shouldBe` "/calendar/TestCalendar"

  it "returns empty for InstallPackage" $
    getInstallDir base InstallPackage "pkg" `shouldBe` ""

--------------------------------------------------------------------------------
-- Ghost Installation Tests
--------------------------------------------------------------------------------

installGhostSpec :: Spec
installGhostSpec = describe "installNar for ghost" $ do
  it "installs a simple ghost NAR" $ withSystemTempDirectory "nar-test" $ \tmpDir -> do
    let narPath = tmpDir </> "test.nar"
        baseDir = mkBaseDir tmpDir
    
    -- Create a test NAR
    let installTxt = "charset,UTF-8\nname,TestGhost\ntype,ghost\ndirectory,TestGhost\n"
        ghostFile = "ghost/master/shiori.dll"
        ghostContent = "dummy shiori content"
        archive = createTestNar
          [ ("install.txt", installTxt)
          , (ghostFile, ghostContent)
          ]
    BL.writeFile narPath (fromArchive archive)
    
    -- Install the NAR
    result <- installNar baseDir narPath
    
    -- Verify result
    case result of
      InstallSuccess name itype path _ -> do
        name `shouldBe` "TestGhost"
        itype `shouldBe` InstallGhost
        path `shouldBe` (tmpDir </> "ghost" </> "TestGhost")
        
        -- Verify files were extracted
        ghostExists <- doesFileExist (path </> ghostFile)
        ghostExists `shouldBe` True
        
      InstallFailure err ->
        expectationFailure $ "Installation failed: " <> T.unpack err

  it "fails without install.txt" $ withSystemTempDirectory "nar-test" $ \tmpDir -> do
    let narPath = tmpDir </> "test.nar"
        baseDir = mkBaseDir tmpDir
    
    -- Create a NAR without install.txt
    let archive = createTestNar [("somefile.txt", "content")]
    BL.writeFile narPath (fromArchive archive)
    
    result <- installNar baseDir narPath
    case result of
      InstallFailure err -> err `textShouldContain` "No install.txt"
      InstallSuccess {} -> expectationFailure "Should have failed"

  it "fails with missing name field" $ withSystemTempDirectory "nar-test" $ \tmpDir -> do
    let narPath = tmpDir </> "test.nar"
        baseDir = mkBaseDir tmpDir
    
    let installTxt = "charset,UTF-8\ntype,ghost\ndirectory,TestGhost\n"
        archive = createTestNar [("install.txt", installTxt)]
    BL.writeFile narPath (fromArchive archive)
    
    result <- installNar baseDir narPath
    case result of
      InstallFailure err -> err `textShouldContain` "name"
      InstallSuccess {} -> expectationFailure "Should have failed"

--------------------------------------------------------------------------------
-- Balloon Installation Tests
--------------------------------------------------------------------------------

installBalloonSpec :: Spec
installBalloonSpec = describe "installNar for balloon" $ do
  it "installs a balloon NAR" $ withSystemTempDirectory "nar-test" $ \tmpDir -> do
    let narPath = tmpDir </> "balloon.nar"
        baseDir = mkBaseDir tmpDir
    
    let installTxt = "charset,UTF-8\nname,TestBalloon\ntype,balloon\ndirectory,TestBalloon\n"
        archive = createTestNar
          [ ("install.txt", installTxt)
          , ("descript.txt", "name,TestBalloon")
          , ("balloons0.png", "png data")
          ]
    BL.writeFile narPath (fromArchive archive)
    
    result <- installNar baseDir narPath
    case result of
      InstallSuccess name itype path _ -> do
        name `shouldBe` "TestBalloon"
        itype `shouldBe` InstallBalloon
        path `shouldBe` (tmpDir </> "balloon" </> "TestBalloon")
        
        -- Verify balloon directory was created
        dirExists <- doesDirectoryExist path
        dirExists `shouldBe` True
        
      InstallFailure err ->
        expectationFailure $ "Installation failed: " <> T.unpack err

--------------------------------------------------------------------------------
-- Shell Installation Tests
--------------------------------------------------------------------------------

installShellSpec :: Spec
installShellSpec = describe "installNar for shell" $ do
  it "installs a shell NAR" $ withSystemTempDirectory "nar-test" $ \tmpDir -> do
    let narPath = tmpDir </> "shell.nar"
        baseDir = mkBaseDir tmpDir
    
    let installTxt = "charset,UTF-8\nname,MasterShell\ntype,shell\ndirectory,master\naccept,SomeGhost\n"
        archive = createTestNar
          [ ("install.txt", installTxt)
          , ("descript.txt", "name,MasterShell")
          , ("surface0.png", "png data")
          , ("surfaces.txt", "surface0 {}") 
          ]
    BL.writeFile narPath (fromArchive archive)
    
    result <- installNar baseDir narPath
    case result of
      InstallSuccess name itype _path _ -> do
        name `shouldBe` "MasterShell"
        itype `shouldBe` InstallShell
        
      InstallFailure err ->
        expectationFailure $ "Installation failed: " <> T.unpack err

--------------------------------------------------------------------------------
-- Error Handling Tests
--------------------------------------------------------------------------------

errorHandlingSpec :: Spec
errorHandlingSpec = describe "error handling" $ do
  it "fails gracefully for non-existent NAR" $ withSystemTempDirectory "nar-test" $ \tmpDir -> do
    let narPath = tmpDir </> "nonexistent.nar"
        baseDir = mkBaseDir tmpDir
    
    result <- installNar baseDir narPath
    case result of
      InstallFailure err -> err `textShouldContain` "not found"
      InstallSuccess {} -> expectationFailure "Should have failed"

  it "fails for missing directory field when required" $ withSystemTempDirectory "nar-test" $ \tmpDir -> do
    let narPath = tmpDir </> "test.nar"
        baseDir = mkBaseDir tmpDir
    
    let installTxt = "charset,UTF-8\nname,TestGhost\ntype,ghost\n"  -- No directory!
        archive = createTestNar [("install.txt", installTxt)]
    BL.writeFile narPath (fromArchive archive)
    
    result <- installNar baseDir narPath
    case result of
      InstallFailure err -> err `textShouldContain` "directory"
      InstallSuccess {} -> expectationFailure "Should have failed"

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Create a BaseDir in a temp directory
mkBaseDir :: FilePath -> BaseDir
mkBaseDir tmpDir = BaseDir
  { bdGhost = tmpDir </> "ghost"
  , bdBalloon = tmpDir </> "balloon"
  , bdPlugin = tmpDir </> "plugin"
  , bdHeadline = tmpDir </> "headline"
  , bdCalendar = tmpDir </> "calendar"
  , bdCalendarSkin = tmpDir </> "calendar" </> "skin"
  }

-- | Create a test NAR archive with given files
createTestNar :: [(FilePath, String)] -> Archive
createTestNar files = foldr addFile emptyArchive files
  where
    addFile (path, content) arch =
      let entry = toEntry path 0 (BL.fromStrict $ TE.encodeUtf8 $ T.pack content)
      in addEntryToArchive entry arch

-- | Helper for shouldContain with Text
textShouldContain :: T.Text -> T.Text -> Expectation
textShouldContain actual expected =
  if expected `T.isInfixOf` actual
    then pure ()
    else expectationFailure $ 
           "Expected '" <> T.unpack actual <> "' to contain '" <> T.unpack expected <> "'"
