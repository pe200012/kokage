{-# LANGUAGE OverloadedStrings #-}

module Types.InstallSpec ( spec ) where

import           Test.Hspec
import           System.IO.Temp             ( withSystemTempDirectory )
import           System.FilePath            ( (</>) )
import qualified Data.Text.IO               as TIO
import qualified Data.Map.Strict            as Map

import           Types.Install

-- | Main spec
spec :: Spec
spec = do
  describe "Install Types" $ do
    installTypeSpec
    emptyInstallSpec
    installDescriptSpec
    deleteListSpec
    developerOptionsSpec

--------------------------------------------------------------------------------
-- InstallType Parsing
--------------------------------------------------------------------------------

installTypeSpec :: Spec
installTypeSpec = describe "parseInstallType" $ do
  it "parses 'ghost'" $
    parseInstallType "ghost" `shouldBe` InstallGhost

  it "parses 'shell'" $
    parseInstallType "shell" `shouldBe` InstallShell

  it "parses 'supplement'" $
    parseInstallType "supplement" `shouldBe` InstallSupplement

  it "parses 'balloon'" $
    parseInstallType "balloon" `shouldBe` InstallBalloon

  it "parses 'plugin'" $
    parseInstallType "plugin" `shouldBe` InstallPlugin

  it "parses 'headline'" $
    parseInstallType "headline" `shouldBe` InstallHeadline

  it "parses 'calendar'" $
    parseInstallType "calendar" `shouldBe` InstallCalendar

  it "parses 'calendarskin'" $
    parseInstallType "calendarskin" `shouldBe` InstallCalendarSkin

  it "parses 'package'" $
    parseInstallType "package" `shouldBe` InstallPackage

  it "handles case insensitivity" $
    parseInstallType "GHOST" `shouldBe` InstallGhost

  it "handles whitespace" $
    parseInstallType "  ghost  " `shouldBe` InstallGhost

  it "returns Unknown for unrecognized types" $
    parseInstallType "foobar" `shouldBe` InstallUnknown "foobar"

--------------------------------------------------------------------------------
-- Empty Install Defaults
--------------------------------------------------------------------------------

emptyInstallSpec :: Spec
emptyInstallSpec = describe "emptyInstallDescript" $ do
  it "has empty name" $
    idName emptyInstallDescript `shouldBe` ""

  it "has empty directory" $
    idDirectory emptyInstallDescript `shouldBe` ""

  it "has Ghost as default type" $
    idType emptyInstallDescript `shouldBe` InstallGhost

  it "has refresh disabled by default" $
    idRefresh emptyInstallDescript `shouldBe` False

  it "has empty refreshUndeleteMask" $
    idRefreshUndeleteMask emptyInstallDescript `shouldBe` []

  it "has no bundled directories by default" $ do
    idBalloonDirectory emptyInstallDescript `shouldBe` Nothing
    idPluginDirectory emptyInstallDescript `shouldBe` Nothing
    idHeadlineDirectory emptyInstallDescript `shouldBe` Nothing

--------------------------------------------------------------------------------
-- InstallDescript Parsing
--------------------------------------------------------------------------------

installDescriptSpec :: Spec
installDescriptSpec = describe "readInstallDescript" $ do
  it "parses required fields" $ withSystemTempDirectory "install-test" $ \tmpDir -> do
    let testFile = tmpDir </> "install.txt"
    TIO.writeFile testFile "charset,UTF-8\nname,TestGhost\ntype,ghost\ndirectory,TestGhost\n"
    inst <- readInstallDescript testFile
    idCharset inst `shouldBe` "UTF-8"
    idName inst `shouldBe` "TestGhost"
    idType inst `shouldBe` InstallGhost
    idDirectory inst `shouldBe` "TestGhost"

  it "parses shell with accept field" $ withSystemTempDirectory "install-test" $ \tmpDir -> do
    let testFile = tmpDir </> "install.txt"
    TIO.writeFile testFile "charset,UTF-8\nname,MasterShell\ntype,shell\ndirectory,master\naccept,SomeGhost\n"
    inst <- readInstallDescript testFile
    idType inst `shouldBe` InstallShell
    idAccept inst `shouldBe` Just "SomeGhost"

  it "parses refresh as true with '1'" $ withSystemTempDirectory "install-test" $ \tmpDir -> do
    let testFile = tmpDir </> "install.txt"
    TIO.writeFile testFile "name,Test\ntype,ghost\ndirectory,test\nrefresh,1\n"
    inst <- readInstallDescript testFile
    idRefresh inst `shouldBe` True

  it "parses refresh as true with 'true'" $ withSystemTempDirectory "install-test" $ \tmpDir -> do
    let testFile = tmpDir </> "install.txt"
    TIO.writeFile testFile "name,Test\ntype,ghost\ndirectory,test\nrefresh,true\n"
    inst <- readInstallDescript testFile
    idRefresh inst `shouldBe` True

  it "parses refresh as false for other values" $ withSystemTempDirectory "install-test" $ \tmpDir -> do
    let testFile = tmpDir </> "install.txt"
    TIO.writeFile testFile "name,Test\ntype,ghost\ndirectory,test\nrefresh,0\n"
    inst <- readInstallDescript testFile
    idRefresh inst `shouldBe` False

  it "parses refreshundeletemask" $ withSystemTempDirectory "install-test" $ \tmpDir -> do
    let testFile = tmpDir </> "install.txt"
    TIO.writeFile testFile "name,Test\ntype,ghost\ndirectory,test\nrefreshundeletemask,profile\\*:ghost\\master\\*\n"
    inst <- readInstallDescript testFile
    idRefreshUndeleteMask inst `shouldBe` ["profile\\*", "ghost\\master\\*"]

  it "parses bundled balloon directory" $ withSystemTempDirectory "install-test" $ \tmpDir -> do
    let testFile = tmpDir </> "install.txt"
    TIO.writeFile testFile "name,Test\ntype,ghost\ndirectory,test\nballoon.directory,MyBalloon\n"
    inst <- readInstallDescript testFile
    idBalloonDirectory inst `shouldBe` Just "MyBalloon"

  it "parses bundled balloon with source directory" $ withSystemTempDirectory "install-test" $ \tmpDir -> do
    let testFile = tmpDir </> "install.txt"
    TIO.writeFile testFile "name,Test\ntype,ghost\ndirectory,test\nballoon.directory,MyBalloon\nballoon.source.directory,bundled_balloon\n"
    inst <- readInstallDescript testFile
    idBalloonDirectory inst `shouldBe` Just "MyBalloon"
    idBalloonSourceDirectory inst `shouldBe` Just "bundled_balloon"

  it "parses bundled plugin directory" $ withSystemTempDirectory "install-test" $ \tmpDir -> do
    let testFile = tmpDir </> "install.txt"
    TIO.writeFile testFile "name,Test\ntype,ghost\ndirectory,test\nplugin.directory,MyPlugin\n"
    inst <- readInstallDescript testFile
    idPluginDirectory inst `shouldBe` Just "MyPlugin"

  it "parses bundled headline directory" $ withSystemTempDirectory "install-test" $ \tmpDir -> do
    let testFile = tmpDir </> "install.txt"
    TIO.writeFile testFile "name,Test\ntype,ghost\ndirectory,test\nheadline.directory,MyHeadline\n"
    inst <- readInstallDescript testFile
    idHeadlineDirectory inst `shouldBe` Just "MyHeadline"

  it "parses bundled calendar directories" $ withSystemTempDirectory "install-test" $ \tmpDir -> do
    let testFile = tmpDir </> "install.txt"
    TIO.writeFile testFile "name,Test\ntype,ghost\ndirectory,test\ncalendar.directory,MyCal\ncalendarskin.directory,MySkin\n"
    inst <- readInstallDescript testFile
    idCalendarDirectory inst `shouldBe` Just "MyCal"
    idCalendarSkinDirectory inst `shouldBe` Just "MySkin"

  it "handles case-insensitive keys" $ withSystemTempDirectory "install-test" $ \tmpDir -> do
    let testFile = tmpDir </> "install.txt"
    TIO.writeFile testFile "NAME,TestGhost\nTYPE,ghost\nDIRECTORY,test\n"
    inst <- readInstallDescript testFile
    idName inst `shouldBe` "TestGhost"
    idType inst `shouldBe` InstallGhost
    idDirectory inst `shouldBe` "test"

  it "stores raw key-value pairs" $ withSystemTempDirectory "install-test" $ \tmpDir -> do
    let testFile = tmpDir </> "install.txt"
    TIO.writeFile testFile "name,Test\ntype,ghost\ndirectory,test\ncustom.field,custom value\n"
    inst <- readInstallDescript testFile
    Map.lookup "custom.field" (idRaw inst) `shouldBe` Just "custom value"

--------------------------------------------------------------------------------
-- DeleteList Parsing
--------------------------------------------------------------------------------

deleteListSpec :: Spec
deleteListSpec = describe "readDeleteList" $ do
  it "parses file paths" $ withSystemTempDirectory "delete-test" $ \tmpDir -> do
    let testFile = tmpDir </> "delete.txt"
    TIO.writeFile testFile "ghost\\master\\old.dll\nshell\\master\\obsolete.png\n"
    dl <- readDeleteList testFile
    dl `shouldBe` ["ghost/master/old.dll", "shell/master/obsolete.png"]

  it "parses directory paths (ending with backslash)" $ withSystemTempDirectory "delete-test" $ \tmpDir -> do
    let testFile = tmpDir </> "delete.txt"
    TIO.writeFile testFile "ghost\\master\\olddir\\\n"
    dl <- readDeleteList testFile
    dl `shouldBe` ["ghost/master/olddir/"]

  it "normalizes backslashes to forward slashes" $ withSystemTempDirectory "delete-test" $ \tmpDir -> do
    let testFile = tmpDir </> "delete.txt"
    TIO.writeFile testFile "path\\to\\file.txt\n"
    dl <- readDeleteList testFile
    dl `shouldBe` ["path/to/file.txt"]

  it "strips whitespace" $ withSystemTempDirectory "delete-test" $ \tmpDir -> do
    let testFile = tmpDir </> "delete.txt"
    TIO.writeFile testFile "  file1.txt  \n  file2.txt\n"
    dl <- readDeleteList testFile
    dl `shouldBe` ["file1.txt", "file2.txt"]

  it "filters empty lines" $ withSystemTempDirectory "delete-test" $ \tmpDir -> do
    let testFile = tmpDir </> "delete.txt"
    TIO.writeFile testFile "file1.txt\n\n\nfile2.txt\n"
    dl <- readDeleteList testFile
    dl `shouldBe` ["file1.txt", "file2.txt"]

  it "handles empty file" $ withSystemTempDirectory "delete-test" $ \tmpDir -> do
    let testFile = tmpDir </> "delete.txt"
    TIO.writeFile testFile ""
    dl <- readDeleteList testFile
    dl `shouldBe` []

--------------------------------------------------------------------------------
-- DeveloperOptions Parsing
--------------------------------------------------------------------------------

developerOptionsSpec :: Spec
developerOptionsSpec = describe "readDeveloperOptions" $ do
  it "parses nonar option" $ withSystemTempDirectory "devopt-test" $ \tmpDir -> do
    let testFile = tmpDir </> "developer_options.txt"
    TIO.writeFile testFile "ghost/master/debug.dll,nonar\n"
    opts <- readDeveloperOptions testFile
    Map.lookup "ghost/master/debug.dll" opts `shouldBe` Just [NoNar]

  it "parses noupdate option" $ withSystemTempDirectory "devopt-test" $ \tmpDir -> do
    let testFile = tmpDir </> "developer_options.txt"
    TIO.writeFile testFile "profile/settings.cfg,noupdate\n"
    opts <- readDeveloperOptions testFile
    Map.lookup "profile/settings.cfg" opts `shouldBe` Just [NoUpdate]

  it "parses multiple options" $ withSystemTempDirectory "devopt-test" $ \tmpDir -> do
    let testFile = tmpDir </> "developer_options.txt"
    TIO.writeFile testFile "test/file.dll,nonar,noupdate\n"
    opts <- readDeveloperOptions testFile
    Map.lookup "test/file.dll" opts `shouldBe` Just [NoNar, NoUpdate]

  it "parses directory with trailing slash" $ withSystemTempDirectory "devopt-test" $ \tmpDir -> do
    let testFile = tmpDir </> "developer_options.txt"
    TIO.writeFile testFile "debug/,nonar\n"
    opts <- readDeveloperOptions testFile
    Map.lookup "debug/" opts `shouldBe` Just [NoNar]

  it "parses multiple lines" $ withSystemTempDirectory "devopt-test" $ \tmpDir -> do
    let testFile = tmpDir </> "developer_options.txt"
    TIO.writeFile testFile "file1.txt,nonar\nfile2.txt,noupdate\n"
    opts <- readDeveloperOptions testFile
    Map.lookup "file1.txt" opts `shouldBe` Just [NoNar]
    Map.lookup "file2.txt" opts `shouldBe` Just [NoUpdate]

  it "handles case insensitive options" $ withSystemTempDirectory "devopt-test" $ \tmpDir -> do
    let testFile = tmpDir </> "developer_options.txt"
    TIO.writeFile testFile "file.txt,NONAR,NoUpdate\n"
    opts <- readDeveloperOptions testFile
    Map.lookup "file.txt" opts `shouldBe` Just [NoNar, NoUpdate]

  it "ignores unknown options" $ withSystemTempDirectory "devopt-test" $ \tmpDir -> do
    let testFile = tmpDir </> "developer_options.txt"
    TIO.writeFile testFile "file.txt,nonar,unknownoption\n"
    opts <- readDeveloperOptions testFile
    Map.lookup "file.txt" opts `shouldBe` Just [NoNar]

  it "ignores lines with no valid options" $ withSystemTempDirectory "devopt-test" $ \tmpDir -> do
    let testFile = tmpDir </> "developer_options.txt"
    TIO.writeFile testFile "file.txt,unknownoption\n"
    opts <- readDeveloperOptions testFile
    Map.lookup "file.txt" opts `shouldBe` Nothing

  it "handles empty file" $ withSystemTempDirectory "devopt-test" $ \tmpDir -> do
    let testFile = tmpDir </> "developer_options.txt"
    TIO.writeFile testFile ""
    opts <- readDeveloperOptions testFile
    opts `shouldBe` Map.empty
