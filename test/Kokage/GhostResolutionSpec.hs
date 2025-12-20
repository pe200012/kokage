{-# LANGUAGE OverloadedStrings #-}

module Kokage.GhostResolutionSpec ( spec ) where

import           Test.Hspec
import           System.IO.Temp             ( withSystemTempDirectory )
import           System.FilePath            ( (</>) )
import           System.Directory           ( createDirectoryIfMissing
                                            , doesFileExist
                                            )
import qualified Data.Text.IO               as TIO

import           Kokage                     ( scanGhosts
                                            , resolveGhost
                                            , saveLastGhost
                                            , loadLastGhost
                                            , KokageConfig(..)
                                            )
import           Kokage.Install             ( BaseDir(..) )

-- | Main spec
spec :: Spec
spec = do
  describe "Ghost Resolution" $ do
    scanGhostsSpec
    resolveGhostSpec
    lastGhostPersistenceSpec

--------------------------------------------------------------------------------
-- scanGhosts Tests
--------------------------------------------------------------------------------

scanGhostsSpec :: Spec
scanGhostsSpec = describe "scanGhosts" $ do
  it "returns empty list for non-existent ghost directory" $ 
    withSystemTempDirectory "kokage-test" $ \tmpDir -> do
      let baseDir = mkBaseDir tmpDir
      -- Don't create ghost directory
      ghosts <- scanGhosts baseDir
      ghosts `shouldBe` []

  it "returns empty list for empty ghost directory" $ 
    withSystemTempDirectory "kokage-test" $ \tmpDir -> do
      let baseDir = mkBaseDir tmpDir
      createDirectoryIfMissing True (bdGhost baseDir)
      ghosts <- scanGhosts baseDir
      ghosts `shouldBe` []

  it "finds valid ghost directories" $ 
    withSystemTempDirectory "kokage-test" $ \tmpDir -> do
      let baseDir = mkBaseDir tmpDir
      -- Create two valid ghosts
      createValidGhost (bdGhost baseDir </> "ghost_a")
      createValidGhost (bdGhost baseDir </> "ghost_b")
      
      ghosts <- scanGhosts baseDir
      length ghosts `shouldBe` 2

  it "returns sorted list of ghosts" $ 
    withSystemTempDirectory "kokage-test" $ \tmpDir -> do
      let baseDir = mkBaseDir tmpDir
      -- Create ghosts in non-alphabetical order
      createValidGhost (bdGhost baseDir </> "zebra")
      createValidGhost (bdGhost baseDir </> "alpha")
      createValidGhost (bdGhost baseDir </> "middle")
      
      ghosts <- scanGhosts baseDir
      map takeBaseName ghosts `shouldBe` ["alpha", "middle", "zebra"]

  it "ignores directories without ghost/master structure" $ 
    withSystemTempDirectory "kokage-test" $ \tmpDir -> do
      let baseDir = mkBaseDir tmpDir
      -- Create a valid ghost
      createValidGhost (bdGhost baseDir </> "valid_ghost")
      -- Create an invalid directory (no ghost/master)
      createDirectoryIfMissing True (bdGhost baseDir </> "not_a_ghost")
      
      ghosts <- scanGhosts baseDir
      length ghosts `shouldBe` 1

  it "ignores files in ghost directory" $ 
    withSystemTempDirectory "kokage-test" $ \tmpDir -> do
      let baseDir = mkBaseDir tmpDir
      createValidGhost (bdGhost baseDir </> "valid_ghost")
      -- Create a file (not a directory)
      TIO.writeFile (bdGhost baseDir </> "some_file.txt") "not a ghost"
      
      ghosts <- scanGhosts baseDir
      length ghosts `shouldBe` 1

--------------------------------------------------------------------------------
-- resolveGhost Tests
--------------------------------------------------------------------------------

resolveGhostSpec :: Spec
resolveGhostSpec = describe "resolveGhost" $ do
  it "returns explicit configGhostPath when valid" $ 
    withSystemTempDirectory "kokage-test" $ \tmpDir -> do
      let baseDir = mkBaseDir tmpDir
      let explicitPath = bdGhost baseDir </> "explicit_ghost"
      createValidGhost explicitPath
      createValidGhost (bdGhost baseDir </> "other_ghost")
      
      let config = mkConfig tmpDir (Just explicitPath) Nothing
      result <- resolveGhost config
      result `shouldBe` Just explicitPath

  it "falls back to lastGhost when configGhostPath is invalid" $ 
    withSystemTempDirectory "kokage-test" $ \tmpDir -> do
      let baseDir = mkBaseDir tmpDir
      let lastPath = bdGhost baseDir </> "last_ghost"
      createValidGhost lastPath
      
      let config = mkConfig tmpDir (Just "/nonexistent/ghost") (Just lastPath)
      result <- resolveGhost config
      result `shouldBe` Just lastPath

  it "returns lastGhost when configGhostPath is Nothing" $ 
    withSystemTempDirectory "kokage-test" $ \tmpDir -> do
      let baseDir = mkBaseDir tmpDir
      let lastPath = bdGhost baseDir </> "last_ghost"
      createValidGhost lastPath
      
      let config = mkConfig tmpDir Nothing (Just lastPath)
      result <- resolveGhost config
      result `shouldBe` Just lastPath

  it "falls back to first available ghost when lastGhost is invalid" $ 
    withSystemTempDirectory "kokage-test" $ \tmpDir -> do
      let baseDir = mkBaseDir tmpDir
      createValidGhost (bdGhost baseDir </> "available_ghost")
      
      let config = mkConfig tmpDir Nothing (Just "/nonexistent/last")
      result <- resolveGhost config
      result `shouldBe` Just (bdGhost baseDir </> "available_ghost")

  it "returns first ghost alphabetically when no preferences" $ 
    withSystemTempDirectory "kokage-test" $ \tmpDir -> do
      let baseDir = mkBaseDir tmpDir
      createValidGhost (bdGhost baseDir </> "zebra")
      createValidGhost (bdGhost baseDir </> "alpha")
      
      let config = mkConfig tmpDir Nothing Nothing
      result <- resolveGhost config
      result `shouldBe` Just (bdGhost baseDir </> "alpha")

  it "returns Nothing when no ghosts available" $ 
    withSystemTempDirectory "kokage-test" $ \tmpDir -> do
      let config = mkConfig tmpDir Nothing Nothing
      result <- resolveGhost config
      result `shouldBe` Nothing

--------------------------------------------------------------------------------
-- lastGhost Persistence Tests
--------------------------------------------------------------------------------

lastGhostPersistenceSpec :: Spec
lastGhostPersistenceSpec = describe "lastGhost persistence" $ do
  it "saveLastGhost creates file in data directory" $ 
    withSystemTempDirectory "kokage-test" $ \tmpDir -> do
      let config = mkConfig tmpDir Nothing Nothing
      let ghostPath = "/some/ghost/path"
      
      saveLastGhost config ghostPath
      
      let lastGhostFile = tmpDir </> "data" </> "last_ghost.txt"
      exists <- doesFileExist lastGhostFile
      exists `shouldBe` True

  it "loadLastGhost returns Nothing for fresh config" $ 
    withSystemTempDirectory "kokage-test" $ \tmpDir -> do
      let config = mkConfig tmpDir Nothing Nothing
      
      result <- loadLastGhost config
      result `shouldBe` Nothing

  it "loadLastGhost returns saved ghost path" $ 
    withSystemTempDirectory "kokage-test" $ \tmpDir -> do
      let config = mkConfig tmpDir Nothing Nothing
      let ghostPath = "/mnt/data/ghosts/emily"
      
      saveLastGhost config ghostPath
      result <- loadLastGhost config
      
      result `shouldBe` Just ghostPath

  it "loadLastGhost handles empty file gracefully" $ 
    withSystemTempDirectory "kokage-test" $ \tmpDir -> do
      let config = mkConfig tmpDir Nothing Nothing
      let lastGhostFile = tmpDir </> "data" </> "last_ghost.txt"
      
      createDirectoryIfMissing True (tmpDir </> "data")
      TIO.writeFile lastGhostFile ""
      
      result <- loadLastGhost config
      result `shouldBe` Nothing

  it "loadLastGhost strips whitespace from path" $ 
    withSystemTempDirectory "kokage-test" $ \tmpDir -> do
      let config = mkConfig tmpDir Nothing Nothing
      let lastGhostFile = tmpDir </> "data" </> "last_ghost.txt"
      
      createDirectoryIfMissing True (tmpDir </> "data")
      TIO.writeFile lastGhostFile "  /path/to/ghost  \n"
      
      result <- loadLastGhost config
      result `shouldBe` Just "/path/to/ghost"

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

-- | Create a minimal config for testing
mkConfig :: FilePath -> Maybe FilePath -> Maybe FilePath -> KokageConfig
mkConfig tmpDir mGhostPath mLastGhost = KokageConfig
  { configGhostPath = mGhostPath
  , configLastGhost = mLastGhost
  , configBaseDir   = mkBaseDir tmpDir
  , configSurfaceId = 0
  , configDataDir   = tmpDir </> "data"
  }

-- | Create a valid ghost directory structure
createValidGhost :: FilePath -> IO ()
createValidGhost path = do
  createDirectoryIfMissing True (path </> "ghost" </> "master")
  -- Optionally create a shiori.dll placeholder
  TIO.writeFile (path </> "ghost" </> "master" </> "shiori.dll") "dummy"

-- | Get base name from path (like takeFileName but for our paths)
takeBaseName :: FilePath -> String
takeBaseName = reverse . takeWhile (/= '/') . reverse
