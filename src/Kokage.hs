{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Main Kokage event loop and application entry point.
-- This is the thin IO shell that wires everything together.
module Kokage
  ( -- * Main Entry Point
    kokageMain
    -- * Configuration
  , KokageConfig(..)
  , defaultConfig
    -- * Ghost Management
  , scanGhosts
  , resolveGhost
  , saveLastGhost
  , loadLastGhost
    -- * Re-exports for convenience
  , module Kokage.Collision
  , module Kokage.Surface
  , module Kokage.Event
    -- * SHIORI Support (Wine Bridge)
  , WineShiori(..)
  , WineBridgeConfig(..)
  , defaultWineBridgeConfig
  , startWineBridge
  , stopWineBridge
  , withWineBridge
  , loadShiori
  , unloadShiori
  , sendRequest
  , sendEvent
  , toWinePath
    -- * X11 Support (always-on-top)
  , setWindowAlwaysOnTop
    -- * Wayland Layer Shell Support (always-on-top)
  , isLayerShellSupported
  , initPlatformWindow
  , isPlatformInitialized
  , setWindowLayer
  , setWindowPosition
  , getWindowPosition
  , Layer(..)
  , Edge(..)
  ) where

import           Control.Exception               ( finally, throwIO, try )

import           Data.Char                       ( isAsciiLower, isAsciiUpper, isDigit )
import qualified Data.GI.Base                    as GI
import           Data.GI.Base                    ( AttrOp((:=))
                                                 , castTo
                                                 , glibType
                                                 , new
                                                 , newObject
                                                 , withManagedPtr
                                                 )
import           Data.GI.Base.GValue             ( get_object )
import           Data.List                       ( nub )
import qualified Data.Map.Strict                 as Map
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO
import           Data.Time                       ( getCurrentTime
                                                 , getCurrentTimeZone
                                                 , timeZoneMinutes
                                                 , timeZoneSummerOnly
                                                 , utcToLocalTime
                                                 )

import           Foreign.Ptr                     ( Ptr, nullPtr )

import qualified GI.GLib                         as GLib
import qualified GI.Gdk                          as Gdk
import qualified GI.GdkPixbuf                    as Pixbuf
import qualified GI.Gio                          as Gio
import qualified GI.Gtk                          as Gtk
import qualified GI.Pango                        as Pango
import qualified GI.PangoCairo                   as PangoCairo

import           Kokage.Balloon                  ( BalloonChoice(..)
                                                 , BalloonState
                                                 , bsDrawArea
                                                 , bsLayerShell
                                                 , bsPosition
                                                 , bsWindow
                                                 , clearBalloon
                                                 , clearChoices
                                                 , hasChoices
                                                 , hideBalloon
                                                 , setBalloonId
                                                 , setChoiceCallback
                                                 )
import           Kokage.Callbacks                ( CallbackEnv(..) )
import           Kokage.Character                ( CharacterState(..)
                                                 , cancelSurfaceLifeTimer
                                                 , createCharacter
                                                 , getCharacterBalloon
                                                 , getCharacterSurface
                                                 , hideCharacter
                                                 , initBalloonPosition
                                                 , setCharacterPosition
                                                 , setCharacterSurface
                                                 , showCharacter
                                                 , startSurfaceLifeTimer
                                                 , tickCharacter
                                                 , updateBalloonPosition
                                                 )
import           Kokage.Collision
import           Kokage.Config                   ( BaseDir(..) )
import           Kokage.Event                    ( BalloonMoveMode(..)
                                                 , CharacterNetworkConfig(..)
                                                 , GlobalNetworkConfig(..)
                                                 , InputHandlers(..)
                                                 , MoveMode(..)
                                                 , ShioriConfig(..)
                                                 , TimerHandlers(..)
                                                 , sendShioriWithCallback
                                                 , setupCharacterNetwork
                                                 , setupGlobalNetwork
                                                 )
import           Kokage.Ghost                    ( listAvailableBalloons )
import           Kokage.Install                  ( InstallResult(..), installNar )
import qualified Kokage.Install                  as Install
import           Kokage.Menu                     ( MenuConfig(..)
                                                 , createContextMenu
                                                 , emptyMenuConfig
                                                 , menuStyleFromShellDescript
                                                 )
import           Kokage.Platform                 ( Edge(..)
                                                 , Layer(..)
                                                 , getWindowPosition
                                                 , initPlatformWindow
                                                 , isLayerShellSupported
                                                 , isPlatformInitialized
                                                 , setWindowAlwaysOnTop
                                                 , setWindowLayer
                                                 , setWindowPosition
                                                 )
import           Kokage.SakuraScript.Interpreter ( defaultInterpreterConfig, executeScriptAsync )
import           Kokage.SakuraScript.Parser      ( parseScript )
import           Kokage.Shiori.WineBridge        ( WineBridgeConfig(..)
                                                 , WineShiori(..)
                                                 , defaultWineBridgeConfig
                                                 , loadShiori
                                                 , sendEvent
                                                 , sendNotify
                                                 , sendRequest
                                                 , startWineBridge
                                                 , stopWineBridge
                                                 , toWinePath
                                                 , unloadShiori
                                                 , withWineBridge
                                                 )
import           Kokage.Sound                    ( newSoundState )
import           Kokage.Surface

import           Prelude                         ()

import           Reactive.Banana                 ( compile )
import           Reactive.Banana.Frameworks      ( actuate, newAddHandler )

import           Relude

import           System.Directory                ( XdgDirectory(..)
                                                 , createDirectoryIfMissing
                                                 , doesDirectoryExist
                                                 , doesFileExist
                                                 , getCurrentDirectory
                                                 , getXdgDirectory
                                                 , listDirectory
                                                 )
import           System.FilePath                 ( (</>), takeBaseName, takeExtension )

import           Types.Ghost                     ( CharacterSettings(..)
                                                 , Ghost(..)
                                                 , GhostDescript
                                                 , Shell(..)
                                                 , descriptKeroDefaultLeft
                                                 , descriptKeroDefaultTop
                                                 , descriptKeroName
                                                 , descriptKeroSerikoDefaultSurface
                                                 , descriptName
                                                 , descriptSakuraDefaultLeft
                                                 , descriptSakuraDefaultTop
                                                 , descriptSakuraName
                                                 , descriptSakuraSerikoDefaultSurface
                                                 , descriptShiori
                                                 , loadGhost
                                                 , shellDescriptName
                                                 )
import           Types.Ghost.Shell               ( getCharSettings, getDefinedScopes )
import           Types.Ghost.Surface             ( crIndex, crName, sdCollisions, sdElements )
import           Types.Shiori                    ( ShioriEvent(..) )

data KokageError
  = NoGhostsAvailable
  | GhostLoadError FilePath
  | NoShellsInGhost FilePath
  | SurfaceCompositeError FilePath Int
  deriving ( Show, Eq )

instance Exception KokageError

-- | Configuration for the Kokage application.
data KokageConfig
  = KokageConfig
  { configGhostPath :: !(Maybe FilePath)  -- ^ Explicit ghost path (overrides lastGhost)
  , configLastGhost :: !(Maybe FilePath)  -- ^ Last used ghost path (from saved state)
  , configBaseDir   :: !BaseDir           -- ^ Base directories for ghosts, balloons, etc.
  , configSurfaceId :: !Int               -- ^ Initial surface ID to display
  , configDataDir   :: !FilePath          -- ^ Data directory for config files
  }
  deriving ( Show, Eq )

-- | Default configuration.
-- Uses current working directory for base directories.
defaultConfig :: IO KokageConfig
defaultConfig = do
  cwd <- getCurrentDirectory
  dataDir <- getXdgDirectory XdgData "kokage"
  return
    KokageConfig { configGhostPath = Nothing
                 , configLastGhost = Nothing
                 , configBaseDir   = BaseDir cwd
                 , configSurfaceId = 0
                 , configDataDir   = dataDir
                 }

-- | Get the default shell (first shell) from a ghost.
getDefaultShell :: Ghost -> Maybe Shell
getDefaultShell ghost = listToMaybe (ghostShells ghost)

-- | Get the primary monitor dimensions (width, height).
-- Returns Nothing if no display or monitors are available.
-- Returns (originX, originY, width, height) of the primary monitor.
getScreenGeometry :: IO (Maybe ( Int32, Int32, Int32, Int32 ))
getScreenGeometry = runMaybeT $ do
  display <- MaybeT Gdk.displayGetDefault
  monitors <- lift $ Gdk.displayGetMonitors display
  nMonitors <- lift $ Gio.listModelGetNItems monitors
  if nMonitors == 0
    then MaybeT $ return Nothing
    else do
      -- Get first monitor (primary)
      mObject <- lift $ Gio.listModelGetItem monitors 0
      obj <- MaybeT $ return mObject
      mMonitor <- lift $ castTo Gdk.Monitor obj
      monitor <- MaybeT $ return mMonitor
      rect <- lift $ Gdk.monitorGetGeometry monitor
      x <- lift $ Gdk.getRectangleX rect
      y <- lift $ Gdk.getRectangleY rect
      w <- lift $ Gdk.getRectangleWidth rect
      h <- lift $ Gdk.getRectangleHeight rect
      return ( x, y, w, h )

-- | Calculate initial position for a character based on descript settings and screen size.
-- For sakura (scope 0): positioned at right side of screen (bottom-right by default)
-- For kero (scope 1): positioned to the left of sakura
-- Returns position in display coordinates (x, y).
-- Takes monitor origin (monX, monY) to correctly offset positions in multi-monitor setups.
calcInitialPosition
  :: GhostDescript -> Int -> ( Int, Int ) -> ( Int32, Int32, Int32, Int32 ) -> ( Int32, Int32 )
calcInitialPosition descript scopeId ( surfW, surfH ) ( monX, monY, screenW, screenH )
  = case scopeId of
    0    -- Sakura: bottom-right of screen
      -> let
          relX
            = maybe
              (screenW - fromIntegral surfW)
              fromIntegral
              (descriptSakuraDefaultLeft descript)
          relY
            = maybe (screenH - fromIntegral surfH) fromIntegral (descriptSakuraDefaultTop descript)
        in 
          ( monX + relX, monY + relY )
    _    -- Kero and others: default to left of sakura position
      -> let
          -- Default kero position: left of sakura, same bottom alignment
          defaultX = screenW - fromIntegral surfW - 300  -- 300px left of sakura
          defaultY = screenH - fromIntegral surfH
          relX     = maybe defaultX fromIntegral (descriptKeroDefaultLeft descript)
          relY     = maybe defaultY fromIntegral (descriptKeroDefaultTop descript)
        in 
          ( monX + relX, monY + relY )

--------------------------------------------------------------------------------
-- Ghost Management
--------------------------------------------------------------------------------

-- | Scan for available ghosts in the ghost directory.
-- Returns a sorted list of ghost directory paths that contain valid ghost structure.
-- A valid ghost has a 'ghost/master' subdirectory.
scanGhosts :: BaseDir -> IO [ FilePath ]
scanGhosts (BaseDir baseDirPath) = do
  let ghostDir = baseDirPath </> "ghost"
  exists <- doesDirectoryExist ghostDir
  if not exists
    then return []
    else do
      entries <- listDirectory ghostDir
      let fullPaths = map (ghostDir </>) entries
      -- Filter for directories that look like ghosts (have ghost/master subdirectory)
      validGhosts <- filterM isValidGhostDir fullPaths
      return $ sort validGhosts

-- | Check if a directory is a valid ghost directory.
-- A valid ghost has a 'ghost/master' subdirectory.
isValidGhostDir :: FilePath -> IO Bool
isValidGhostDir path = do
  isDir <- doesDirectoryExist path
  if not isDir
    then return False
    else do
      -- Check for ghost/master directory (standard ghost structure)
      let masterPath = path </> "ghost" </> "master"
      doesDirectoryExist masterPath

-- | Resolve which ghost to load based on configuration.
-- Priority:
--   1. Explicit configGhostPath (if set and valid)
--   2. configLastGhost (if set and valid)
--   3. First ghost from scanned list
--   4. Nothing if no ghosts available
resolveGhost :: KokageConfig -> IO (Maybe FilePath)
resolveGhost config = do
  -- Try explicit path first
  case configGhostPath config of
    Just path -> do
      valid <- isValidGhostDir path
      if valid
        then return $ Just path
        else tryLastGhost
    Nothing   -> tryLastGhost
  where
    tryLastGhost  = case configLastGhost config of
      Just path -> do
        valid <- isValidGhostDir path
        if valid
          then return $ Just path
          else tryFirstGhost
      Nothing   -> tryFirstGhost

    tryFirstGhost = do
      ghosts <- scanGhosts (configBaseDir config)
      return $ listToMaybe ghosts

-- | Save the last used ghost path to persistent storage.
saveLastGhost :: KokageConfig -> FilePath -> IO ()
saveLastGhost config gPath = do
  let dataDir       = configDataDir config
      lastGhostFile = dataDir </> "last_ghost.txt"
  -- Ensure data directory exists
  createDirectoryIfMissing True dataDir
  -- Write the ghost path
  TIO.writeFile lastGhostFile (T.pack gPath)

-- | Load the last used ghost path from persistent storage.
loadLastGhost :: KokageConfig -> IO (Maybe FilePath)
loadLastGhost config = do
  let lastGhostFile = configDataDir config </> "last_ghost.txt"
  exists <- doesFileExist lastGhostFile
  if not exists
    then return Nothing
    else do
      result <- try $ TIO.readFile lastGhostFile
      case result of
        Left (_ :: SomeException) -> return Nothing
        Right content -> do
          let path = T.unpack $ T.strip content
          if null path
            then return Nothing
            else return $ Just path

--------------------------------------------------------------------------------
-- Ghost History (HISTORY file)
--------------------------------------------------------------------------------

-- | Ghost history data stored in HISTORY file.
-- This tracks cumulative time spent with the ghost.
data GhostHistory
  = GhostHistory { ghTime          :: !Int   -- ^ Total runtime in hours (cumulative)
                 , ghVanishedCount :: !Int   -- ^ Number of times the ghost was "vanished"
                 }
  deriving ( Show, Eq )

-- | Default history for a new ghost.
defaultGhostHistory :: GhostHistory
defaultGhostHistory = GhostHistory { ghTime = 0, ghVanishedCount = 0 }

-- | Path to the HISTORY file for a ghost.
historyFilePath :: FilePath -> FilePath
historyFilePath gp = gp </> "ghost" </> "master" </> "HISTORY"

-- | Load ghost history from HISTORY file.
-- Returns 'Nothing' if this is the first boot (file doesn't exist).
-- Returns default history if file exists but can't be parsed.
loadGhostHistory :: FilePath -> IO (Maybe GhostHistory)
loadGhostHistory gp = do
  let historyFile = historyFilePath gp
  exists <- doesFileExist historyFile
  if not exists
    then return Nothing  -- First boot
    else do
      result <- try $ TIO.readFile historyFile
      case result of
        Left (_ :: SomeException) -> return $ Just defaultGhostHistory
        Right content -> return $ Just $ parseHistory content

-- | Parse HISTORY file content.
parseHistory :: T.Text -> GhostHistory
parseHistory content
  = let
      ls = T.lines content
      pairs
        = [ ( key, val )
          | l <- ls
          , let stripped = T.strip l
          , not (T.null stripped)
          , ( key, rest ) <- [ T.breakOn "," stripped ]
          , let val = T.strip $ T.drop 1 rest
          ]
      lookupInt k def = case find (\( key, _ ) -> key == T.toLower (T.strip k)) pairs of
        Nothing       -> def
        Just ( _, v ) -> case reads (T.unpack v) of
          [ ( n, "" ) ] -> n
          _ -> def
    in 
      GhostHistory { ghTime = lookupInt "time" 0, ghVanishedCount = lookupInt "vanished_count" 0 }

-- | Save ghost history to HISTORY file.
saveGhostHistory :: FilePath -> GhostHistory -> IO ()
saveGhostHistory gp history = do
  let historyFile = historyFilePath gp
      content
        = T.unlines
          [ "time, " <> T.pack (show (ghTime history))
          , "vanished_count, " <> T.pack (show (ghVanishedCount history))
          ]
  result <- try $ TIO.writeFile historyFile content
  case result of
    Left (e :: SomeException) -> putStrLn $ "[HISTORY] Warning: Could not save history: " <> show e
    Right () -> return ()

-- | Check if this is the first boot for a ghost.
isFirstBoot :: FilePath -> IO Bool
isFirstBoot gp = do
  mHistory <- loadGhostHistory gp
  return $ case mHistory of
    Nothing -> True   -- No HISTORY file = first boot
    Just _  -> False

--------------------------------------------------------------------------------
-- Balloon Directory Resolution
--------------------------------------------------------------------------------

-- | Find the balloon directory for a ghost.
-- Priority:
--   1. Bundled balloon in ghost directory (ghostPath/balloon/)
--   2. First available balloon in global balloon directory (bdBalloon baseDir)
--   3. Nothing if no balloons found
findBalloonDir :: FilePath -> BaseDir -> IO (Maybe FilePath)
findBalloonDir gp (BaseDir baseDirPath) = do
  -- Check for bundled balloon first
  let bundledBalloon = gp </> "balloon"
  hasBundled <- doesDirectoryExist bundledBalloon
  if hasBundled
    then do
      -- Verify it has balloon surfaces
      hasSurfaces <- doesFileExist (bundledBalloon </> "balloons0.png")
      if hasSurfaces
        then do
          putStrLn $ "[Balloon] Using bundled balloon: " <> bundledBalloon
          return $ Just bundledBalloon
        else tryGlobalBalloon
    else tryGlobalBalloon
  where
    tryGlobalBalloon = do
      let globalBalloonDir = baseDirPath </> "balloon"
      exists <- doesDirectoryExist globalBalloonDir
      if not exists
        then do
          putStrLn "[Balloon] No balloon directory found"
          return Nothing
        else do
          -- List subdirectories (each is a balloon)
          entries <- listDirectory globalBalloonDir
          let fullPaths = map (globalBalloonDir </>) entries
          validBalloons <- filterM isBalloonDir fullPaths
          case validBalloons of
            [] -> do
              putStrLn "[Balloon] No valid balloons in global directory"
              return Nothing
            (firstBalloon : _) -> do
              putStrLn $ "[Balloon] Using global balloon: " <> firstBalloon
              return $ Just firstBalloon

    -- Check if a directory contains balloon surfaces
    isBalloonDir :: FilePath -> IO Bool
    isBalloonDir path = do
      isDir <- doesDirectoryExist path
      if not isDir
        then return False
        else doesFileExist (path </> "balloons0.png")

-- | Main entry point for Kokage.
-- Resolves which ghost to load (explicit path, last used, or first available)
-- and runs the GTK event loop.
kokageMain :: KokageConfig -> IO Int32
kokageMain config = do
  -- Resolve which ghost to load
  gPath <- justOrError NoGhostsAvailable =<< resolveGhost config
  putStrLn $ "Loading ghost from: " <> gPath
  mGhost <- loadGhost gPath
  ghost <- justOrError (GhostLoadError gPath) mGhost
  putStrLn $ "Loaded ghost: " <> gPath
  -- Save this as the last used ghost
  saveLastGhost config gPath
  -- Get default shell
  shell <- justOrError (NoShellsInGhost gPath) $ getDefaultShell ghost
  putStrLn $ "Using shell: " <> shellPath shell
  -- Find requested surface
  let surfaces = shellSurfaces shell
      surfId   = configSurfaceId config
  case findSurfaceById surfId surfaces of
    Nothing      -> do
      putStrLn $ "Error: Surface " <> show surfId <> " not found"
      return (-1)
    Just surfDef -> do
      putStrLn
        $ "Found surface "
        <> show surfId
        <> " with "
        <> show (length $ sdElements surfDef)
        <> " elements, "
        <> show (length $ sdCollisions surfDef)
        <> " collision regions"

      -- Log collision regions for debugging
      forM_ (sdCollisions surfDef) $ \cr -> putStrLn
        $ "  - Collision " <> show (crIndex cr) <> ": " <> T.unpack (crName cr)

      -- Composite the surface
      pixbuf <- justOrError (SurfaceCompositeError gPath surfId)
        =<< compositeSurface (shellPath shell) surfDef
      width <- Pixbuf.pixbufGetWidth pixbuf
      height <- Pixbuf.pixbufGetHeight pixbuf
      putStrLn $ "Composited surface: " <> show width <> "x" <> show height

      -- Try to initialize SHIORI (optional - ghost can run without it)
      -- Use the shiori path from ghost's descript.txt
      let ghostMasterPath = gPath </> "ghost" </> "master"
          shioriName      = descriptShiori (ghostDescript ghost)
      mShiori <- initializeShiori ghostMasterPath shioriName

      -- Check if this is first boot (no HISTORY file)
      firstBoot <- isFirstBoot gPath
      mHistory <- loadGhostHistory gPath
      let vanishedCount = maybe 0 ghVanishedCount mHistory

      when firstBoot $ putStrLn "[HISTORY] First boot detected"

      -- Find balloon directory for the ghost
      mBalloonDir <- findBalloonDir gPath (configBaseDir config)

      -- Send boot NOTIFY sequence if SHIORI is available
      case mShiori of
        Just shiori -> do
          let bootCtx
                = BootNotifyContext
                { bncShiori     = shiori
                , bncGhost      = ghost
                , bncShell      = shell
                , bncGhostPath  = gPath
                , bncBalloonDir = mBalloonDir
                , bncBaseDir    = configBaseDir config
                }
          sendBootNotifySequence bootCtx
        Nothing     -> return ()

      -- Create Install.BaseDir from config's base directory
      let configDir  = unBaseDir (configBaseDir config)
          installDir
            = Install.BaseDir
            { Install.bdGhost        = configDir </> "ghost"
            , Install.bdBalloon      = configDir </> "balloon"
            , Install.bdPlugin       = configDir </> "plugin"
            , Install.bdHeadline     = configDir </> "headline"
            , Install.bdCalendar     = configDir </> "calendar"
            , Install.bdCalendarSkin = configDir </> "calendar" </> "skin"
            }

      -- Run the GTK application with shell (for surface switching)
      runGtkApp ghost shell surfId mShiori gPath firstBoot vanishedCount mBalloonDir installDir
        `finally` cleanupShiori mShiori
  where
    justOrError :: KokageError -> Maybe a -> IO a
    justOrError err Nothing = throwIO err
    justOrError _ (Just x)  = return x

-- | Initialize SHIORI bridge and load the DLL.
-- Returns Nothing if no DLL found or initialization fails.
-- The shioriName comes from the ghost's descript.txt (descriptShiori field).
initializeShiori :: FilePath -> T.Text -> IO (Maybe WineShiori)
initializeShiori ghostMasterPath shioriName = do
  -- Build path from descript's shiori field
  let dllPath = ghostMasterPath </> T.unpack shioriName

  -- Check if the DLL exists
  exists <- doesFileExist dllPath
  if not exists
    then do
      putStrLn $ "[SHIORI] DLL not found: " <> dllPath
      return Nothing
    else do
      putStrLn $ "[SHIORI] Found DLL: " <> dllPath

      -- Determine which bridge to use based on DLL architecture
      -- For now, assume 32-bit DLLs (most ghosts use 32-bit)
      let bridgeConfig
            = defaultWineBridgeConfig { wbcBridgePath = "wine-helper" </> "shiori_bridge32.exe" }

      -- Start the Wine bridge
      putStrLn "[SHIORI] Starting Wine bridge..."
      bridgeResult <- startWineBridge bridgeConfig
      case bridgeResult of
        Left err     -> do
          putStrLn $ "[SHIORI] Failed to start bridge: " <> err
          return Nothing
        Right shiori -> do
          putStrLn "[SHIORI] Bridge started, loading DLL..."
          loadResult <- loadShiori shiori dllPath ghostMasterPath
          case loadResult of
            Left err           -> do
              putStrLn $ "[SHIORI] Failed to load DLL: " <> err
              stopWineBridge shiori
              return Nothing
            Right loadedShiori -> do
              putStrLn "[SHIORI] DLL loaded successfully"
              return $ Just loadedShiori

-- | Clean up SHIORI bridge on exit.
cleanupShiori :: Maybe WineShiori -> IO Int32
cleanupShiori Nothing       = return 0
cleanupShiori (Just shiori) = do
  -- Send OnDestroy NOTIFY before unloading
  -- Reference0: empty for normal shutdown, "reload" for reload
  putStrLn "[SHIORI] Sending OnDestroy..."
  _ <- sendNotify shiori "OnDestroy" (Map.fromList [ ( 0, "" :: T.Text ) ])
  putStrLn "[SHIORI] Unloading DLL..."
  _ <- unloadShiori shiori
  putStrLn "[SHIORI] Stopping bridge..."
  stopWineBridge shiori
  putStrLn "[SHIORI] Cleanup complete"
  return 0

-- | Send boot sequence NOTIFY events to SHIORI.
-- These are informational events sent before OnBoot/OnFirstBoot.
-- Per UKADOC, the full sequence includes system info, installed items, and current state.
data BootNotifyContext
  = BootNotifyContext
  { bncShiori     :: !WineShiori
  , bncGhost      :: !Ghost
  , bncShell      :: !Shell
  , bncGhostPath  :: !FilePath
  , bncBalloonDir :: !(Maybe FilePath)
  , bncBaseDir    :: !BaseDir
  }

sendBootNotifySequence :: BootNotifyContext -> IO ()
sendBootNotifySequence ctx = do
  let shiori      = bncShiori ctx
      ghost       = bncGhost ctx
      shell       = bncShell ctx
      gp          = bncGhostPath ctx
      mBalloonDir = bncBalloonDir ctx
      baseDir     = bncBaseDir ctx
      ghostDesc   = ghostDescript ghost
      shellDesc   = shellDescript shell
      ghostName   = descriptName ghostDesc
      sakuraName  = descriptSakuraName ghostDesc
      keroName    = descriptKeroName ghostDesc
      shellName   = shellDescriptName shellDesc

  putStrLn "[SHIORI] Sending boot NOTIFY sequence..."

  -- 1. OnInitialize - signals SHIORI initialization complete
  -- Reference0: empty for normal boot, "reload" for reload
  _ <- sendNotify shiori "OnInitialize" (Map.fromList [ ( 0, "" :: T.Text ) ])
  putStrLn "[SHIORI] Sent OnInitialize"

  -- 2. basewareversion - baseware version info
  let version      = "0.1.0" :: T.Text
      basewareName = "Kokage" :: T.Text
      fullVersion  = "0.1.0.0" :: T.Text
  _ <- sendNotify
    shiori
    "basewareversion"
    (Map.fromList [ ( 0, version ), ( 1, basewareName ), ( 2, fullVersion ) ])
  putStrLn "[SHIORI] Sent basewareversion"

  -- 3. hwnd - window handle (Windows-specific, send 0 on Linux)
  _ <- sendNotify shiori "hwnd" (Map.fromList [ ( 0, "0" :: T.Text ) ])
  putStrLn "[SHIORI] Sent hwnd"

  -- 4. uniqueid - unique identifier for this ghost instance
  -- Use ghost path hash as a simple unique ID
  let uniqueId = T.pack $ show $ abs $ simpleHash gp
  _ <- sendNotify shiori "uniqueid" (Map.fromList [ ( 0, uniqueId ) ])
  putStrLn "[SHIORI] Sent uniqueid"

  -- 5. capability - list of supported features (full SSP list)
  let capabilities
        = Map.fromList
          [ ( 0, "request.status" :: T.Text )
          , ( 1, "request.securitylevel" )
          , ( 2, "request.baseid" )
          , ( 3, "request.sendertype" )
          , ( 4, "request.x-sstp-passthru" )
          , ( 5, "response.marker" )
          , ( 6, "response.markersend" )
          , ( 7, "response.errorlevel" )
          , ( 8, "response.errordescription" )
          , ( 9, "response.balloonoffset" )
          , ( 10, "response.age" )
          , ( 11, "response.x-sstp-passthru" )
          , ( 12, "response.valuenotify" )
          , ( 13, "response.securitylevel" )
          , ( 14, "response.requestcharset" )
          ]
  _ <- sendNotify shiori "capability" capabilities
  putStrLn "[SHIORI] Sent capability (15 features)"

  -- 6. ownerghostname - the name of this ghost
  _ <- sendNotify shiori "ownerghostname" (Map.fromList [ ( 0, ghostName ) ])
  putStrLn $ "[SHIORI] Sent ownerghostname: " <> T.unpack ghostName

  -- 7. otherghostname - names of other running ghosts (not implemented, send empty)
  _ <- sendNotify shiori "otherghostname" Map.empty
  putStrLn "[SHIORI] Sent otherghostname (empty - single ghost mode)"

  -- 8. installedghostname - list of all installed ghost names
  installedGhosts <- scanGhosts baseDir
  ghostNames <- forM installedGhosts $ \gPath -> do
    mG <- loadGhost gPath
    return $ maybe (T.pack $ takeBaseName gPath) (descriptName . ghostDescript) mG
  let installedGhostRefs = Map.fromList $ zip [ 0 .. ] ghostNames
  _ <- sendNotify shiori "installedghostname" installedGhostRefs
  putStrLn $ "[SHIORI] Sent installedghostname (" <> show (length ghostNames) <> " ghosts)"

  -- 9. installedshellname - list of shell names for current ghost
  let shellNames         = map (shellDescriptName . shellDescript) (ghostShells ghost)
      installedShellRefs = Map.fromList $ zip [ 0 .. ] shellNames
  _ <- sendNotify shiori "installedshellname" installedShellRefs
  putStrLn $ "[SHIORI] Sent installedshellname (" <> show (length shellNames) <> " shells)"

  -- 10. installedballoonname - list of all installed balloon names
  balloonNames <- listAvailableBalloonNames baseDir
  let installedBalloonRefs = Map.fromList $ zip [ 0 .. ] balloonNames
  _ <- sendNotify shiori "installedballoonname" installedBalloonRefs
  putStrLn $ "[SHIORI] Sent installedballoonname (" <> show (length balloonNames) <> " balloons)"

  -- 11. installedheadlinename - list of headline names (not implemented yet)
  _ <- sendNotify shiori "installedheadlinename" Map.empty
  putStrLn "[SHIORI] Sent installedheadlinename (empty)"

  -- 12. installedplugin - list of plugin names (not implemented yet)
  _ <- sendNotify shiori "installedplugin" Map.empty
  putStrLn "[SHIORI] Sent installedplugin (empty)"

  -- 13. ghostpathlist - paths to all installed ghosts
  let ghostPathRefs = Map.fromList $ zip [ 0 .. ] (map T.pack installedGhosts)
  _ <- sendNotify shiori "ghostpathlist" ghostPathRefs
  putStrLn $ "[SHIORI] Sent ghostpathlist (" <> show (length installedGhosts) <> " paths)"

  -- 14. balloonpathlist - paths to all installed balloons
  balloonPaths <- listAvailableBalloonPaths baseDir
  let balloonPathRefs = Map.fromList $ zip [ 0 .. ] (map T.pack balloonPaths)
  _ <- sendNotify shiori "balloonpathlist" balloonPathRefs
  putStrLn $ "[SHIORI] Sent balloonpathlist (" <> show (length balloonPaths) <> " paths)"

  -- 15. headlinepathlist (not implemented)
  _ <- sendNotify shiori "headlinepathlist" Map.empty
  putStrLn "[SHIORI] Sent headlinepathlist (empty)"

  -- 16. pluginpathlist (not implemented)
  _ <- sendNotify shiori "pluginpathlist" Map.empty
  putStrLn "[SHIORI] Sent pluginpathlist (empty)"

  -- 17. calendarskinpathlist (not implemented)
  _ <- sendNotify shiori "calendarskinpathlist" Map.empty
  putStrLn "[SHIORI] Sent calendarskinpathlist (empty)"

  -- 18. calendarpluginpathlist (not implemented)
  _ <- sendNotify shiori "calendarpluginpathlist" Map.empty
  putStrLn "[SHIORI] Sent calendarpluginpathlist (empty)"

  -- 19. rateofusegraph - usage statistics (not implemented)
  _ <- sendNotify shiori "rateofusegraph" Map.empty
  putStrLn "[SHIORI] Sent rateofusegraph (empty)"

  -- 20. OnNotifySelfInfo - current ghost/shell/balloon info
  let balloonName = maybe "" (T.pack . takeBaseName) mBalloonDir
      balloonPath = maybe "" T.pack mBalloonDir
  _ <- sendNotify
    shiori
    "OnNotifySelfInfo"
    (Map.fromList
       [ ( 0, ghostName )
       , ( 1, sakuraName )
       , ( 2, keroName )
       , ( 3, T.pack gp )
       , ( 4, shellName )
       , ( 5, T.pack $ shellPath shell )
       , ( 6, balloonName )
       , ( 7, balloonPath )
       ])
  putStrLn "[SHIORI] Sent OnNotifySelfInfo"

  -- 21. OnNotifyBalloonInfo - current balloon dimensions
  -- Reference0=name, Ref1=path, Ref2=sakura_w, Ref3=sakura_h, Ref4=kero_w, Ref5=kero_h
  _ <- sendNotify
    shiori
    "OnNotifyBalloonInfo"
    (Map.fromList
       [ ( 0, balloonName )
       , ( 1, balloonPath )
       , ( 2, "200" )  -- Default balloon width (would need actual measurement)
       , ( 3, "150" )  -- Default balloon height
       , ( 4, "200" )  -- Kero balloon width
       , ( 5, "150" )  -- Kero balloon height
       ])
  putStrLn "[SHIORI] Sent OnNotifyBalloonInfo"

  -- 22. OnNotifyShellInfo - current shell info
  -- Ref0=name, Ref1=path, Ref2=sakura_w, Ref3=sakura_h, Ref4=kero_w, Ref5=kero_h,
  -- Ref6=author, Ref7=sakura_default_surface, Ref8=kero_default_surface
  let sakuraDefaultSurf = descriptSakuraSerikoDefaultSurface ghostDesc
      keroDefaultSurf   = descriptKeroSerikoDefaultSurface ghostDesc
  _ <- sendNotify
    shiori
    "OnNotifyShellInfo"
    (Map.fromList
       [ ( 0, shellName )
       , ( 1, T.pack $ shellPath shell )
       , ( 2, "200" )  -- Sakura surface width (would need actual measurement)
       , ( 3, "400" )  -- Sakura surface height
       , ( 4, "150" )  -- Kero surface width
       , ( 5, "300" )  -- Kero surface height
       , ( 6, "" )     -- Shell author (from shell descript if available)
       , ( 7, T.pack $ show sakuraDefaultSurf )
       , ( 8, T.pack $ show keroDefaultSurf )
       ])
  putStrLn "[SHIORI] Sent OnNotifyShellInfo"

  -- 23. OnNotifyDressupInfo - dressup/clothing info (not implemented)
  _ <- sendNotify shiori "OnNotifyDressupInfo" Map.empty
  putStrLn "[SHIORI] Sent OnNotifyDressupInfo (empty)"

  -- 24. OnNotifyUserInfo - user information
  -- Reference0=user name, Reference1=default charset
  userName <- getEffectiveUserName
  _ <- sendNotify
    shiori
    "OnNotifyUserInfo"
    (Map.fromList [ ( 0, T.pack userName ), ( 1, "UTF-8" ) ])
  putStrLn "[SHIORI] Sent OnNotifyUserInfo"

  -- 25. OnNotifyOSInfo - OS information
  -- Ref0=OS type/version, Ref1=CPU info, Ref2=memory info, Ref3=display count
  osInfo <- getOSInfo
  _ <- sendNotify
    shiori
    "OnNotifyOSInfo"
    (Map.fromList
       [ ( 0, osInfo )
       , ( 1, "" )  -- CPU info (complex to get portably)
       , ( 2, "" )  -- Memory info
       , ( 3, "1" ) -- Display count
       ])
  putStrLn "[SHIORI] Sent OnNotifyOSInfo"

  -- 26. OnNotifyFontInfo - list of available system fonts
  -- NOTE: Temporarily disabled - causes SHIORI DLL (yaya.dll) to crash
  -- with large font lists.
  -- TODO: Investigate Wine encoding issues or DLL limitations
  -- fontNames <- getSystemFontNames
  -- let fontRefs = Map.fromList $ zip [0..] fontNames
  -- _ <- sendNotify shiori "OnNotifyFontInfo" fontRefs
  -- putStrLn $ "[SHIORI] Sent OnNotifyFontInfo (" <> show (length fontNames) <> " fonts)"
  putStrLn "[SHIORI] Skipped OnNotifyFontInfo (disabled due to DLL crash)"

  -- 27. OnNotifyInternationalInfo - timezone and locale
  tz <- getCurrentTimeZone
  let tzOffsetMins = negate $ timeZoneMinutes tz
  locale <- getLocaleInfo
  _ <- sendNotify
    shiori
    "OnNotifyInternationalInfo"
    (Map.fromList
       [ ( 0, T.pack $ show tzOffsetMins )
       , ( 1
         , if timeZoneSummerOnly tz
             then "1"
             else "0"
         )
       , ( 2, fst locale )  -- Country code
       , ( 3, snd locale )  -- Language code
       ])
  putStrLn "[SHIORI] Sent OnNotifyInternationalInfo"

  putStrLn "[SHIORI] Boot NOTIFY sequence complete (27 events)"

-- | Get OS information string.
getOSInfo :: IO T.Text
getOSInfo = do
  -- Try to read /etc/os-release for Linux distro info
  exists <- doesFileExist "/etc/os-release"
  if exists
    then do
      content <- TIO.readFile "/etc/os-release"
      let lines'     = T.lines content
          prettyName
            = listToMaybe
              [ T.drop 13 (T.filter (/= '"') l) | l <- lines', "PRETTY_NAME=" `T.isPrefixOf` l ]
      return $ fromMaybe "Linux" prettyName
    else return "Linux"

-- | Get effective user name.
getEffectiveUserName :: IO String
getEffectiveUserName = do
  mUser <- lookupEnv "USER"
  return $ fromMaybe "user" mUser

-- | Get locale information (country code, language code).
getLocaleInfo :: IO ( T.Text, T.Text )
getLocaleInfo = do
  mLang <- lookupEnv "LANG"
  case mLang of
    Nothing   -> return ( "", "" )
    Just lang ->
      -- Parse LANG format: "en_US.UTF-8" -> ("US", "en")
      let
          langPart = takeWhile (/= '.') lang
          parts    = break (== '_') langPart
        in 
          case parts of
            ( langCode, '_' : countryCode ) -> return ( T.pack countryCode, T.pack langCode )
            ( langCode, _ ) -> return ( "", T.pack langCode )

-- | Simple hash function for strings (DJB2 algorithm).
simpleHash :: String -> Int
simpleHash = foldl' (\h c -> 33 * h + fromEnum c) 5381

-- | List available balloon names from base directory.
listAvailableBalloonNames :: BaseDir -> IO [ T.Text ]
listAvailableBalloonNames (BaseDir baseDirPath) = do
  let balloonDir = baseDirPath </> "balloon"
  exists <- doesDirectoryExist balloonDir
  if not exists
    then return []
    else do
      entries <- listDirectory balloonDir
      let fullPaths = map (balloonDir </>) entries
      validBalloons <- filterM isBalloonDir fullPaths
      return $ map (T.pack . takeBaseName) validBalloons
  where
    isBalloonDir path = do
      isDir <- doesDirectoryExist path
      if not isDir
        then return False
        else doesFileExist (path </> "balloons0.png")

-- | List available balloon paths from base directory.
listAvailableBalloonPaths :: BaseDir -> IO [ FilePath ]
listAvailableBalloonPaths (BaseDir baseDirPath) = do
  let balloonDir = baseDirPath </> "balloon"
  exists <- doesDirectoryExist balloonDir
  if not exists
    then return []
    else do
      entries <- listDirectory balloonDir
      let fullPaths = map (balloonDir </>) entries
      filterM isBalloonDir fullPaths
  where
    isBalloonDir path = do
      isDir <- doesDirectoryExist path
      if not isDir
        then return False
        else doesFileExist (path </> "balloons0.png")

-- | Get list of system font family names using Pango.
-- Filters out empty names, names with control characters, and duplicates.
_getSystemFontNames :: IO [ T.Text ]
_getSystemFontNames = do
  -- Get the default Pango font map
  fontMap <- PangoCairo.fontMapGetDefault
  -- List all font families
  families <- Pango.fontMapListFamilies fontMap
  -- Get the name of each family, filter out invalid names
  names <- forM families Pango.fontFamilyGetName
  let validNames = filter isValidFontName names
  putStrLn
    $ "[Font] Total fonts: " <> show (length names) <> ", valid: " <> show (length validNames)
  return $ nub validNames
  where
    -- A valid font name is non-empty, contains only printable characters,
    -- and has at least one alphanumeric character (filters out "????????" etc.)
    isValidFontName :: T.Text -> Bool
    isValidFontName name = not (T.null name) && T.all isPrintable name && T.any isAlphaNum name  -- Must have at least one letter or digit

    isPrintable :: Char -> Bool
    isPrintable c = c >= ' ' && c < '\DEL'

    isAlphaNum :: Char -> Bool
    isAlphaNum c = isAsciiLower c || isAsciiUpper c || isDigit c || c > '\x7F'  -- Allow non-ASCII (Japanese, Chinese, etc.)

-- | Run the GTK application with the given shell.
-- The shell contains surface definitions for dynamic surface switching.
-- Now uses CharacterMap for multi-character support.
runGtkApp :: Ghost
          -> Shell
          -> Int
          -> Maybe WineShiori
          -> FilePath
          -> Bool
          -> Int
          -> Maybe FilePath
          -> Install.BaseDir
          -> IO Int32
runGtkApp
  ghost
  shell
  initialSurfaceId
  mShiori
  ghostPath'
  firstBoot
  vanishedCount
  mBalloonDir
  installBaseDir = do
  -- Get start time for uptime tracking
  startTime <- getCurrentTime

  let ghostDesc = ghostDescript ghost

  -- Create SHIORI config if we have a bridge
  let mShioriConfig = case mShiori of
        Nothing -> Nothing
        Just ws -> Just
          $ ShioriConfig { scShiori    = ws
                         , scSurfaceId = initialSurfaceId
                         , scStartTime = startTime
                         , scGhostPath = ghostPath'
                         }

  -- Initialize GTK application
  app <- new
    Gtk.Application
    [ #applicationId := "com.kokage.app", #flags := [ Gio.ApplicationFlagsFlagsNone ] ]

  -- Register application-level actions for context menu
  -- "app.quit" action - cleanly quit the application
  quitAction <- Gio.simpleActionNew "quit" Nothing
  _ <- GI.on quitAction #activate $ \_ -> do
    putStrLn "[Menu] Quit action triggered"
    Gio.applicationQuit app
  Gio.actionMapAddAction app quitAction

  -- "app.cancel" action - does nothing, just closes the menu
  cancelAction <- Gio.simpleActionNew "cancel" Nothing
  _ <- GI.on cancelAction #activate $ \_ -> do
    putStrLn "[Menu] Cancel action triggered (menu closed)"
  Gio.actionMapAddAction app cancelAction

  -- "app.stick" action - toggle always-on-top for all windows
  stickyRef <- newIORef False
  stickAction <- Gio.simpleActionNew "stick" Nothing
  _ <- GI.on stickAction #activate $ \_ -> do
    isSticky <- readIORef stickyRef
    let newSticky = not isSticky
    writeIORef stickyRef newSticky
    putStrLn $ "[Menu] Stick action triggered: " <> show newSticky
    -- Get all windows and set keep-above
    windows <- Gtk.applicationGetWindows app
    forM_ windows $ \win -> do
      Gtk.windowSetDeletable win (not newSticky)
      -- Note: GTK4 doesn't have setKeepAbove directly, we use the Platform module
      -- For now, just log the state change
      putStrLn $ "[Menu] Window sticky state: " <> show newSticky
  Gio.actionMapAddAction app stickAction

  -- "app.close" action - close the current ghost
  closeAction <- Gio.simpleActionNew "close" Nothing
  _ <- GI.on closeAction #activate $ \_ -> do
    putStrLn "[Menu] Close action triggered"
    -- For now, close all windows (single ghost mode)
    windows <- Gtk.applicationGetWindows app
    forM_ windows Gtk.windowClose
  Gio.actionMapAddAction app closeAction

  -- Register placeholder actions
  let dummyActions
        = [ "todo"
          , "update"
          , "vanish"
          , "edit_preference"
          , "open_console"
          , "ghost_manager"
          , "script_log"
          , "scriptinputbox"
          , "usage"
          , "version"
          ]

  forM_ dummyActions $ \name -> do
    action <- Gio.simpleActionNew (T.pack name) Nothing
    _ <- GI.on action #activate $ \_ -> do
      putStrLn $ "[Menu] Placeholder action triggered: " <> name
    Gio.actionMapAddAction app action

  -- Connect activate signal
  _ <- GI.on app #activate $ do
    -- Create all characters defined in shell config
    -- Scope 0 = Sakura, Scope 1 = Kero, Scope 2+ = extra characters
    let shellDesc     = shellDescript shell
        -- Get all defined scopes, ensure 0 and 1 are always included
        definedScopes = nub $ [ 0, 1 ] ++ getDefinedScopes shellDesc

    -- Create each character
    putStrLn $ "[Startup] Defined scopes: " <> show definedScopes
    characterPairs <- forM definedScopes $ \scopeId -> do
      mChar <- createCharacter app shell ghostDesc scopeId mBalloonDir
      case mChar of
        Just _  -> putStrLn $ "[Startup] Created character for scope " <> show scopeId
        Nothing -> putStrLn $ "[Startup] Failed to create character for scope " <> show scopeId
      return ( scopeId, mChar )

    -- Build character map from successfully created characters
    let characters = Map.fromList $ [ ( scopeId, c ) | ( scopeId, Just c ) <- characterPairs ]

    when (Map.null characters) $ do
      putStrLn "Error: No characters could be created"
      return ()

    -- Track current character scope (0=sakura, 1=kero, etc.)
    currentScopeRef <- newIORef (0 :: Int)

    -- Initialize sound state
    soundState <- newSoundState

    -- Helper to get balloon for a scope (falls back to sakura if scope doesn't exist)
    let getBalloon scope = case Map.lookup scope characters of
          Just cs -> getCharacterBalloon cs
          Nothing -> case Map.lookup 0 characters of
            Just cs -> getCharacterBalloon cs
            Nothing -> error "No characters available"  -- Should never happen

    -- Helper to get balloon for current scope
    let _getCurrentBalloon :: IO BalloonState
        _getCurrentBalloon = do
          scope <- readIORef currentScopeRef
          return $ getBalloon scope

    -- IORef to hold surface restore callback (filled after displayScript is defined)
    surfaceRestoreCallbackRef <- newIORef (return () :: IO ())

    -- Time-critical mode: when True, mouse events are blocked (set by \t tag)
    -- Using AddHandler for FRP integration instead of IORef
    ( timeCriticalHandler, fireTimeCritical ) <- newAddHandler

    -- Surface change function using Character module
    let changeSurface :: Int -> Int -> IO ()
        changeSurface scope newSurfaceId = do
          case Map.lookup scope characters of
            Nothing -> putStrLn $ "[Surface] Scope " <> show scope <> " not found"
            Just cs -> do
              setCharacterSurface cs shell newSurfaceId
              -- Send OnSurfaceChange NOTIFY to SHIORI
              case mShiori of
                Nothing     -> return ()
                Just shiori -> do
                  -- Reference0 = sakura surface, Reference1 = kero surface
                  -- Reference2 = change details (scope,surfaceId)
                  sakuraSurf <- maybe (return 0) getCharacterSurface (Map.lookup 0 characters)
                  keroSurf <- maybe (return 10) getCharacterSurface (Map.lookup 1 characters)
                  let refs
                        = Map.fromList
                          [ ( 0, T.pack $ show sakuraSurf )
                          , ( 1, T.pack $ show keroSurf )
                          , ( 2, T.pack $ show scope <> "," <> show newSurfaceId )
                          ]
                  _ <- sendNotify shiori "OnSurfaceChange" refs
                  putStrLn
                    $ "[SHIORI] Sent OnSurfaceChange: scope="
                    <> show scope
                    <> ", surface="
                    <> show newSurfaceId
              -- A hidden character becomes visible again when a surface >= 0 is selected.
              -- Extra characters (scope >= 2) also recalculate their initial position based on
              -- the actual surface size.
              void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
                visible <- readIORef (csVisible cs)
                unless visible $ do
                  when (scope >= 2) $ do
                    mScreenGeom <- getScreenGeometry
                    case mScreenGeom of
                      Just screenGeom -> do
                        surfSize <- readIORef (csSurfaceSize cs)
                        let pos = calcInitialPosition ghostDesc scope surfSize screenGeom
                        putStrLn
                          $ "[Position] Repositioning char" <> show scope <> " to " <> show pos
                        uncurry (setCharacterPosition cs) pos
                      Nothing         -> return ()
                  showCharacter cs
                return False

    -- Track balloon hide timer to prevent race conditions
    balloonHideTimerRef <- newIORef Nothing

    let cancelBalloonHideTimer :: IO ()
        cancelBalloonHideTimer = do
          mTimerId <- readIORef balloonHideTimerRef
          case mTimerId of
            Just timerId -> do
              _ <- GLib.sourceRemove timerId
              writeIORef balloonHideTimerRef Nothing
              putStrLn "[Balloon] Cancelled pending hide timer"
            Nothing      -> return ()

    let hideBalloonIfNoChoices :: IO ()
        hideBalloonIfNoChoices = do
          anyHasChoices <- or <$> mapM (hasChoices . getCharacterBalloon) (Map.elems characters)
          unless anyHasChoices $ do
            -- Cancel any existing timer first to prevent stacking
            cancelBalloonHideTimer
            -- Start new timer and track its ID
            timerId <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 3000 $ do
              putStrLn "[Script] No pending choices, hiding balloons"
              forM_ (Map.elems characters) $ \cs -> hideBalloon (getCharacterBalloon cs)
              writeIORef balloonHideTimerRef Nothing  -- Clear ref when timer fires
              startOnSurfaceRestoreTimer
              return False
            writeIORef balloonHideTimerRef (Just timerId)

        startOnSurfaceRestoreTimer :: IO ()
        startOnSurfaceRestoreTimer = do
          forM_ (Map.elems characters) cancelSurfaceLifeTimer
          -- Check if any character is on a non-default surface
          anyNonDefault <- or
            <$> forM (Map.elems characters) (\cs -> do
                                               currentSurf <- getCharacterSurface cs
                                               return $ currentSurf /= csDefaultSurface cs)
          when anyNonDefault $ do
            let sDesc        = shellDescript shell
                charSettings = getCharSettings 0 sDesc
                delayMs      = fromIntegral $ fromMaybe 15000 (csSurfaceLife charSettings)
            case Map.lookup 0 characters of
              Nothing     -> return ()
              Just sakura -> do
                onSurfaceRestore <- readIORef surfaceRestoreCallbackRef
                startSurfaceLifeTimer sakura delayMs onSurfaceRestore
                putStrLn
                  $ "[SurfaceLife] Started OnSurfaceRestore timer (" <> show delayMs <> "ms)"

    -- Create callback environment
    let callbackEnv
          = CallbackEnv
          { ceCharacters = characters
          , ceCurrentScopeRef = currentScopeRef
          , ceSoundState = soundState
          , ceFireTimeCritical = fireTimeCritical
          , ceChangeSurface = changeSurface
          , ceHideBalloonIfNoChoices = hideBalloonIfNoChoices
          , ceCancelBalloonHideTimer = cancelBalloonHideTimer
          }

    -- IORef to hold the current script's interrupt function
    currentScriptInterruptRef <- newIORef (return () :: IO ())

    -- Helper to display script in balloon with character-by-character animation
    let displayScript :: Maybe T.Text -> IO ()
        displayScript Nothing           = return ()
        displayScript (Just scriptText) = do
          -- Interrupt any currently running script first
          join $ readIORef currentScriptInterruptRef
          -- Cancel all surface life timers when new script starts
          forM_ (Map.elems characters) cancelSurfaceLifeTimer
          -- Hide extra characters (scope >= 2) at script start
          forM_ (Map.toList characters) $ \( scopeId, cs ) -> when (scopeId >= 2)
            $ hideCharacter cs
          -- Reset scope to sakura (0) at start of each new script
          writeIORef currentScopeRef 0
          -- Parse the SakuraScript
          case parseScript scriptText of
            Left err     -> putStrLn $ "[Balloon] Parse error: " <> show err
            Right script -> do
              -- Cancel any pending balloon hide timer before new script
              cancelBalloonHideTimer
              -- Clear all balloons and reset to default balloon surface before new script
              forM_ (Map.elems characters) $ \cs -> do
                let balloon = getCharacterBalloon cs
                clearBalloon balloon
                setBalloonId balloon 0  -- Reset to default balloon surface
              -- Execute script asynchronously with animation
              interruptAction <- executeScriptAsync defaultInterpreterConfig callbackEnv script
              -- Save the interrupt function for this script
              writeIORef currentScriptInterruptRef interruptAction

    -- Collect current surface IDs for OnSurfaceRestore event
    let collectSurfaceRefs :: IO (Map.Map Int T.Text)
        collectSurfaceRefs = do
          surfacePairs <- forM (Map.toList characters) $ \( scopeId, cs ) -> do
            surfId <- getCharacterSurface cs
            return ( scopeId, T.pack $ show surfId )
          return $ Map.fromList surfacePairs

    -- Fill in the surface restore callback now that displayScript is defined
    -- Per UKADOC: Reference0 = sakura surface, Reference1 = kero surface
    writeIORef surfaceRestoreCallbackRef $ do
      surfaceRefs <- collectSurfaceRefs
      sendShioriWithCallback mShioriConfig OnSurfaceRestore surfaceRefs displayScript

    -- Set up choice callback on sakura's balloon
    case Map.lookup 0 characters of
      Just sakura -> do
        let sakuraBalloon = getCharacterBalloon sakura
        setChoiceCallback sakuraBalloon $ \choice -> do
          putStrLn
            $ "[Choice] Selected: "
            <> T.unpack (bcText choice)
            <> " (id="
            <> T.unpack (bcId choice)
            <> ", action="
            <> T.unpack (bcAction choice)
            <> ")"
          -- Clear all balloons and choices after selection
          forM_ (Map.elems characters) $ \cs -> do
            clearBalloon (getCharacterBalloon cs)
            clearChoices (getCharacterBalloon cs)
          -- Handle the action based on its type
          let action = bcAction choice
          case T.stripPrefix "event:" action of
            Just _eventId -> do
              let refs = Map.fromList [ ( 0, bcId choice ), ( 1, bcText choice ) ]
              sendShioriWithCallback mShioriConfig OnChoiceSelect refs displayScript
            Nothing       -> case T.stripPrefix "script:" action of
              Just scriptText -> displayScript (Just scriptText)
              Nothing         -> case T.stripPrefix "url:" action of
                Just url -> do
                  putStrLn $ "[Choice] Opening URL: " <> T.unpack url
                  void $ Gio.appInfoLaunchDefaultForUri url (Nothing :: Maybe Gio.AppLaunchContext)
                Nothing  -> case T.stripPrefix "anchor:" action of
                  Just anchorId -> do
                    let refs = Map.fromList [ ( 0, anchorId ), ( 1, bcText choice ) ]
                    sendShioriWithCallback mShioriConfig OnAnchorSelect refs displayScript
                  Nothing       -> do
                    let refs = Map.fromList [ ( 0, bcId choice ), ( 1, bcText choice ) ]
                    sendShioriWithCallback mShioriConfig OnChoiceSelect refs displayScript
      Nothing     -> return ()

    -- Register "app.close" action - sends OnClose event to SHIORI, then quits
    appCloseAction <- Gio.simpleActionNew "close" Nothing
    _ <- GI.on appCloseAction #activate $ \_ -> do
      putStrLn "[Menu] Close action triggered, sending OnClose event"
      -- Send OnClose event to SHIORI with reason "user"
      let refs = Map.fromList [ ( 0, "user" :: T.Text ) ]
      sendShioriWithCallback mShioriConfig OnClose refs $ \mScript -> do
        case mScript of
          Just script -> do
            -- Execute the goodbye script, then quit when done
            putStrLn "[Close] Executing goodbye script..."
            displayScript (Just script)
            -- Wait a bit for the script to display, then quit
            _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 3000 $ do
              putStrLn "[Close] Goodbye script finished, quitting..."
              Gio.applicationQuit app
              return False
            return ()
          Nothing     -> do
            -- No script returned, just quit immediately
            putStrLn "[Close] No goodbye script, quitting immediately"
            Gio.applicationQuit app
    Gio.actionMapAddAction app appCloseAction

    -- Create global timer event handlers
    ( secondTickHandler, fireSecondTick ) <- newAddHandler
    ( minuteTickHandler, fireMinuteTick ) <- newAddHandler
    ( hourTickHandler, fireHourTick ) <- newAddHandler
    ( motionTickHandler, fireMotionTick ) <- newAddHandler

    -- Helper to get current local time
    let getLocalTime' = do
          tz <- getCurrentTimeZone
          utcToLocalTime tz <$> getCurrentTime

    -- Set up second timer (fires every 1000ms = 1 second)
    _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 1000 $ do
      lt <- getLocalTime'
      fireSecondTick lt
      return True

    -- Set up minute timer (fires every 60000ms = 1 minute)
    _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 60000 $ do
      lt <- getLocalTime'
      fireMinuteTick lt
      return True

    -- Set up hour timer (fires every 3600000ms = 1 hour)
    _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 3600000 $ do
      lt <- getLocalTime'
      fireHourTick lt
      return True

    -- Set up motion tick timer (fires every 100ms for mouse motion sampling)
    _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 100 $ do
      fireMotionTick ()
      return True

    -- Set up animation timer (fires every 50ms = 20fps)
    -- This drives the SERIKO animations
    _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 50 $ do
      -- Tick all characters
      forM_ (Map.elems characters) $ \cs -> tickCharacter cs shell 50
      return True

    -- Set up global timer FRP network
    let globalConfig
          = GlobalNetworkConfig
          { gncTimers        = TimerHandlers
              { thSecondTick = secondTickHandler
              , thMinuteTick = minuteTickHandler
              , thHourTick   = hourTickHandler
              , thMotionTick = motionTickHandler
              }
          , gncShiori        = mShioriConfig
          , gncScriptHandler = displayScript
          }
    globalNetwork <- compile (setupGlobalNetwork globalConfig)
    actuate globalNetwork

    -- Set up FRP network for each character window
    forM_ (Map.toList characters) $ \( scopeId, cs ) -> do
      -- Create input handlers for this character's window
      ( dragBeginHandler, fireDragBegin ) <- newAddHandler
      ( dragUpdateHandler, fireDragUpdate ) <- newAddHandler
      ( dragEndHandler, fireDragEnd ) <- newAddHandler
      ( motionHandler, fireMotion ) <- newAddHandler
      ( rightClickHandler, fireRightClick ) <- newAddHandler
      ( leftClickHandler, fireLeftClick ) <- newAddHandler

      -- Create input handlers for this character's balloon
      ( balloonDragBeginHandler, fireBalloonDragBegin ) <- newAddHandler
      ( balloonDragUpdateHandler, fireBalloonDragUpdate ) <- newAddHandler
      ( balloonDragEndHandler, fireBalloonDragEnd ) <- newAddHandler
      ( balloonMotionHandler, fireBalloonMotion ) <- newAddHandler
      ( balloonRightClickHandler, _fireBalloonRightClick ) <- newAddHandler
      ( balloonLeftClickHandler, fireBalloonLeftClick ) <- newAddHandler

      let window  = csWindow cs
          picture = csPicture cs
          bs      = getCharacterBalloon cs

      -- Create drag gesture (for left-click drag/move)
      dragGesture <- new Gtk.GestureDrag []
      _ <- GI.on dragGesture #dragBegin $ curry fireDragBegin
      _ <- GI.on dragGesture #dragUpdate $ curry fireDragUpdate
      _ <- GI.on dragGesture #dragEnd $ curry fireDragEnd
      Gtk.widgetAddController picture dragGesture

      -- Create left-click gesture (Button 1) for click count detection (single/double click)
      leftClickGesture <- new Gtk.GestureClick [ #button := 1 ]
      _ <- GI.on leftClickGesture #pressed
        $ \nPress x y -> fireLeftClick ( fromIntegral nPress, x, y )
      Gtk.widgetAddController picture leftClickGesture

      -- Create drag gesture for balloon
      balloonDragGesture <- new Gtk.GestureDrag []
      _ <- GI.on balloonDragGesture #dragBegin $ curry fireBalloonDragBegin
      _ <- GI.on balloonDragGesture #dragUpdate $ curry fireBalloonDragUpdate
      _ <- GI.on balloonDragGesture #dragEnd $ curry fireBalloonDragEnd
      Gtk.widgetAddController (bsDrawArea bs) balloonDragGesture

      -- Create left-click gesture for balloon
      balloonLeftClickGesture <- new Gtk.GestureClick [ #button := 1 ]
      _ <- GI.on balloonLeftClickGesture #pressed $ \nPress x y -> do
        fireBalloonLeftClick ( fromIntegral nPress, x, y )
      Gtk.widgetAddController (bsDrawArea bs) balloonLeftClickGesture

      -- Create motion controller for balloon
      balloonMotionController <- new Gtk.EventControllerMotion []
      _ <- GI.on balloonMotionController #motion $ curry fireBalloonMotion
      Gtk.widgetAddController (bsDrawArea bs) balloonMotionController

      -- Create right-click gesture (Button 3) for context menu
      rightClickGesture <- new Gtk.GestureClick [ #button := 3 ]  -- Button 3 = right-click
      _ <- GI.on rightClickGesture #pressed $ \_nPress x y -> do
        fireRightClick ( x, y )
      Gtk.widgetAddController picture rightClickGesture

      -- Create context menu for this character's window
      let menuStyle        = menuStyleFromShellDescript (shellPath shell) (shellDescript shell)
          shellList
            = map
              (\s -> ( shellDescriptName (shellDescript s), T.pack (shellPath s) ))
              (ghostShells ghost)
          currentShellName = shellDescriptName (shellDescript shell)
      balloonList <- listAvailableBalloons installBaseDir
      let menuConfig
            = emptyMenuConfig
            { mcShells = shellList, mcCurrentShell = currentShellName, mcBalloons = balloonList }
      contextMenu <- createContextMenu window menuStyle menuConfig

      -- Create motion controller
      motionController <- new Gtk.EventControllerMotion []
      _ <- GI.on motionController #motion $ curry fireMotion
      Gtk.widgetAddController picture motionController

      -- Create DropTarget for NAR file drops (only on sakura)
      when (scopeId == 0) $ do
        gfileType <- glibType @Gio.File
        dropTarget <- Gtk.dropTargetNew gfileType [ Gdk.DragActionCopy ]
        _ <- GI.on dropTarget #drop $ \gvalue _x _y -> do
          mPath <- withManagedPtr gvalue $ \gvPtr -> do
            objPtr <- get_object gvPtr :: IO (Ptr Gio.File)
            if objPtr == nullPtr
              then return Nothing
              else do
                file <- newObject Gio.File objPtr
                Gio.fileGetPath file
          case mPath of
            Nothing   -> do
              putStrLn "Drop: Could not get file path"
              return False
            Just path -> do
              putStrLn $ "Dropped file: " <> path
              if takeExtension path == ".nar"
                then do
                  baseDir <- getDefaultBaseDir
                  result <- installNar baseDir path
                  case result of
                    InstallSuccess name itype ipath _ -> do
                      putStrLn
                        $ "Installed " <> T.unpack name <> " (" <> show itype <> ") to " <> ipath
                      return True
                    InstallFailure err -> do
                      putStrLn $ "Installation failed: " <> T.unpack err
                      return False
                else do
                  putStrLn $ "Ignored non-NAR file: " <> path
                  return False
        Gtk.widgetAddController window dropTarget

      -- Get collision regions from current surface
      currentSurfId <- readIORef (csCurrentSurface cs)
      let surfaces   = shellSurfaces shell
          collisions = maybe [] sdCollisions (findSurfaceById currentSurfId surfaces)

      -- Create move mode based on layer-shell status
      let isLayerShell = csLayerShell cs
      moveMode <- if isLayerShell
        then do
          let updatePosition :: Double -> Double -> IO ()
              updatePosition dx dy = do
                ( currentX, currentY ) <- readIORef (csPosition cs)
                let newX = currentX + round dx
                    newY = currentY + round dy
                writeIORef (csPosition cs) ( newX, newY )
                void $ setWindowPosition window newX newY
                -- Update balloon position after character moves
                updateBalloonPosition cs dx dy
          return $ MoveLayerShell updatePosition
        else do
          let beginMove :: Double -> Double -> IO ()
              beginMove x y = void $ runMaybeT $ do
                surface <- MaybeT $ Gtk.nativeGetSurface window
                toplevel <- MaybeT $ Gdk.castTo Gdk.Toplevel surface
                disp <- MaybeT Gdk.displayGetDefault
                seat <- MaybeT $ Gdk.displayGetDefaultSeat disp
                device <- MaybeT $ Gdk.seatGetPointer seat
                MaybeT $ pure <$> Gdk.toplevelBeginMove toplevel device 0 x y 0
          return $ MoveToplevel beginMove

      -- Create balloon move mode based on layer-shell status
      balloonIsLayerShell <- readIORef (bsLayerShell bs)
      balloonMoveMode <- if balloonIsLayerShell
        then do
          let updateBalloonPos :: Double -> Double -> IO ()
              updateBalloonPos dx dy = do
                ( currentX, currentY ) <- readIORef (bsPosition bs)
                let newX = currentX + round dx
                    newY = currentY + round dy
                writeIORef (bsPosition bs) ( newX, newY )
                void $ setWindowPosition (bsWindow bs) (fromIntegral newX) (fromIntegral newY)
          return $ BalloonMoveLayerShell updateBalloonPos
        else do
          let beginBalloonMove :: Double -> Double -> IO ()
              beginBalloonMove x y = void $ runMaybeT $ do
                surface <- MaybeT $ Gtk.nativeGetSurface (bsWindow bs)
                toplevel <- MaybeT $ Gdk.castTo Gdk.Toplevel surface
                disp <- MaybeT Gdk.displayGetDefault
                seat <- MaybeT $ Gdk.displayGetDefaultSeat disp
                device <- MaybeT $ Gdk.seatGetPointer seat
                lift $ void $ Gdk.toplevelBeginMove toplevel device 0 x y 0
          return $ BalloonMoveToplevel beginBalloonMove

      -- Build unified character+balloon network config
      let charConfig
            = CharacterNetworkConfig
            { cncWindow = window
            , cncInputs = InputHandlers
                { ihDragBegin  = dragBeginHandler
                , ihDragUpdate = dragUpdateHandler
                , ihDragEnd    = dragEndHandler
                , ihMotion     = motionHandler
                , ihRightClick = rightClickHandler
                , ihLeftClick  = leftClickHandler
                }
            , cncCollisions = collisions
            , cncMoveMode = moveMode
            , cncScopeId = scopeId
            , cncShiori = mShioriConfig
            , cncScriptHandler = displayScript
            , cncContextMenu = contextMenu
            , cncMotionTick = motionTickHandler
              -- Balloon integration
            , cncBalloonWindow = bsWindow bs
            , cncBalloonInputs = InputHandlers
                { ihDragBegin  = balloonDragBeginHandler
                , ihDragUpdate = balloonDragUpdateHandler
                , ihDragEnd    = balloonDragEndHandler
                , ihMotion     = balloonMotionHandler
                , ihRightClick = balloonRightClickHandler
                , ihLeftClick  = balloonLeftClickHandler
                }
            , cncBalloonMoveMode = balloonMoveMode
              -- Time-critical mode handler (blocks mouse events during \t sections)
            , cncTimeCriticalHandler = timeCriticalHandler
            }

      -- Compile and activate unified character+balloon network
      charNetwork <- compile (setupCharacterNetwork charConfig)
      actuate charNetwork

      putStrLn $ "[Character " <> show scopeId <> "] FRP network activated (with balloon)"

    -- Set initial positions for characters based on descript and screen size
    mScreenGeom <- getScreenGeometry
    case mScreenGeom of
      Just screenGeom@( monX, monY, monW, monH ) -> do
        putStrLn
          $ "[Position] Screen geometry: origin=("
          <> show monX
          <> ","
          <> show monY
          <> ") size="
          <> show monW
          <> "x"
          <> show monH
        forM_ (Map.toList characters) $ \( scopeId, cs ) -> do
          surfSize <- readIORef (csSurfaceSize cs)
          let pos = calcInitialPosition ghostDesc scopeId surfSize screenGeom
          putStrLn $ "[Position] Character " <> show scopeId <> " initial position: " <> show pos
          uncurry (setCharacterPosition cs) pos
      Nothing
       -> putStrLn "[Position] Warning: Could not get screen geometry, using default positions"

    -- Show only main characters (scope 0 and 1), keep extra characters (scope >= 2) hidden
    forM_ (Map.toList characters) $ \( scopeId, cs ) -> when (scopeId < 2) $ showCharacter cs

    -- Position balloons relative to their characters
    -- This needs a small delay to ensure windows are realized
    _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 100 $ do
      forM_ (Map.elems characters) $ \cs -> initBalloonPosition cs shell
      return False  -- Don't repeat

    -- Send OnBoot or OnFirstBoot event
    if firstBoot
      then do
        let refs = Map.fromList [ ( 0, T.pack $ show vanishedCount ) ]
        sendShioriWithCallback mShioriConfig OnFirstBoot refs displayScript
        saveGhostHistory ghostPath' defaultGhostHistory
      else sendShioriWithCallback mShioriConfig OnBoot Map.empty displayScript

  -- Run application
  Gio.applicationRun app Nothing

-- | Get the default base directories for NAR installation
-- Uses XDG data directory (~/.local/share/kokage/)
getDefaultBaseDir :: IO Install.BaseDir
getDefaultBaseDir = do
  cwd <- getCurrentDirectory
  return
    Install.BaseDir
    { Install.bdGhost        = cwd </> "ghost"
    , Install.bdBalloon      = cwd </> "balloon"
    , Install.bdPlugin       = cwd </> "plugin"
    , Install.bdHeadline     = cwd </> "headline"
    , Install.bdCalendar     = cwd </> "calendar"
    , Install.bdCalendarSkin = cwd </> "calendar" </> "skin"
    }

-- | Parse a hex color string like "#FF0000" or "FF0000" to RGB values (0.0-1.0)
_parseHexColor :: T.Text -> Maybe ( Double, Double, Double )
_parseHexColor hexStr = do
  let hex = T.dropWhile (== '#') hexStr
  case T.length hex of
    6 -> do
      r <- parseHexByte (T.take 2 hex)
      g <- parseHexByte (T.take 2 (T.drop 2 hex))
      b <- parseHexByte (T.drop 4 hex)
      Just ( fromIntegral r / 255.0, fromIntegral g / 255.0, fromIntegral b / 255.0 )
    3 -> do
      -- Short form like "F00" -> "FF0000"
      r <- parseHexNibble (T.take 1 hex)
      g <- parseHexNibble (T.take 1 (T.drop 1 hex))
      b <- parseHexNibble (T.drop 2 hex)
      Just
        ( fromIntegral (r * 17) / 255.0
        , fromIntegral (g * 17) / 255.0
        , fromIntegral (b * 17) / 255.0
        )
    _ -> Nothing
  where
    parseHexByte :: T.Text -> Maybe Int
    parseHexByte t = case reads ("0x" ++ T.unpack t) of
      [ ( n, "" ) ] -> Just n
      _ -> Nothing

    parseHexNibble :: T.Text -> Maybe Int
    parseHexNibble t = case reads ("0x" ++ T.unpack t) of
      [ ( n, "" ) ] -> Just n
      _ -> Nothing

-- | Look up a named color to RGB values (0.0-1.0)
_lookupColorName :: T.Text -> Maybe ( Double, Double, Double )
_lookupColorName name = Map.lookup (T.toLower name) colorNames
  where
    colorNames :: Map.Map T.Text ( Double, Double, Double )
    colorNames
      = Map.fromList
        [ ( "black", ( 0.0, 0.0, 0.0 ) )
        , ( "white", ( 1.0, 1.0, 1.0 ) )
        , ( "red", ( 1.0, 0.0, 0.0 ) )
        , ( "green", ( 0.0, 0.5, 0.0 ) )
        , ( "blue", ( 0.0, 0.0, 1.0 ) )
        , ( "yellow", ( 1.0, 1.0, 0.0 ) )
        , ( "cyan", ( 0.0, 1.0, 1.0 ) )
        , ( "magenta", ( 1.0, 0.0, 1.0 ) )
        , ( "gray", ( 0.5, 0.5, 0.5 ) )
        , ( "grey", ( 0.5, 0.5, 0.5 ) )
        , ( "orange", ( 1.0, 0.65, 0.0 ) )
        , ( "pink", ( 1.0, 0.75, 0.8 ) )
        , ( "purple", ( 0.5, 0.0, 0.5 ) )
        , ( "brown", ( 0.65, 0.16, 0.16 ) )
        , ( "navy", ( 0.0, 0.0, 0.5 ) )
        , ( "teal", ( 0.0, 0.5, 0.5 ) )
        , ( "olive", ( 0.5, 0.5, 0.0 ) )
        , ( "maroon", ( 0.5, 0.0, 0.0 ) )
        , ( "lime", ( 0.0, 1.0, 0.0 ) )
        , ( "aqua", ( 0.0, 1.0, 1.0 ) )
        , ( "silver", ( 0.75, 0.75, 0.75 ) )
        , ( "fuchsia", ( 1.0, 0.0, 1.0 ) )
        , ( "fuchsia", ( 1.0, 0.0, 1.0 ) )
        ]
