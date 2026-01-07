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

import           Control.Concurrent              ( threadDelay )
import           Control.Exception               ( Exception
                                                 , SomeException
                                                 , finally
                                                 , throwIO
                                                 , try
                                                 )
import           Control.Monad                   ( filterM, forM_, unless, void, when, join )
import           Control.Monad.Except            ( MonadError(throwError) )
import           Control.Monad.Trans.Class       ( lift )
import           Control.Monad.Trans.Maybe       ( MaybeT(runMaybeT, MaybeT) )

import           Data.GI.Base                    ( AttrOp((:=))
                                                 , castTo
                                                 , glibType
                                                 , new
                                                 , newObject
                                                 , on
                                                 , withManagedPtr
                                                 )
import           Data.GI.Base.GValue             ( get_object )
import           Data.IORef                      ( newIORef, readIORef, writeIORef )
import           Data.Int                        ( Int32 )
import           Data.List                       ( sort )
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      ( fromMaybe, listToMaybe )
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO
import           Data.Time                       ( getCurrentTime
                                                 , getCurrentTimeZone
                                                 , utcToLocalTime
                                                 , utctDay
                                                 , utctDayTime
                                                 , toGregorian
                                                 , timeToTimeOfDay
                                                 , TimeOfDay(..)
                                                 )
import           Data.Time.Calendar.WeekDate     ( toWeekDate )
import           System.Environment              ( lookupEnv )

import qualified Data.Set                        as Set

import           Foreign.Ptr                     ( Ptr, nullPtr )

import qualified GI.GLib                         as GLib
import qualified GI.Gdk                          as Gdk
import qualified GI.GdkPixbuf                    as Pixbuf
import qualified GI.Gio                          as Gio
import qualified GI.Gtk                          as Gtk

import           Kokage.Animation                ( AnimationState(..)
                                                 , ActiveAnim(..)
                                                 , clearAnimations
                                                 , getEnabledBinds
                                                 , stopAnimation
                                                 , toggleBind
                                                 )
import qualified Types.Ghost.Surface             as Surface
import           Kokage.Balloon                  ( BalloonChoice(..)
                                                 , BalloonState(..)
                                                 , addChoice
                                                 , appendChar
                                                 , appendNewline
                                                 , appendNewlineHalf
                                                 , appendNewlinePercent
                                                 , appendText
                                                 , clearBalloon
                                                 , clearChars
                                                 , clearChoices
                                                 , hasChoices
                                                 , hideBalloon
                                                 , moveCursor
                                                 , setChoiceCallback
                                                 , setFontBold
                                                 , setFontColor
                                                 , setFontItalic
                                                 , setFontName
                                                 , setFontSize
                                                 , getFontSize
                                                 , setFontStrike
                                                 , setFontUnderline
                                                 , setFontSub
                                                 , setFontSup
                                                 , resetFont
                                                 , showBalloon
                                                 )
import           Kokage.Character                ( CharacterState(..)
                                                 , createCharacter
                                                 , getCharacterBalloon
                                                 , initBalloonPosition
                                                 , setCharacterPosition
                                                 , setCharacterSurface
                                                 , showCharacter
                                                 , tickCharacter
                                                 , updateBalloonPosition
                                                 )
import           Kokage.Collision
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
import           Kokage.Install                  ( BaseDir(..), InstallResult(..), installNar )
import           Kokage.Menu                     ( createContextMenu, menuStyleFromShellDescript )
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
import           Kokage.Sound                    ( SoundState
                                                 , newSoundState
                                                 , playSound
                                                 , stopSound
                                                 )
import           Kokage.SakuraScript.Interpreter ( InterpreterCallbacks(..)
                                                 , InterpreterConfig(..)
                                                 , defaultCallbacks
                                                 , defaultInterpreterConfig
                                                 , executeScriptAsync
                                                 )
import           Kokage.SakuraScript.Parser      ( parseScript )
import           Types.SakuraScript              ( EnvVar(..), FontCmd(..), Color(..), FontSize(..), FontToggle(..), SoundAction(..) )
import           Kokage.Shiori.WineBridge        ( WineBridgeConfig(..)
                                                 , WineShiori(..)
                                                 , defaultWineBridgeConfig
                                                 , loadShiori
                                                 , sendEvent
                                                 , sendRequest
                                                 , startWineBridge
                                                 , stopWineBridge
                                                 , toWinePath
                                                 , unloadShiori
                                                 , withWineBridge
                                                 )
import           Kokage.Surface

import           Reactive.Banana                 ( compile )
import           Reactive.Banana.Frameworks      ( actuate, newAddHandler )

import           System.Directory                ( XdgDirectory(..)
                                                 , createDirectoryIfMissing
                                                 , doesDirectoryExist
                                                 , doesFileExist
                                                 , getCurrentDirectory
                                                 , getXdgDirectory
                                                 , listDirectory
                                                 )
import           System.FilePath                 ( (</>), takeExtension )

import           Types.Ghost                     ( CollisionRegion(..)
                                                 , Ghost(..)
                                                 , GhostDescript(..)
                                                 , Shell(..)
                                                 , SurfaceDefinition(..)
                                                 , loadGhost
                                                 )
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
  let baseDir
        = BaseDir { bdGhost        = cwd </> "ghost"
                  , bdBalloon      = cwd </> "balloon"
                  , bdPlugin       = cwd </> "plugin"
                  , bdHeadline     = cwd </> "headline"
                  , bdCalendar     = cwd </> "calendar"
                  , bdCalendarSkin = cwd </> "calendar" </> "skin"
                  }
  return
    KokageConfig { configGhostPath = Nothing
                 , configLastGhost = Nothing
                 , configBaseDir   = baseDir
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
scanGhosts baseDir = do
  let ghostDir = bdGhost baseDir
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
historyFilePath ghostPath = ghostPath </> "ghost" </> "master" </> "HISTORY"

-- | Load ghost history from HISTORY file.
-- Returns 'Nothing' if this is the first boot (file doesn't exist).
-- Returns default history if file exists but can't be parsed.
loadGhostHistory :: FilePath -> IO (Maybe GhostHistory)
loadGhostHistory ghostPath = do
  let historyFile = historyFilePath ghostPath
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
      lookupInt k def = case lookup (T.toLower $ T.strip k) pairs of
        Nothing -> def
        Just v  -> case reads (T.unpack v) of
          [ ( n, "" ) ] -> n
          _ -> def
    in
      GhostHistory { ghTime = lookupInt "time" 0, ghVanishedCount = lookupInt "vanished_count" 0 }

-- | Save ghost history to HISTORY file.
saveGhostHistory :: FilePath -> GhostHistory -> IO ()
saveGhostHistory ghostPath history = do
  let historyFile = historyFilePath ghostPath
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
isFirstBoot ghostPath = do
  mHistory <- loadGhostHistory ghostPath
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
findBalloonDir ghostPath baseDir = do
  -- Check for bundled balloon first
  let bundledBalloon = ghostPath </> "balloon"
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
      let globalBalloonDir = bdBalloon baseDir
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
            []          -> do
              putStrLn "[Balloon] No valid balloons in global directory"
              return Nothing
            (first : _) -> do
              putStrLn $ "[Balloon] Using global balloon: " <> first
              return $ Just first

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
kokageMain :: KokageConfig -> IO ()
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
    Nothing      -> putStrLn $ "Error: Surface " <> show surfId <> " not found"
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

      -- Run the GTK application with shell (for surface switching)
      runGtkApp ghost shell surfId mShiori gPath firstBoot vanishedCount mBalloonDir
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
cleanupShiori :: Maybe WineShiori -> IO ()
cleanupShiori Nothing       = return ()
cleanupShiori (Just shiori) = do
  putStrLn "[SHIORI] Unloading DLL..."
  _ <- unloadShiori shiori
  putStrLn "[SHIORI] Stopping bridge..."
  stopWineBridge shiori
  putStrLn "[SHIORI] Cleanup complete"

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
          -> IO ()
runGtkApp ghost shell initialSurfaceId mShiori ghostPath' firstBoot vanishedCount mBalloonDir = do
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
  _ <- on quitAction #activate $ \_ -> do
    putStrLn "[Menu] Quit action triggered"
    Gio.applicationQuit app
  Gio.actionMapAddAction app quitAction

  -- "app.cancel" action - does nothing, just closes the menu
  cancelAction <- Gio.simpleActionNew "cancel" Nothing
  _ <- on cancelAction #activate $ \_ -> do
    putStrLn "[Menu] Cancel action triggered (menu closed)"
  Gio.actionMapAddAction app cancelAction

  -- Register placeholder actions
  let dummyActions
        = [ "todo"
          , "stick"
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
    _ <- on action #activate $ \_ -> do
      putStrLn $ "[Menu] Placeholder action triggered: " <> name
    Gio.actionMapAddAction app action

  -- Connect activate signal
  _ <- on app #activate $ do
    -- Create characters using the Character module
    -- Scope 0 = Sakura, Scope 1 = Kero
    -- Pass balloon directory so each character loads the correct balloon surface
    mSakura <- createCharacter app shell ghostDesc 0 mBalloonDir
    mKero <- createCharacter app shell ghostDesc 1 mBalloonDir

    -- Build character map from successfully created characters
    let characters
          = Map.fromList
          $ concat [ maybe [] (\c -> [ ( 0, c ) ]) mSakura, maybe [] (\c -> [ ( 1, c ) ]) mKero ]

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
    let getCurrentBalloon = do
          scope <- readIORef currentScopeRef
          return $ getBalloon scope

    -- Surface change function using Character module
    let changeSurface :: Int -> Int -> IO ()
        changeSurface scope newSurfaceId = do
          case Map.lookup scope characters of
            Nothing -> putStrLn $ "[Surface] Scope " <> show scope <> " not found"
            Just cs -> setCharacterSurface cs shell newSurfaceId

    -- Helper to hide all balloons if none have choices
    let hideBalloonIfNoChoices :: IO ()
        hideBalloonIfNoChoices = do
          -- Check if any balloon has choices
          anyHasChoices <- or
            <$> mapM (\cs -> hasChoices (getCharacterBalloon cs)) (Map.elems characters)
          unless anyHasChoices $ do
            -- wait for a short moment to ensure user sees the completed text
            _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 3000 $ do
              putStrLn "[Script] No pending choices, hiding balloons"
              forM_ (Map.elems characters) $ \cs -> hideBalloon (getCharacterBalloon cs)
              return False
            pure ()

    -- Create interpreter callbacks that interact with the balloon and surface
    let interpreterCallbacks
          = defaultCallbacks
          { cbAppendChar   = \c -> getCurrentBalloon >>= \b -> appendChar b c
          , cbAppendText   = \t -> getCurrentBalloon >>= \b -> appendText b t
          , cbNewline      = getCurrentBalloon >>= appendNewline
          , cbNewlineHalf  = getCurrentBalloon >>= appendNewlineHalf
          , cbNewlinePercent = \pct -> getCurrentBalloon >>= \b -> appendNewlinePercent b pct
          , cbClear        = getCurrentBalloon >>= clearBalloon
          , cbClearChars   = \n -> getCurrentBalloon >>= \b -> clearChars b n
          , cbSetScope     = \scope -> do
              writeIORef currentScopeRef scope
              putStrLn $ "[Scope] Switched to scope " <> show scope
          , cbSetSurface   = changeSurface
          , cbSetBalloon   = \scope balloonId -> do
              putStrLn $ "[Balloon] Set balloon " <> show balloonId <> " for scope " <> show scope
              -- Balloon style switching would require loading different balloon surfaces
              -- For now, log the request - full implementation needs balloon directory access
          , cbHideBalloon  = \scope -> do
              case Map.lookup scope characters of
                Just cs -> hideBalloon (getCharacterBalloon cs)
                Nothing -> return ()
          , cbShowBalloon  = \scope -> do
              case Map.lookup scope characters of
                Just cs -> showBalloon (getCharacterBalloon cs)
                Nothing -> return ()
          , cbMoveCursor   = \x y -> do
              b <- getCurrentBalloon
              moveCursor b x y
          , cbAddChoice    = \choiceId text action -> do
              b <- getCurrentBalloon
              addChoice b (BalloonChoice text choiceId action)
          , cbClearChoices = getCurrentBalloon >>= clearChoices
          -- Animation callbacks
          , cbAnimStart    = \scope animId -> do
              putStrLn $ "[Anim] Start animation " <> show animId <> " on scope " <> show scope
              -- Animation start requires surface definition lookup which is done at Character level
          , cbAnimStop     = \scope animId -> do
              putStrLn $ "[Anim] Stop animation " <> show animId <> " on scope " <> show scope
              case Map.lookup scope characters of
                Just cs -> do
                  let animState = csAnimState cs
                  activeAnims <- readIORef (asActiveAnims animState)
                  let newAnims = stopAnimation activeAnims animId
                  writeIORef (asActiveAnims animState) newAnims
                Nothing -> return ()
          , cbAnimWait     = \scope animId' -> do
              putStrLn $ "[Anim] Wait for animation " <> show animId' <> " on scope " <> show scope
              -- Poll until animation is no longer active (simple busy wait with delay)
              case Map.lookup scope characters of
                Just cs -> do
                  let animState = csAnimState cs
                      waitLoop = do
                        activeAnims <- readIORef (asActiveAnims animState)
                        let isRunning = any (\a -> animId' == Surface.animId (aaDef a)) activeAnims
                        when isRunning $ do
                          GLib.usleep 50000  -- Wait 50ms
                          waitLoop
                  waitLoop
                Nothing -> return ()
          , cbAnimClear    = \scope mAnimId -> do
              putStrLn $ "[Anim] Clear animations on scope " <> show scope
              case Map.lookup scope characters of
                Just cs -> do
                  let animState = csAnimState cs
                  case mAnimId of
                    Just aid -> do
                      activeAnims <- readIORef (asActiveAnims animState)
                      let newAnims = stopAnimation activeAnims aid
                      writeIORef (asActiveAnims animState) newAnims
                    Nothing -> clearAnimations animState
                Nothing -> return ()
          , cbBindToggle   = \scope category part enabled -> do
              putStrLn $ "[Bind] Toggle " <> T.unpack category <> "/" <> T.unpack part 
                       <> " = " <> show enabled <> " on scope " <> show scope
              case Map.lookup scope characters of
                Just cs -> do
                  let animState = csAnimState cs
                  -- Find animation ID by category/part name (simplified: use hash)
                  let animId' = abs $ fromIntegral $ T.length category * 1000 + T.length part
                  if enabled
                    then do
                      binds <- getEnabledBinds animState
                      unless (Set.member animId' binds) $
                        toggleBind animState animId'
                    else do
                      binds <- getEnabledBinds animState
                      when (Set.member animId' binds) $
                        toggleBind animState animId'
                Nothing -> return ()
          -- Move callbacks
          , cbMove         = \scope x y mTime async -> do
              putStrLn $ "[Move] Move scope " <> show scope <> " to (" <> show x <> ", " <> show y <> ")"
                       <> maybe "" (\t -> " in " <> show t <> "ms") mTime
                       <> if async then " (async)" else ""
              case Map.lookup scope characters of
                Just cs -> setCharacterPosition cs (fromIntegral x) (fromIntegral y)
                Nothing -> return ()
          -- Font callbacks
          , cbSetFont      = \fontCmd -> do
              b <- getCurrentBalloon
              case fontCmd of
                FontName name      -> setFontName b name
                FontHeight fontSize -> case fontSize of
                  FontSizeAbsolute size -> setFontSize b size
                  FontSizeRelative delta -> do
                    currentSize <- getFontSize b
                    setFontSize b (max 1 (currentSize + delta))
                  FontSizePercent pct -> do
                    let defaultSize = 12
                    setFontSize b (max 1 ((defaultSize * pct) `div` 100))
                  FontSizeDefault       -> setFontSize b 12  -- Default size
                FontColor color    -> case color of
                  ColorRGB r g b'  -> setFontColor b (fromIntegral r / 255.0) (fromIntegral g / 255.0) (fromIntegral b' / 255.0)
                  ColorRGBPercent r g b' -> setFontColor b r g b'
                  ColorHex hexStr  -> case parseHexColor hexStr of
                    Just (r, g, b') -> setFontColor b r g b'
                    Nothing -> return ()
                  ColorName name   -> case lookupColorName name of
                    Just (r, g, b') -> setFontColor b r g b'
                    Nothing -> return ()
                  ColorDefault     -> resetFont b
                  ColorDisable     -> return ()
                FontBold toggle    -> case toggle of
                  ToggleOn      -> setFontBold b True
                  ToggleOff     -> setFontBold b False
                  ToggleDefault -> setFontBold b False
                  ToggleDisable -> return ()
                FontItalic toggle  -> case toggle of
                  ToggleOn      -> setFontItalic b True
                  ToggleOff     -> setFontItalic b False
                  ToggleDefault -> setFontItalic b False
                  ToggleDisable -> return ()
                FontUnderline toggle -> case toggle of
                  ToggleOn      -> setFontUnderline b True
                  ToggleOff     -> setFontUnderline b False
                  ToggleDefault -> setFontUnderline b False
                  ToggleDisable -> return ()
                FontStrike toggle  -> case toggle of
                  ToggleOn      -> setFontStrike b True
                  ToggleOff     -> setFontStrike b False
                  ToggleDefault -> setFontStrike b False
                  ToggleDisable -> return ()
                FontDefault        -> resetFont b
                FontSub toggle     -> case toggle of
                  ToggleOn      -> setFontSub b True
                  ToggleOff     -> setFontSub b False
                  ToggleDefault -> setFontSub b False
                  ToggleDisable -> return ()
                FontSup toggle     -> case toggle of
                  ToggleOn      -> setFontSup b True
                  ToggleOff     -> setFontSup b False
                  ToggleDefault -> setFontSup b False
                  ToggleDisable -> return ()
                FontAlign _        -> return ()  -- Text alignment is handled at balloon render level
                FontVAlign _       -> return ()  -- Vertical alignment is handled at balloon render level
                FontShadowColor _  -> return ()  -- Shadow requires Pango attributes
                FontDisable _      -> return ()  -- Disable is a special render flag
                FontCursor _       -> return ()  -- Cursor color for input fields
                FontAnchorNormal _ -> return ()  -- Anchor styling stored in balloon state
                FontAnchorHover _  -> return ()  -- Anchor hover styling
                FontChoiceNormal _ -> return ()  -- Choice styling stored in balloon state
                FontChoiceHover _  -> return ()  -- Choice hover styling
          -- Sound callbacks
          , cbPlaySound    = \file -> playSound soundState file
          , cbStopSound    = stopSound soundState
          , cbSoundAction  = \action file -> do
              case action of
                SoundActionPlay      -> playSound soundState file
                SoundActionLoop      -> playSound soundState file  -- Loop not fully supported yet
                SoundActionPause     -> stopSound soundState
                SoundActionResume    -> playSound soundState file
                SoundActionSoundStop -> stopSound soundState
                SoundActionWait      -> return ()  -- Wait for sound completion
          -- Event callbacks
          , cbRaiseEvent   = \eventName refs -> do
              putStrLn $ "[Event] Raise: " <> T.unpack eventName <> " with refs: " <> show refs
              -- Event raising would send to SHIORI - logged for now
          , cbNotify       = \name refs -> do
              putStrLn $ "[Event] Notify: " <> T.unpack name <> " with refs: " <> show refs
              -- Notifications are one-way messages to SHIORI
          , cbTimerRaise   = \name delayMs -> do
              putStrLn $ "[Event] Timer raise: " <> T.unpack name <> " in " <> show delayMs <> "ms"
              -- Timer would use GLib.timeoutAdd - schedule for later
              void $ GLib.timeoutAdd GLib.PRIORITY_DEFAULT (fromIntegral delayMs) $ do
                putStrLn $ "[Event] Timer fired: " <> T.unpack name
                return False  -- Don't repeat
          , cbTimerCancel  = \name -> do
              putStrLn $ "[Event] Timer cancel: " <> T.unpack name
              -- Timer cancellation requires tracking timer IDs
          , cbGhostChange  = \ghostName -> do
              putStrLn $ "[Event] Ghost change: " <> T.unpack ghostName
              -- Ghost change requires reloading entire ghost
          , cbShellChange  = \shellName -> do
              putStrLn $ "[Event] Shell change: " <> T.unpack shellName
              -- Shell change requires reloading shell surfaces
          , cbBalloonStyleChange = \balloonName -> do
              putStrLn $ "[Event] Balloon style change: " <> T.unpack balloonName
              -- Balloon style change requires loading different balloon directory
          -- Open callbacks
          , cbOpenURL      = \url -> do
              putStrLn $ "[Open] URL: " <> T.unpack url
              void $ Gio.appInfoLaunchDefaultForUri url (Nothing :: Maybe Gio.AppLaunchContext)
          , cbOpenFile     = \file -> do
              putStrLn $ "[Open] File: " <> T.unpack file
              let uri = "file://" <> file
              void $ Gio.appInfoLaunchDefaultForUri uri (Nothing :: Maybe Gio.AppLaunchContext)
          , cbOpenInputBox = \eventId opts -> do
              putStrLn $ "[Open] Input box for event: " <> T.unpack eventId <> " opts: " <> show opts
              -- Create an input dialog using GTK
              dialog <- Gtk.new Gtk.Window [ #title := "Input"
                                           , #modal := True
                                           , #defaultWidth := 300
                                           , #defaultHeight := 100
                                           ]
              box <- Gtk.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                                     , #spacing := 10
                                     , #marginTop := 10
                                     , #marginBottom := 10
                                     , #marginStart := 10
                                     , #marginEnd := 10
                                     ]
              entry <- Gtk.new Gtk.Entry [ #placeholderText := "Enter text..." ]
              buttonBox <- Gtk.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                                           , #spacing := 10
                                           , #halign := Gtk.AlignEnd
                                           ]
              okBtn <- Gtk.new Gtk.Button [ #label := "OK" ]
              cancelBtn <- Gtk.new Gtk.Button [ #label := "Cancel" ]
              
              Gtk.boxAppend buttonBox okBtn
              Gtk.boxAppend buttonBox cancelBtn
              Gtk.boxAppend box entry
              Gtk.boxAppend box buttonBox
              Gtk.windowSetChild dialog (Just box)
              
              -- Handle OK button
              void $ Gtk.on okBtn #clicked $ do
                inputText <- Gtk.editableGetText entry
                putStrLn $ "[Open] Input result: " <> T.unpack inputText
                -- Send result back via event (would call SHIORI)
                Gtk.windowClose dialog
              
              -- Handle Cancel button  
              void $ Gtk.on cancelBtn #clicked $ do
                putStrLn "[Open] Input cancelled"
                Gtk.windowClose dialog
              
              Gtk.windowPresent dialog
              
          , cbOpenDialog   = \msg opt -> do
              putStrLn $ "[Open] Dialog: " <> T.unpack msg <> " opt: " <> show opt
              -- Create a message dialog
              dialog <- Gtk.new Gtk.Window [ #title := "Message"
                                           , #modal := True
                                           , #defaultWidth := 300
                                           , #defaultHeight := 150
                                           ]
              box <- Gtk.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                                     , #spacing := 10
                                     , #marginTop := 20
                                     , #marginBottom := 10
                                     , #marginStart := 20
                                     , #marginEnd := 20
                                     ]
              label <- Gtk.new Gtk.Label [ #label := msg
                                         , #wrap := True
                                         ]
              okBtn <- Gtk.new Gtk.Button [ #label := "OK"
                                          , #halign := Gtk.AlignCenter
                                          ]
              
              Gtk.boxAppend box label
              Gtk.boxAppend box okBtn
              Gtk.windowSetChild dialog (Just box)
              
              void $ Gtk.on okBtn #clicked $ Gtk.windowClose dialog
              
              Gtk.windowPresent dialog
          -- Meta callbacks
          , cbSetProperty  = \prop -> do
              putStrLn $ "[Meta] Set property: " <> show prop
              -- Property setting requires ghost state management
          , cbGetProperty  = \prop -> do
              putStrLn $ "[Meta] Get property: " <> show prop
              return ""  -- Property getting requires ghost state access
          , cbReload       = \target -> do
              putStrLn $ "[Meta] Reload: " <> show target
              -- Reload requires reloading specific components
          , cbExecute      = \execCmd -> do
              putStrLn $ "[Meta] Execute: " <> show execCmd
              -- Execute runs external commands
          , cbSetPassiveMode = \enabled -> do
              putStrLn $ "[Meta] Passive mode: " <> show enabled
              -- Passive mode affects event handling
          , cbLock         = \component -> do
              putStrLn $ "[Meta] Lock: " <> T.unpack component
              -- Lock prevents user interaction with component
          , cbUnlock       = \component -> do
              putStrLn $ "[Meta] Unlock: " <> T.unpack component
              -- Unlock restores user interaction
          -- EnvVar callback
          , cbGetEnvVar    = \envVar -> do
              now <- getCurrentTime
              let (year, month, day) = toGregorian (utctDay now)
                  TimeOfDay hour minute second = timeToTimeOfDay (utctDayTime now)
              case envVar of
                EnvYear        -> return $ T.pack $ show year
                EnvMonth       -> return $ T.pack $ show month
                EnvDay         -> return $ T.pack $ show day
                EnvHour        -> return $ T.pack $ show hour
                EnvMinute      -> return $ T.pack $ show minute
                EnvSecond      -> return $ T.pack $ show (truncate second :: Int)
                EnvWeekday     -> do
                  let (_, _, dow) = toWeekDate (utctDay now)
                  return $ T.pack $ ["日", "月", "火", "水", "木", "金", "土"] !! (dow `mod` 7)
                EnvSelfname    -> return "Emily"  -- Sakura name from ghost
                EnvSelfname2   -> return ""       -- Alternate sakura name
                EnvKeroname    -> return ""       -- Kero name from ghost
                EnvGhostname   -> return "Kokage Ghost"  -- Ghost name
                EnvShellname   -> return "master"  -- Current shell name
                EnvUsername    -> do
                  mUser <- lookupEnv "USER"
                  return $ T.pack $ fromMaybe "User" mUser
                EnvOS          -> return "Linux"
                EnvScreenWidth -> do
                  mDisplay <- Gdk.displayGetDefault
                  case mDisplay of
                    Nothing -> return "1920"
                    Just display -> do
                      monitors <- Gdk.displayGetMonitors display
                      n <- Gio.listModelGetNItems monitors
                      if n > 0
                        then do
                          mMonitor <- Gio.listModelGetItem monitors 0
                          case mMonitor of
                            Nothing -> return "1920"
                            Just obj -> do
                              monitor <- Gdk.unsafeCastTo Gdk.Monitor obj
                              geom <- Gdk.monitorGetGeometry monitor
                              w <- Gdk.getRectangleWidth geom
                              return $ T.pack $ show w
                        else return "1920"
                EnvScreenHeight -> do
                  mDisplay <- Gdk.displayGetDefault
                  case mDisplay of
                    Nothing -> return "1080"
                    Just display -> do
                      monitors <- Gdk.displayGetMonitors display
                      n <- Gio.listModelGetNItems monitors
                      if n > 0
                        then do
                          mMonitor <- Gio.listModelGetItem monitors 0
                          case mMonitor of
                            Nothing -> return "1080"
                            Just obj -> do
                              monitor <- Gdk.unsafeCastTo Gdk.Monitor obj
                              geom <- Gdk.monitorGetGeometry monitor
                              h <- Gdk.getRectangleHeight geom
                              return $ T.pack $ show h
                        else return "1080"
                EnvSurface     -> do
                  scope <- readIORef currentScopeRef
                  case Map.lookup scope characters of
                    Just cs -> do
                      surfId <- readIORef (csCurrentSurface cs)
                      return $ T.pack $ show surfId
                    Nothing -> return "0"
                EnvSurface0    -> do
                  case Map.lookup 0 characters of
                    Just cs -> do
                      surfId <- readIORef (csCurrentSurface cs)
                      return $ T.pack $ show surfId
                    Nothing -> return "0"
                EnvSurface1    -> do
                  case Map.lookup 1 characters of
                    Just cs -> do
                      surfId <- readIORef (csCurrentSurface cs)
                      return $ T.pack $ show surfId
                    Nothing -> return "0"
                EnvCustom name -> do
                  -- Custom environment variables - could be extended
                  putStrLn $ "[EnvVar] Custom: " <> T.unpack name
                  return ""
          -- Completion callbacks
          , cbOnComplete   = do
              putStrLn "[Script] Execution complete"
              hideBalloonIfNoChoices
          , cbOnInterrupt  = do
              putStrLn "[Script] Execution interrupted"
              hideBalloonIfNoChoices
          , cbOnClickWait  = do
              -- Click wait is handled by the FRP network
              -- The interpreter pauses until a click event is received
              putStrLn "[Script] Click wait triggered"
          }

    -- IORef to hold the current script's interrupt function
    currentScriptInterruptRef <- newIORef (return () :: IO ())

    -- Helper to display script in balloon with character-by-character animation
    let displayScript :: Maybe T.Text -> IO ()
        displayScript Nothing           = return ()
        displayScript (Just scriptText) = do
          -- Interrupt any currently running script first
          join $ readIORef currentScriptInterruptRef
          -- Reset scope to sakura (0) at start of each new script
          writeIORef currentScopeRef 0
          -- Parse the SakuraScript
          case parseScript scriptText of
            Left err     -> putStrLn $ "[Balloon] Parse error: " <> show err
            Right script -> do
              -- Clear all balloons before new script
              forM_ (Map.elems characters) $ \cs -> clearBalloon (getCharacterBalloon cs)
              -- Execute script asynchronously with animation
              interruptAction
                <- executeScriptAsync defaultInterpreterConfig interpreterCallbacks script
              -- Save the interrupt function for this script
              writeIORef currentScriptInterruptRef interruptAction

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
                Nothing   -> case T.stripPrefix "anchor:" action of
                  Just anchorId -> do
                    let refs = Map.fromList [ ( 0, anchorId ), ( 1, bcText choice ) ]
                    sendShioriWithCallback mShioriConfig OnAnchorSelect refs displayScript
                  Nothing       -> do
                    let refs = Map.fromList [ ( 0, bcId choice ), ( 1, bcText choice ) ]
                    sendShioriWithCallback mShioriConfig OnChoiceSelect refs displayScript
      Nothing     -> return ()

    -- Register "app.close" action - sends OnClose event to SHIORI, then quits
    closeAction <- Gio.simpleActionNew "close" Nothing
    _ <- on closeAction #activate $ \_ -> do
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
    Gio.actionMapAddAction app closeAction

    -- Create global timer event handlers
    ( secondTickHandler, fireSecondTick ) <- newAddHandler
    ( minuteTickHandler, fireMinuteTick ) <- newAddHandler
    ( hourTickHandler, fireHourTick ) <- newAddHandler
    ( motionTickHandler, fireMotionTick ) <- newAddHandler

    -- Helper to get current local time
    let getLocalTime' = do
          tz <- getCurrentTimeZone
          utc <- getCurrentTime
          return $ utcToLocalTime tz utc

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
          bs = getCharacterBalloon cs

      -- Create drag gesture (for left-click drag/move)
      dragGesture <- new Gtk.GestureDrag []
      _ <- on dragGesture #dragBegin $ \x y -> fireDragBegin ( x, y )
      _ <- on dragGesture #dragUpdate $ \ox oy -> fireDragUpdate ( ox, oy )
      _ <- on dragGesture #dragEnd $ \ox oy -> fireDragEnd ( ox, oy )
      Gtk.widgetAddController picture dragGesture

      -- Create left-click gesture (Button 1) for click count detection (single/double click)
      leftClickGesture <- new Gtk.GestureClick [ #button := 1 ]
      _ <- on leftClickGesture #pressed $ \nPress x y -> do
        fireLeftClick ( fromIntegral nPress, x, y )
      Gtk.widgetAddController picture leftClickGesture

      -- Create drag gesture for balloon
      balloonDragGesture <- new Gtk.GestureDrag []
      _ <- on balloonDragGesture #dragBegin $ \x y -> fireBalloonDragBegin ( x, y )
      _ <- on balloonDragGesture #dragUpdate $ \ox oy -> fireBalloonDragUpdate ( ox, oy )
      _ <- on balloonDragGesture #dragEnd $ \ox oy -> fireBalloonDragEnd ( ox, oy )
      Gtk.widgetAddController (bsDrawArea bs) balloonDragGesture

      -- Create left-click gesture for balloon
      balloonLeftClickGesture <- new Gtk.GestureClick [ #button := 1 ]
      _ <- on balloonLeftClickGesture #pressed $ \nPress x y -> do
        fireBalloonLeftClick ( fromIntegral nPress, x, y )
      Gtk.widgetAddController (bsDrawArea bs) balloonLeftClickGesture

      -- Create motion controller for balloon
      balloonMotionController <- new Gtk.EventControllerMotion []
      _ <- on balloonMotionController #motion $ \x y -> fireBalloonMotion ( x, y )
      Gtk.widgetAddController (bsDrawArea bs) balloonMotionController

      -- Create right-click gesture (Button 3) for context menu
      rightClickGesture <- new Gtk.GestureClick [ #button := 3 ]  -- Button 3 = right-click
      _ <- on rightClickGesture #pressed $ \_nPress x y -> do
        fireRightClick ( x, y )
      Gtk.widgetAddController picture rightClickGesture

      -- Create context menu for this character's window
      let menuStyle = menuStyleFromShellDescript (shellPath shell) (shellDescript shell)
      contextMenu <- createContextMenu window menuStyle

      -- Create motion controller
      motionController <- new Gtk.EventControllerMotion []
      _ <- on motionController #motion $ \x y -> fireMotion ( x, y )
      Gtk.widgetAddController picture motionController

      -- Create DropTarget for NAR file drops (only on sakura)
      when (scopeId == 0) $ do
        gfileType <- glibType @Gio.File
        dropTarget <- Gtk.dropTargetNew gfileType [ Gdk.DragActionCopy ]
        _ <- on dropTarget #drop $ \gvalue _x _y -> do
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
          collisions = case findSurfaceById currentSurfId surfaces of
            Nothing -> []
            Just sd -> sdCollisions sd

      -- Create move mode based on layer-shell status
      isLayerShell <- readIORef (csLayerShell cs)
      moveMode <- if isLayerShell
        then do
          let updatePosition :: Double -> Double -> IO ()
              updatePosition dx dy = do
                ( currentX, currentY ) <- readIORef (csPosition cs)
                let newX = currentX + round dx
                    newY = currentY + round dy
                writeIORef (csPosition cs) ( newX, newY )
                _ <- setWindowPosition window newX newY
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
                _ <- setWindowPosition (bsWindow bs) (fromIntegral newX) (fromIntegral newY)
                return ()
          return $ BalloonMoveLayerShell updateBalloonPos
        else do
          let beginBalloonMove :: Double -> Double -> IO ()
              beginBalloonMove x y = void $ runMaybeT $ do
                surface <- MaybeT $ Gtk.nativeGetSurface (bsWindow bs)
                toplevel <- MaybeT $ Gdk.castTo Gdk.Toplevel surface
                disp <- MaybeT Gdk.displayGetDefault
                seat <- MaybeT $ Gdk.displayGetDefaultSeat disp
                device <- MaybeT $ Gdk.seatGetPointer seat
                MaybeT $ pure <$> Gdk.toplevelBeginMove toplevel device 0 x y 0
          return $ BalloonMoveToplevel beginBalloonMove

      -- Build unified character+balloon network config
      let charConfig
            = CharacterNetworkConfig
            { cncWindow        = window
            , cncInputs        = InputHandlers
                { ihDragBegin  = dragBeginHandler
                , ihDragUpdate = dragUpdateHandler
                , ihDragEnd    = dragEndHandler
                , ihMotion     = motionHandler
                , ihRightClick = rightClickHandler
                , ihLeftClick  = leftClickHandler
                }
            , cncCollisions    = collisions
            , cncMoveMode      = moveMode
            , cncScopeId       = scopeId
            , cncShiori        = mShioriConfig
            , cncScriptHandler = displayScript
            , cncContextMenu   = contextMenu
            , cncMotionTick    = motionTickHandler
            -- Balloon integration
            , cncBalloonWindow   = bsWindow bs
            , cncBalloonInputs   = InputHandlers
                { ihDragBegin  = balloonDragBeginHandler
                , ihDragUpdate = balloonDragUpdateHandler
                , ihDragEnd    = balloonDragEndHandler
                , ihMotion     = balloonMotionHandler
                , ihRightClick = balloonRightClickHandler
                , ihLeftClick  = balloonLeftClickHandler
                }
            , cncBalloonMoveMode = balloonMoveMode
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
          setCharacterPosition cs (fst pos) (snd pos)
      Nothing
       -> putStrLn "[Position] Warning: Could not get screen geometry, using default positions"

    -- Show all characters and set always-on-top
    forM_ (Map.elems characters) showCharacter

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
  _ <- Gio.applicationRun app Nothing
  return ()

-- | Get the default base directories for NAR installation
-- Uses XDG data directory (~/.local/share/kokage/)
getDefaultBaseDir :: IO BaseDir
getDefaultBaseDir = do
  cwd <- getCurrentDirectory
  return
    BaseDir { bdGhost        = cwd </> "ghost"
            , bdBalloon      = cwd </> "balloon"
            , bdPlugin       = cwd </> "plugin"
            , bdHeadline     = cwd </> "headline"
            , bdCalendar     = cwd </> "calendar"
            , bdCalendarSkin = cwd </> "calendar" </> "skin"
            }

-- | Parse a hex color string like "#FF0000" or "FF0000" to RGB values (0.0-1.0)
parseHexColor :: T.Text -> Maybe (Double, Double, Double)
parseHexColor hexStr = do
  let hex = T.dropWhile (== '#') hexStr
  case T.length hex of
    6 -> do
      r <- parseHexByte (T.take 2 hex)
      g <- parseHexByte (T.take 2 (T.drop 2 hex))
      b <- parseHexByte (T.drop 4 hex)
      Just (fromIntegral r / 255.0, fromIntegral g / 255.0, fromIntegral b / 255.0)
    3 -> do
      -- Short form like "F00" -> "FF0000"
      r <- parseHexNibble (T.take 1 hex)
      g <- parseHexNibble (T.take 1 (T.drop 1 hex))
      b <- parseHexNibble (T.drop 2 hex)
      Just (fromIntegral (r * 17) / 255.0, fromIntegral (g * 17) / 255.0, fromIntegral (b * 17) / 255.0)
    _ -> Nothing
  where
    parseHexByte :: T.Text -> Maybe Int
    parseHexByte t = case reads ("0x" ++ T.unpack t) of
      [(n, "")] -> Just n
      _         -> Nothing
    parseHexNibble :: T.Text -> Maybe Int
    parseHexNibble t = case reads ("0x" ++ T.unpack t) of
      [(n, "")] -> Just n
      _         -> Nothing

-- | Look up a named color to RGB values (0.0-1.0)
lookupColorName :: T.Text -> Maybe (Double, Double, Double)
lookupColorName name = Map.lookup (T.toLower name) colorNames
  where
    colorNames :: Map.Map T.Text (Double, Double, Double)
    colorNames = Map.fromList
      [ ("black",   (0.0, 0.0, 0.0))
      , ("white",   (1.0, 1.0, 1.0))
      , ("red",     (1.0, 0.0, 0.0))
      , ("green",   (0.0, 0.5, 0.0))
      , ("blue",    (0.0, 0.0, 1.0))
      , ("yellow",  (1.0, 1.0, 0.0))
      , ("cyan",    (0.0, 1.0, 1.0))
      , ("magenta", (1.0, 0.0, 1.0))
      , ("gray",    (0.5, 0.5, 0.5))
      , ("grey",    (0.5, 0.5, 0.5))
      , ("orange",  (1.0, 0.65, 0.0))
      , ("pink",    (1.0, 0.75, 0.8))
      , ("purple",  (0.5, 0.0, 0.5))
      , ("brown",   (0.65, 0.16, 0.16))
      , ("navy",    (0.0, 0.0, 0.5))
      , ("teal",    (0.0, 0.5, 0.5))
      , ("olive",   (0.5, 0.5, 0.0))
      , ("maroon",  (0.5, 0.0, 0.0))
      , ("lime",    (0.0, 1.0, 0.0))
      , ("aqua",    (0.0, 1.0, 1.0))
      , ("silver",  (0.75, 0.75, 0.75))
      , ("fuchsia", (1.0, 0.0, 1.0))
      ]
