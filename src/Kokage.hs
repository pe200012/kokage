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

import           Control.Exception          ( try, SomeException, finally )
import           Control.Monad              ( forM_, void, filterM, when )
import           Control.Monad.Trans.Class  ( lift )
import           Control.Monad.Trans.Maybe  ( MaybeT(runMaybeT, MaybeT) )

import           Data.GI.Base               ( AttrOp((:=)), new, on, glibType
                                            , withManagedPtr, newObject, castTo )
import           Data.GI.Base.GValue        ( get_object )
import           Data.Int                   ( Int32 )
import           Data.IORef                 ( newIORef, readIORef, writeIORef )
import           Foreign.Ptr                ( Ptr, nullPtr )
import           Data.List                  ( sort )
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 ( listToMaybe )
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Time                  ( getCurrentTime, utcToLocalTime, getCurrentTimeZone )

import qualified GI.Gdk                     as Gdk
import qualified GI.GdkPixbuf               as Pixbuf
import qualified GI.Gio                     as Gio
import qualified GI.GLib                    as GLib
import qualified GI.Gtk                     as Gtk

import           Kokage.Balloon             ( clearBalloon, appendText, appendChar, appendNewline
                                            , BalloonChoice(..), addChoice, clearChoices, setChoiceCallback
                                            , BalloonState(..) )
import           Kokage.Character           ( CharacterState(..)
                                            , createCharacter, showCharacter
                                            , setCharacterSurface, getCharacterBalloon
                                            , setCharacterPosition
                                            , updateBalloonPosition )
import           Kokage.Collision
import           Kokage.Event               ( InputHandlers(..)
                                            , TimerHandlers(..), MoveMode(..)
                                            , ShioriConfig(..)
                                            , CharacterNetworkConfig(..)
                                            , GlobalNetworkConfig(..)
                                            , BalloonNetworkConfig(..), BalloonMoveMode(..)
                                            , setupCharacterNetwork, setupGlobalNetwork
                                            , setupBalloonNetwork
                                            , sendShioriWithCallback )
import           Kokage.Install             ( BaseDir(..), InstallResult(..), installNar )
import           Kokage.SakuraScript.Parser ( parseScript )
import           Kokage.SakuraScript.Interpreter ( InterpreterCallbacks(..)
                                              , defaultInterpreterConfig, defaultCallbacks
                                              , executeScriptAsync )
import           Kokage.Shiori.WineBridge   ( WineShiori(..)
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
                                            )
import           Kokage.Surface
import           Kokage.Platform            ( isLayerShellSupported, initPlatformWindow
                                            , isPlatformInitialized, setWindowLayer
                                            , setWindowPosition, getWindowPosition
                                            , setWindowAlwaysOnTop
                                            , Layer(..), Edge(..) )

import           Reactive.Banana            ( compile )
import           Reactive.Banana.Frameworks ( actuate, newAddHandler )

import           System.Directory           ( createDirectoryIfMissing
                                            , doesDirectoryExist
                                            , doesFileExist
                                            , getCurrentDirectory
                                            , getXdgDirectory
                                            , listDirectory
                                            , XdgDirectory(..)
                                            )
import           System.FilePath            ( takeExtension, (</>) )

import           Types.Ghost                ( CollisionRegion(..)
                                            , Ghost(..)
                                            , GhostDescript(..)
                                            , Shell(..)
                                            , SurfaceDefinition(..)
                                            , loadGhost
                                            )
import           Types.Shiori               ( ShioriEvent(..) )

-- | Configuration for the Kokage application.
data KokageConfig
  = KokageConfig { configGhostPath   :: !(Maybe FilePath)  -- ^ Explicit ghost path (overrides lastGhost)
                 , configLastGhost   :: !(Maybe FilePath)  -- ^ Last used ghost path (from saved state)
                 , configBaseDir     :: !BaseDir           -- ^ Base directories for ghosts, balloons, etc.
                 , configSurfaceId   :: !Int               -- ^ Initial surface ID to display
                 , configDataDir     :: !FilePath          -- ^ Data directory for config files
                 }
  deriving ( Show, Eq )

-- | Default configuration.
-- Uses current working directory for base directories.
defaultConfig :: IO KokageConfig
defaultConfig = do
  cwd <- getCurrentDirectory
  dataDir <- getXdgDirectory XdgData "kokage"
  let baseDir = BaseDir
        { bdGhost = cwd </> "ghost"
        , bdBalloon = cwd </> "balloon"
        , bdPlugin = cwd </> "plugin"
        , bdHeadline = cwd </> "headline"
        , bdCalendar = cwd </> "calendar"
        , bdCalendarSkin = cwd </> "calendar" </> "skin"
        }
  return KokageConfig
    { configGhostPath = Nothing
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
getScreenDimensions :: IO (Maybe (Int32, Int32))
getScreenDimensions = runMaybeT $ do
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
      w <- lift $ Gdk.getRectangleWidth rect
      h <- lift $ Gdk.getRectangleHeight rect
      return (w, h)

-- | Calculate initial position for a character based on descript settings and screen size.
-- For sakura (scope 0): positioned at right side of screen (bottom-right by default)
-- For kero (scope 1): positioned to the left of sakura
-- Returns position in display coordinates (x, y).
calcInitialPosition :: GhostDescript -> Int -> (Int, Int) -> (Int32, Int32) -> (Int32, Int32)
calcInitialPosition descript scopeId (surfW, surfH) (screenW, screenH) =
  case scopeId of
    0 -> -- Sakura: bottom-right of screen
      let x = maybe (screenW - fromIntegral surfW) fromIntegral (descriptSakuraDefaultLeft descript)
          y = maybe (screenH - fromIntegral surfH) fromIntegral (descriptSakuraDefaultTop descript)
      in (x, y)
    _ -> -- Kero and others: default to left of sakura position
      let -- Default kero position: left of sakura, same bottom alignment
          defaultX = screenW - fromIntegral surfW - 300  -- 300px left of sakura
          defaultY = screenH - fromIntegral surfH
          x = maybe defaultX fromIntegral (descriptKeroDefaultLeft descript)
          y = maybe defaultY fromIntegral (descriptKeroDefaultTop descript)
      in (x, y)

--------------------------------------------------------------------------------
-- Ghost Management
--------------------------------------------------------------------------------

-- | Scan for available ghosts in the ghost directory.
-- Returns a sorted list of ghost directory paths that contain valid ghost structure.
-- A valid ghost has a 'ghost/master' subdirectory.
scanGhosts :: BaseDir -> IO [FilePath]
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
    Nothing -> tryLastGhost
  where
    tryLastGhost = case configLastGhost config of
      Just path -> do
        valid <- isValidGhostDir path
        if valid
          then return $ Just path
          else tryFirstGhost
      Nothing -> tryFirstGhost

    tryFirstGhost = do
      ghosts <- scanGhosts (configBaseDir config)
      return $ listToMaybe ghosts

-- | Save the last used ghost path to persistent storage.
saveLastGhost :: KokageConfig -> FilePath -> IO ()
saveLastGhost config gPath = do
  let dataDir = configDataDir config
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
  = GhostHistory
  { ghTime          :: !Int   -- ^ Total runtime in hours (cumulative)
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
parseHistory content =
  let ls = T.lines content
      pairs = [ (key, val)
              | l <- ls
              , let stripped = T.strip l
              , not (T.null stripped)
              , (key, rest) <- [T.breakOn "," stripped]
              , let val = T.strip $ T.drop 1 rest
              ]
      lookupInt k def = case lookup (T.toLower $ T.strip k) pairs of
        Nothing -> def
        Just v  -> case reads (T.unpack v) of
          [(n, "")] -> n
          _         -> def
  in GhostHistory
    { ghTime          = lookupInt "time" 0
    , ghVanishedCount = lookupInt "vanished_count" 0
    }

-- | Save ghost history to HISTORY file.
saveGhostHistory :: FilePath -> GhostHistory -> IO ()
saveGhostHistory ghostPath history = do
  let historyFile = historyFilePath ghostPath
      content = T.unlines
        [ "time, " <> T.pack (show (ghTime history))
        , "vanished_count, " <> T.pack (show (ghVanishedCount history))
        ]
  result <- try $ TIO.writeFile historyFile content
  case result of
    Left (e :: SomeException) ->
      putStrLn $ "[HISTORY] Warning: Could not save history: " <> show e
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
            [] -> do
              putStrLn "[Balloon] No valid balloons in global directory"
              return Nothing
            (first:_) -> do
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
  let surfId = configSurfaceId config

  -- Resolve which ghost to load
  mGhostPath <- resolveGhost config

  case mGhostPath of
    Nothing -> do
      putStrLn "Error: No ghosts available"
      putStrLn $ "  Ghost directory: " <> bdGhost (configBaseDir config)
      putStrLn "  Please install a ghost first (drag and drop a .nar file)"

    Just gPath -> do
      putStrLn $ "Loading ghost from: " <> gPath
      mGhost <- loadGhost gPath

      case mGhost of
        Nothing -> putStrLn $ "Error: Failed to load ghost from " <> gPath
        Just ghost -> do
          putStrLn $ "Loaded ghost: " <> gPath

          -- Save this as the last used ghost
          saveLastGhost config gPath

          -- Get default shell
          case getDefaultShell ghost of
            Nothing -> putStrLn "Error: No shells found in ghost"
            Just shell -> do
              putStrLn $ "Using shell: " <> shellPath shell

              -- Find requested surface
              let surfaces = shellSurfaces shell
              case findSurfaceById surfId surfaces of
                Nothing -> putStrLn $ "Error: Surface " <> show surfId <> " not found"
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
                  mPixbuf <- compositeSurface (shellPath shell) surfDef

                  case mPixbuf of
                    Nothing -> putStrLn "Error: Failed to composite surface"
                    Just pixbuf -> do
                      width <- Pixbuf.pixbufGetWidth pixbuf
                      height <- Pixbuf.pixbufGetHeight pixbuf
                      putStrLn $ "Composited surface: " <> show width <> "x" <> show height

                      -- Try to initialize SHIORI (optional - ghost can run without it)
                      -- Use the shiori path from ghost's descript.txt
                      let ghostMasterPath = gPath </> "ghost" </> "master"
                          shioriName = descriptShiori (ghostDescript ghost)
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
      let bridgeConfig = defaultWineBridgeConfig
            { wbcBridgePath = "wine-helper" </> "shiori_bridge32.exe"
            }

      -- Start the Wine bridge
      putStrLn "[SHIORI] Starting Wine bridge..."
      bridgeResult <- startWineBridge bridgeConfig
      case bridgeResult of
        Left err -> do
          putStrLn $ "[SHIORI] Failed to start bridge: " <> err
          return Nothing
        Right shiori -> do
          putStrLn "[SHIORI] Bridge started, loading DLL..."
          loadResult <- loadShiori shiori dllPath ghostMasterPath
          case loadResult of
            Left err -> do
              putStrLn $ "[SHIORI] Failed to load DLL: " <> err
              stopWineBridge shiori
              return Nothing
            Right loadedShiori -> do
              putStrLn "[SHIORI] DLL loaded successfully"
              return $ Just loadedShiori

-- | Clean up SHIORI bridge on exit.
cleanupShiori :: Maybe WineShiori -> IO ()
cleanupShiori Nothing = return ()
cleanupShiori (Just shiori) = do
  putStrLn "[SHIORI] Unloading DLL..."
  _ <- unloadShiori shiori
  putStrLn "[SHIORI] Stopping bridge..."
  stopWineBridge shiori
  putStrLn "[SHIORI] Cleanup complete"

-- | Run the GTK application with the given shell.
-- The shell contains surface definitions for dynamic surface switching.
-- Now uses CharacterMap for multi-character support.
runGtkApp :: Ghost -> Shell -> Int -> Maybe WineShiori -> FilePath -> Bool -> Int -> Maybe FilePath -> IO ()
runGtkApp ghost shell initialSurfaceId mShiori ghostPath' firstBoot vanishedCount mBalloonDir = do
  -- Get start time for uptime tracking
  startTime <- getCurrentTime

  let ghostDesc = ghostDescript ghost

  -- Create SHIORI config if we have a bridge
  let mShioriConfig = case mShiori of
        Nothing -> Nothing
        Just ws -> Just $ ShioriConfig
          { scShiori    = ws
          , scSurfaceId = initialSurfaceId
          , scStartTime = startTime
          , scGhostPath = ghostPath'
          }

  -- Initialize GTK application
  app <- new
    Gtk.Application
    [ #applicationId := "com.kokage.app", #flags := [ Gio.ApplicationFlagsFlagsNone ] ]

  -- Connect activate signal
  _ <- on app #activate $ do
    -- Create characters using the Character module
    -- Scope 0 = Sakura, Scope 1 = Kero
    -- Pass balloon directory so each character loads the correct balloon surface
    mSakura <- createCharacter app shell ghostDesc 0 mBalloonDir
    mKero <- createCharacter app shell ghostDesc 1 mBalloonDir

    -- Build character map from successfully created characters
    let characters = Map.fromList $ concat
          [ maybe [] (\c -> [(0, c)]) mSakura
          , maybe [] (\c -> [(1, c)]) mKero
          ]

    when (Map.null characters) $ do
      putStrLn "Error: No characters could be created"
      return ()

    -- Track current character scope (0=sakura, 1=kero, etc.)
    currentScopeRef <- newIORef (0 :: Int)

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

    -- Create interpreter callbacks that interact with the balloon and surface
    let interpreterCallbacks = defaultCallbacks
          { cbAppendChar    = \c -> getCurrentBalloon >>= \b -> appendChar b c
          , cbAppendText    = \t -> getCurrentBalloon >>= \b -> appendText b t
          , cbNewline       = getCurrentBalloon >>= appendNewline
          , cbClear         = getCurrentBalloon >>= clearBalloon
          , cbSetScope      = \scope -> do
              writeIORef currentScopeRef scope
              putStrLn $ "[Scope] Switched to scope " <> show scope
          , cbSetSurface    = changeSurface
          , cbAddChoice     = \choiceId text action -> do
              b <- getCurrentBalloon
              addChoice b (BalloonChoice text choiceId action)
          , cbClearChoices  = getCurrentBalloon >>= clearChoices
          , cbOnComplete    = putStrLn "[Script] Execution complete"
          , cbOnInterrupt   = putStrLn "[Script] Execution interrupted"
          }

    -- Helper to display script in balloon with character-by-character animation
    let displayScript :: Maybe T.Text -> IO ()
        displayScript Nothing = return ()
        displayScript (Just scriptText) = do
          -- Reset scope to sakura (0) at start of each new script
          writeIORef currentScopeRef 0
          -- Parse the SakuraScript
          case parseScript scriptText of
            Left err -> putStrLn $ "[Balloon] Parse error: " <> show err
            Right script -> do
              -- Clear all balloons before new script
              forM_ (Map.elems characters) $ \cs ->
                clearBalloon (getCharacterBalloon cs)
              -- Execute script asynchronously with animation
              _ <- executeScriptAsync defaultInterpreterConfig interpreterCallbacks script
              return ()

    -- Set up choice callback on sakura's balloon
    case Map.lookup 0 characters of
      Just sakura -> do
        let sakuraBalloon = getCharacterBalloon sakura
        setChoiceCallback sakuraBalloon $ \choice -> do
          putStrLn $ "[Choice] Selected: " <> T.unpack (bcText choice)
                  <> " (id=" <> T.unpack (bcId choice)
                  <> ", action=" <> T.unpack (bcAction choice) <> ")"
          -- Clear all balloons and choices after selection
          forM_ (Map.elems characters) $ \cs -> do
            clearBalloon (getCharacterBalloon cs)
            clearChoices (getCharacterBalloon cs)
          -- Handle the action based on its type
          let action = bcAction choice
          case T.stripPrefix "event:" action of
            Just _eventId -> do
              let refs = Map.fromList [(0, bcId choice), (1, bcText choice)]
              sendShioriWithCallback mShioriConfig OnChoiceSelect refs displayScript
            Nothing -> case T.stripPrefix "script:" action of
              Just scriptText -> displayScript (Just scriptText)
              Nothing -> case T.stripPrefix "url:" action of
                Just _url -> putStrLn "[Choice] URL action not yet implemented"
                Nothing -> case T.stripPrefix "anchor:" action of
                  Just anchorId -> do
                    let refs = Map.fromList [(0, anchorId), (1, bcText choice)]
                    sendShioriWithCallback mShioriConfig OnAnchorSelect refs displayScript
                  Nothing -> do
                    let refs = Map.fromList [(0, bcId choice), (1, bcText choice)]
                    sendShioriWithCallback mShioriConfig OnChoiceSelect refs displayScript
      Nothing -> return ()

    -- Create global timer event handlers
    ( secondTickHandler, fireSecondTick ) <- newAddHandler
    ( minuteTickHandler, fireMinuteTick ) <- newAddHandler

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

    -- Set up global timer FRP network
    let globalConfig = GlobalNetworkConfig
          { gncTimers = TimerHandlers
              { thSecondTick = secondTickHandler
              , thMinuteTick = minuteTickHandler
              }
          , gncShiori = mShioriConfig
          }
    globalNetwork <- compile (setupGlobalNetwork globalConfig)
    actuate globalNetwork

    -- Set up FRP network for each character window
    forM_ (Map.toList characters) $ \(scopeId, cs) -> do
      -- Create input handlers for this character's window
      ( dragBeginHandler, fireDragBegin ) <- newAddHandler
      ( dragUpdateHandler, fireDragUpdate ) <- newAddHandler
      ( dragEndHandler, fireDragEnd ) <- newAddHandler
      ( motionHandler, fireMotion ) <- newAddHandler

      let window = csWindow cs
          picture = csPicture cs

      -- Create drag gesture
      dragGesture <- new Gtk.GestureDrag []
      _ <- on dragGesture #dragBegin $ \x y -> fireDragBegin ( x, y )
      _ <- on dragGesture #dragUpdate $ \ox oy -> fireDragUpdate ( ox, oy )
      _ <- on dragGesture #dragEnd $ \ox oy -> fireDragEnd ( ox, oy )
      Gtk.widgetAddController picture dragGesture

      -- Create motion controller
      motionController <- new Gtk.EventControllerMotion []
      _ <- on motionController #motion $ \x y -> fireMotion ( x, y )
      Gtk.widgetAddController picture motionController

      -- Create DropTarget for NAR file drops (only on sakura)
      when (scopeId == 0) $ do
        gfileType <- glibType @Gio.File
        dropTarget <- Gtk.dropTargetNew gfileType [Gdk.DragActionCopy]
        _ <- on dropTarget #drop $ \gvalue _x _y -> do
          mPath <- withManagedPtr gvalue $ \gvPtr -> do
            objPtr <- get_object gvPtr :: IO (Ptr Gio.File)
            if objPtr == nullPtr
              then return Nothing
              else do
                file <- newObject Gio.File objPtr
                Gio.fileGetPath file
          case mPath of
            Nothing -> do
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
                      putStrLn $ "Installed " <> T.unpack name
                              <> " (" <> show itype <> ") to " <> ipath
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
      let surfaces = shellSurfaces shell
          collisions = case findSurfaceById currentSurfId surfaces of
            Nothing -> []
            Just sd -> sdCollisions sd

      -- Create move mode based on layer-shell status
      isLayerShell <- readIORef (csLayerShell cs)
      moveMode <- if isLayerShell
        then do
          let updatePosition :: Double -> Double -> IO ()
              updatePosition dx dy = do
                (currentX, currentY) <- readIORef (csPosition cs)
                let newX = currentX + round dx
                    newY = currentY + round dy
                writeIORef (csPosition cs) (newX, newY)
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

      -- Build character network config
      let charConfig = CharacterNetworkConfig
            { cncWindow     = window
            , cncInputs     = InputHandlers
                { ihDragBegin  = dragBeginHandler
                , ihDragUpdate = dragUpdateHandler
                , ihDragEnd    = dragEndHandler
                , ihMotion     = motionHandler
                }
            , cncCollisions = collisions
            , cncMoveMode   = moveMode
            , cncScopeId    = scopeId
            , cncShiori     = mShioriConfig
            }

      -- Compile and activate character network
      charNetwork <- compile (setupCharacterNetwork charConfig)
      actuate charNetwork

      putStrLn $ "[Character " <> show scopeId <> "] FRP network activated"

      -- Set up balloon FRP network for this character's balloon
      let bs = getCharacterBalloon cs

      -- Create input handlers for balloon window
      ( balloonDragBeginHandler, fireBalloonDragBegin ) <- newAddHandler
      ( balloonDragUpdateHandler, fireBalloonDragUpdate ) <- newAddHandler
      ( balloonDragEndHandler, fireBalloonDragEnd ) <- newAddHandler
      ( balloonMotionHandler, fireBalloonMotion ) <- newAddHandler

      -- Create drag gesture for balloon
      balloonDragGesture <- new Gtk.GestureDrag []
      _ <- on balloonDragGesture #dragBegin $ \x y -> fireBalloonDragBegin ( x, y )
      _ <- on balloonDragGesture #dragUpdate $ \ox oy -> fireBalloonDragUpdate ( ox, oy )
      _ <- on balloonDragGesture #dragEnd $ \ox oy -> fireBalloonDragEnd ( ox, oy )
      Gtk.widgetAddController (bsDrawArea bs) balloonDragGesture

      -- Create motion controller for balloon (for future hover effects, etc.)
      balloonMotionController <- new Gtk.EventControllerMotion []
      _ <- on balloonMotionController #motion $ \x y -> fireBalloonMotion ( x, y )
      Gtk.widgetAddController (bsDrawArea bs) balloonMotionController

      -- Create balloon move mode based on layer-shell status
      balloonIsLayerShell <- readIORef (bsLayerShell bs)
      balloonMoveMode <- if balloonIsLayerShell
        then do
          let updatePosition :: Double -> Double -> IO ()
              updatePosition dx dy = do
                (currentX, currentY) <- readIORef (bsPosition bs)
                let newX = currentX + round dx
                    newY = currentY + round dy
                writeIORef (bsPosition bs) (newX, newY)
                _ <- setWindowPosition (bsWindow bs) (fromIntegral newX) (fromIntegral newY)
                return ()
          return $ BalloonMoveLayerShell updatePosition
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

      -- Build balloon network config
      let balloonConfig = BalloonNetworkConfig
            { bncWindow      = bsWindow bs
            , bncInputs      = InputHandlers
                { ihDragBegin  = balloonDragBeginHandler
                , ihDragUpdate = balloonDragUpdateHandler
                , ihDragEnd    = balloonDragEndHandler
                , ihMotion     = balloonMotionHandler
                }
            , bncMoveMode    = balloonMoveMode
            }

      -- Compile and activate balloon network
      balloonNetwork <- compile (setupBalloonNetwork balloonConfig)
      actuate balloonNetwork

      putStrLn $ "[Character " <> show scopeId <> "] Balloon FRP network activated"

    -- Set initial positions for characters based on descript and screen size
    mScreenDims <- getScreenDimensions
    case mScreenDims of
      Just screenDims -> do
        putStrLn $ "[Position] Screen dimensions: " <> show screenDims
        forM_ (Map.toList characters) $ \(scopeId, cs) -> do
          surfSize <- readIORef (csSurfaceSize cs)
          let pos = calcInitialPosition ghostDesc scopeId surfSize screenDims
          putStrLn $ "[Position] Character " <> show scopeId <> " initial position: " <> show pos
          setCharacterPosition cs (fst pos) (snd pos)
      Nothing ->
        putStrLn "[Position] Warning: Could not get screen dimensions, using default positions"

    -- Show all characters and set always-on-top
    forM_ (Map.elems characters) showCharacter

    -- Position balloons relative to their characters
    -- This needs a small delay to ensure windows are realized
    _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 100 $ do
      forM_ (Map.elems characters) $ \cs ->
        -- updateBalloonPosition cs dx dy
        -- TODO: Calculate initial offsets properly
        updateBalloonPosition cs 0 0
      return False  -- Don't repeat

    -- Send OnBoot or OnFirstBoot event
    if firstBoot
      then do
        let refs = Map.fromList [(0, T.pack $ show vanishedCount)]
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
  return BaseDir
    { bdGhost = cwd </> "ghost"
    , bdBalloon = cwd </> "balloon"
    , bdPlugin = cwd </> "plugin"
    , bdHeadline = cwd </> "headline"
    , bdCalendar = cwd </> "calendar"
    , bdCalendarSkin = cwd </> "calendar" </> "skin"
    }
