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
  , setWindowAboveFromGtk
    -- * Wayland Layer Shell Support (always-on-top)
  , isLayerShellSupported
  , initLayerShell
  , isLayerShellWindow
  , setWindowLayer
  , setLayerShellPosition
  , getLayerShellPosition
  , Layer(..)
  , Edge(..)
  ) where

import           Control.Exception          ( try, SomeException, finally )
import           Control.Monad              ( forM_, void, filterM, when )
import           Control.Monad.Trans.Maybe  ( MaybeT(runMaybeT, MaybeT) )

import           Data.GI.Base               ( AttrOp((:=)), new, on, glibType
                                            , withManagedPtr, newObject )
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

import           Kokage.Balloon             ( newBalloonState, initBalloonAlwaysOnTop
                                            , clearBalloon, appendText, appendChar, appendNewline
                                            , BalloonChoice(..), addChoice, clearChoices, setChoiceCallback )
import           Kokage.InputRegion         ( setInputRegionFromPixbuf )
import           Kokage.Collision
import           Kokage.Event               ( NetworkConfig(..), InputHandlers(..)
                                            , TimerHandlers(..), MoveMode(..)
                                            , ShioriConfig(..)
                                            , setupNetwork, sendShioriWithCallback )
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
import           Kokage.LayerShell          ( isLayerShellSupported, initLayerShell
                                            , isLayerShellWindow, setWindowLayer
                                            , setLayerShellPosition, getLayerShellPosition
                                            , Layer(..), Edge(..) )
import           Kokage.X11                 ( setWindowAboveFromGtk )

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

                      -- Run the GTK application with shell (for surface switching)
                      runGtkApp shell surfId mShiori gPath firstBoot vanishedCount
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
runGtkApp :: Shell -> Int -> Maybe WineShiori -> FilePath -> Bool -> Int -> IO ()
runGtkApp shell initialSurfaceId mShiori ghostPath firstBoot vanishedCount = do
  -- Get start time for uptime tracking
  startTime <- getCurrentTime
  
  -- Find initial surface definition
  let surfaces = shellSurfaces shell
  case findSurfaceById initialSurfaceId surfaces of
    Nothing -> putStrLn $ "Error: Initial surface " <> show initialSurfaceId <> " not found"
    Just initialSurfDef -> do
      -- Composite initial surface
      mInitialPixbuf <- compositeSurface (shellPath shell) initialSurfDef
      case mInitialPixbuf of
        Nothing -> putStrLn "Error: Failed to composite initial surface"
        Just initialPixbuf -> do
          width <- Pixbuf.pixbufGetWidth initialPixbuf
          height <- Pixbuf.pixbufGetHeight initialPixbuf
          
          -- Create SHIORI config if we have a bridge
          let mShioriConfig = case mShiori of
                Nothing -> Nothing
                Just ws -> Just $ ShioriConfig 
                  { scShiori    = ws
                  , scSurfaceId = initialSurfaceId
                  , scStartTime = startTime
                  , scGhostPath = ghostPath
                  }
                
          -- Initialize GTK application
          app <- new
            Gtk.Application
            [ #applicationId := "com.kokage.app", #flags := [ Gio.ApplicationFlagsFlagsNone ] ]

          -- Connect activate signal
          _ <- on app #activate $ do
            -- Create window
            window <- new
              Gtk.Window
              [ #application := app
              , #title := "Kokage"
              , #defaultWidth := width
              , #defaultHeight := height
              , #resizable := False
              , #decorated := False  -- Hide title bar
              ]

            -- Make window transparent using CSS
            cssProvider <- new Gtk.CssProvider []
            Gtk.cssProviderLoadFromString
              cssProvider
              "window.transparent { background-color: transparent; }"
            display <- Gdk.displayGetDefault
            case display of
              Nothing -> putStrLn "Warning: Could not get default display"
              Just d  -> Gtk.styleContextAddProviderForDisplay d cssProvider 800
            Gtk.widgetAddCssClass window "transparent"

            -- Create texture from pixbuf for GTK4
            texture <- Gdk.textureNewForPixbuf initialPixbuf

            -- Create picture widget to display the texture
            picture <- new Gtk.Picture [ #paintable := texture, #canShrink := False ]
            
            -- Track current surface ID for each scope (sakura=0, kero=1, etc.)
            -- For now we only handle scope 0 (main character)
            currentSurfaceRef <- newIORef initialSurfaceId
            
            -- Surface change function - updates the displayed surface
            -- IMPORTANT: This may be called from a background thread (script interpreter),
            -- so we must schedule GTK operations on the main thread using idleAdd.
            let changeSurface :: Int -> Int -> IO ()
                changeSurface _scope newSurfaceId = do
                  currentId <- readIORef currentSurfaceRef
                  when (currentId /= newSurfaceId) $ do
                    putStrLn $ "[Surface] Changing from " <> show currentId <> " to " <> show newSurfaceId
                    case findSurfaceById newSurfaceId surfaces of
                      Nothing -> putStrLn $ "[Surface] Surface " <> show newSurfaceId <> " not found"
                      Just newSurfDef -> do
                        -- Composite the new surface (can be done on any thread)
                        mNewPixbuf <- compositeSurface (shellPath shell) newSurfDef
                        case mNewPixbuf of
                          Nothing -> putStrLn "[Surface] Failed to composite new surface"
                          Just newPixbuf -> do
                            -- Schedule GTK operations on the main thread
                            _ <- GLib.idleAdd GLib.PRIORITY_HIGH $ do
                              -- Update the picture widget with new texture
                              newTexture <- Gdk.textureNewForPixbuf newPixbuf
                              Gtk.pictureSetPaintable picture (Just newTexture)
                              -- Update input region for click-through
                              applyInputRegion window newPixbuf
                              -- Track new surface ID
                              writeIORef currentSurfaceRef newSurfaceId
                              putStrLn $ "[Surface] Changed to surface " <> show newSurfaceId
                              return False  -- Don't repeat
                            return ()

            -- Create all event handlers BEFORE creating gestures
            ( dragBeginHandler, fireDragBegin ) <- newAddHandler
            ( dragUpdateHandler, fireDragUpdate ) <- newAddHandler
            ( dragEndHandler, fireDragEnd ) <- newAddHandler
            ( motionHandler, fireMotion ) <- newAddHandler

            -- Create drag gesture and connect signals BEFORE adding to widget
            -- We use only GestureDrag for both click and drag detection
            -- Click is detected as a drag that ends without moving beyond threshold
            dragGesture <- new Gtk.GestureDrag []
            _ <- on dragGesture #dragBegin $ \x y -> fireDragBegin ( x, y )
            _ <- on dragGesture #dragUpdate $ \ox oy -> fireDragUpdate ( ox, oy )
            _ <- on dragGesture #dragEnd $ \ox oy -> fireDragEnd ( ox, oy )
            Gtk.widgetAddController picture dragGesture

            -- Create motion controller for OnMouseMove events
            motionController <- new Gtk.EventControllerMotion []
            _ <- on motionController #motion $ \x y -> fireMotion ( x, y )
            Gtk.widgetAddController picture motionController

            -- Create DropTarget for NAR file drops
            -- GFile type is used to accept file drops
            gfileType <- glibType @Gio.File
            dropTarget <- Gtk.dropTargetNew gfileType [Gdk.DragActionCopy]
            _ <- on dropTarget #drop $ \gvalue _x _y -> do
              -- Extract GFile from GValue using get_object
              -- The drop signal provides a GValue containing the dropped data
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
                  -- Check if it's a .nar file
                  if takeExtension path == ".nar"
                    then do
                      -- Get base directories for installation
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

            -- Set picture as window content
            Gtk.windowSetChild window (Just picture)

            -- Try to initialize layer-shell BEFORE window is shown
            -- This determines which move mode we'll use
            layerShellSuccess <- initLayerShell window
            
            -- Create the appropriate move mode based on platform
            moveMode <- if layerShellSuccess
              then do
                -- Layer-shell mode: track position via margins
                -- We need to track the window position to update it during drags
                positionRef <- newIORef (0 :: Int32, 0 :: Int32)
                
                -- Create update function that applies offset to current position
                let updatePosition :: Double -> Double -> IO ()
                    updatePosition dx dy = do
                      (currentX, currentY) <- readIORef positionRef
                      let newX = currentX + round dx
                          newY = currentY + round dy
                      writeIORef positionRef (newX, newY)
                      setLayerShellPosition window newX newY
                
                return $ MoveLayerShell updatePosition
              else do
                -- Standard toplevel mode: use compositor-driven move
                let beginMove :: Double -> Double -> IO ()
                    beginMove x y = void $ runMaybeT $ do
                      surface <- MaybeT $ Gtk.nativeGetSurface window
                      toplevel <- MaybeT $ Gdk.castTo Gdk.Toplevel surface
                      disp <- MaybeT Gdk.displayGetDefault
                      seat <- MaybeT $ Gdk.displayGetDefaultSeat disp
                      device <- MaybeT $ Gdk.seatGetPointer seat
                      MaybeT $ pure <$> Gdk.toplevelBeginMove toplevel device 0 x y 0
                
                return $ MoveToplevel beginMove

            -- Create timer event handlers
            ( secondTickHandler, fireSecondTick ) <- newAddHandler
            ( minuteTickHandler, fireMinuteTick ) <- newAddHandler

            -- Helper to get current local time
            let getLocalTime = do
                  tz <- getCurrentTimeZone
                  utc <- getCurrentTime
                  return $ utcToLocalTime tz utc

            -- Set up second timer (fires every 1000ms = 1 second)
            _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 1000 $ do
              lt <- getLocalTime
              fireSecondTick lt
              return True  -- True = keep timer running

            -- Set up minute timer (fires every 60000ms = 1 minute)
            _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 60000 $ do
              lt <- getLocalTime
              fireMinuteTick lt
              return True  -- True = keep timer running

            -- Get collision regions from initial surface
            let collisions = sdCollisions initialSurfDef

            -- Build network config
            let inputHandlers
                  = InputHandlers { ihDragBegin  = dragBeginHandler
                                  , ihDragUpdate = dragUpdateHandler
                                  , ihDragEnd    = dragEndHandler
                                  , ihMotion     = motionHandler
                                  }
                timerHandlers
                  = TimerHandlers { thSecondTick = secondTickHandler
                                  , thMinuteTick = minuteTickHandler
                                  }
                networkConfig
                  = NetworkConfig { ncWindow     = window
                                  , ncInputs     = inputHandlers
                                  , ncTimers     = timerHandlers
                                  , ncCollisions = collisions
                                  , ncMoveMode   = moveMode
                                  , ncShiori     = mShioriConfig
                                  }

            -- Set up and activate FRP network
            network <- compile (setupNetwork networkConfig)
            actuate network

            -- Create balloon window for displaying SHIORI responses
            balloon <- newBalloonState app
            
            -- Initialize balloon for always-on-top (must be before first show)
            _ <- initBalloonAlwaysOnTop balloon
            
            -- Create interpreter callbacks that interact with the balloon and surface
            let interpreterCallbacks = defaultCallbacks
                  { cbAppendChar    = appendChar balloon
                  , cbAppendText    = appendText balloon
                  , cbNewline       = appendNewline balloon
                  , cbClear         = clearBalloon balloon
                  , cbSetSurface    = changeSurface
                  , cbAddChoice     = \choiceId text action -> 
                      addChoice balloon (BalloonChoice text choiceId action)
                  , cbClearChoices  = clearChoices balloon
                  , cbOnComplete    = putStrLn "[Script] Execution complete"
                  , cbOnInterrupt   = putStrLn "[Script] Execution interrupted"
                  }
            
            -- Helper to display script in balloon with character-by-character animation
            let displayScript :: Maybe T.Text -> IO ()
                displayScript Nothing = return ()
                displayScript (Just scriptText) = do
                  -- Parse the SakuraScript
                  case parseScript scriptText of
                    Left err -> putStrLn $ "[Balloon] Parse error: " <> show err
                    Right script -> do
                      -- Clear balloon before new script
                      clearBalloon balloon
                      -- Execute script asynchronously with animation
                      _ <- executeScriptAsync defaultInterpreterConfig interpreterCallbacks script
                      return ()

            -- Set up choice callback to handle user clicks on choices
            setChoiceCallback balloon $ \choice -> do
              putStrLn $ "[Choice] Selected: " <> T.unpack (bcText choice) 
                      <> " (id=" <> T.unpack (bcId choice) 
                      <> ", action=" <> T.unpack (bcAction choice) <> ")"
              -- Clear the balloon and choices after selection
              clearBalloon balloon
              clearChoices balloon
              -- Handle the action based on its type
              let action = bcAction choice
              case T.stripPrefix "event:" action of
                Just _eventId -> do
                  -- Fire OnChoiceSelect event with the choice ID
                  let refs = Map.fromList [(0, bcId choice), (1, bcText choice)]
                  sendShioriWithCallback mShioriConfig OnChoiceSelect refs displayScript
                Nothing -> case T.stripPrefix "script:" action of
                  Just script -> do
                    -- Execute script directly
                    displayScript (Just script)
                  Nothing -> case T.stripPrefix "url:" action of
                    Just _url -> do
                      -- TODO: Open URL in browser
                      putStrLn $ "[Choice] URL action not yet implemented"
                    Nothing -> case T.stripPrefix "anchor:" action of
                      Just anchorId -> do
                        -- Anchors fire OnAnchorSelect
                        let refs = Map.fromList [(0, anchorId), (1, bcText choice)]
                        sendShioriWithCallback mShioriConfig OnAnchorSelect refs displayScript
                      Nothing -> do
                        -- Default: treat as event ID
                        let refs = Map.fromList [(0, bcId choice), (1, bcText choice)]
                        sendShioriWithCallback mShioriConfig OnChoiceSelect refs displayScript

            -- Send OnBoot or OnFirstBoot event after network is activated
            -- OnFirstBoot: Reference0 = vanished count
            if firstBoot
              then do
                let refs = Map.fromList [(0, T.pack $ show vanishedCount)]
                sendShioriWithCallback mShioriConfig OnFirstBoot refs displayScript
                -- Create HISTORY file for next time
                saveGhostHistory ghostPath defaultGhostHistory
              else sendShioriWithCallback mShioriConfig OnBoot Map.empty displayScript

            -- Finalize window setup based on platform
            if layerShellSuccess
              then do
                -- Layer shell: set layer and show
                setWindowLayer window LayerTop
                putStrLn "Window set to always-on-top (Wayland layer-shell)"
                Gtk.windowPresent window
              else do
                -- Not layer-shell: show window then try X11
                Gtk.windowPresent window
                x11Success <- setWindowAboveFromGtk window True
                if x11Success
                  then putStrLn "Window set to always-on-top (X11)"
                  else putStrLn "Note: Always-on-top not available (Wayland without layer-shell or X11 error)"

            -- Set input region based on pixbuf alpha for click-through on transparent areas
            -- This must be done after the window is realized (has a GDK surface)
            applyInputRegion window initialPixbuf

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

-- | Apply input region to window based on pixbuf alpha channel.
-- This enables click-through on transparent areas of the surface.
applyInputRegion :: Gtk.Window -> Pixbuf.Pixbuf -> IO ()
applyInputRegion window pixbuf = void $ runMaybeT $ do
  -- Get the GDK surface from the window
  surface <- MaybeT $ Gtk.nativeGetSurface window
  -- Set input region based on pixbuf alpha
  success <- setInputRegionFromPixbuf surface pixbuf
  MaybeT $ do
    if success
      then putStrLn "[InputRegion] Set input region from pixbuf alpha"
      else putStrLn "[InputRegion] Pixbuf has no alpha channel, using full surface"
    return $ Just ()
