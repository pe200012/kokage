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

import           Control.Exception          ( try, SomeException )
import           Control.Monad              ( forM_, void, filterM )
import           Control.Monad.Trans.Maybe  ( MaybeT(runMaybeT, MaybeT) )

import           Data.GI.Base               ( AttrOp((:=)), new, on, glibType
                                            , withManagedPtr, newObject )
import           Data.GI.Base.GValue        ( get_object )
import           Data.Int                   ( Int32 )
import           Data.IORef                 ( newIORef, readIORef, writeIORef )
import           Foreign.Ptr                ( Ptr, nullPtr )
import           Data.List                  ( sort )
import           Data.Maybe                 ( listToMaybe )
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Time                  ( getCurrentTime, utcToLocalTime, getCurrentTimeZone )

import qualified GI.Gdk                     as Gdk
import qualified GI.GdkPixbuf               as Pixbuf
import qualified GI.Gio                     as Gio
import qualified GI.GLib                    as GLib
import qualified GI.Gtk                     as Gtk

import           Kokage.Collision
import           Kokage.Event               ( NetworkConfig(..), InputHandlers(..)
                                            , TimerHandlers(..), MoveMode(..)
                                            , setupNetwork )
import           Kokage.Install             ( BaseDir(..), InstallResult(..), installNar )
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
                                            , Shell(..)
                                            , SurfaceDefinition(..)
                                            , loadGhost
                                            )

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

                      -- Run the GTK application
                      runGtkApp pixbuf width height (sdCollisions surfDef)

-- | Run the GTK application with the given pixbuf.
runGtkApp :: Pixbuf.Pixbuf -> Int32 -> Int32 -> [ CollisionRegion ] -> IO ()
runGtkApp pixbuf width height collisions = do
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
    texture <- Gdk.textureNewForPixbuf pixbuf

    -- Create picture widget to display the texture
    picture <- new Gtk.Picture [ #paintable := texture, #canShrink := False ]

    -- Create all event handlers BEFORE creating gestures
    ( dragBeginHandler, fireDragBegin ) <- newAddHandler
    ( dragUpdateHandler, fireDragUpdate ) <- newAddHandler
    ( dragEndHandler, fireDragEnd ) <- newAddHandler

    -- Create drag gesture and connect signals BEFORE adding to widget
    -- We use only GestureDrag for both click and drag detection
    -- Click is detected as a drag that ends without moving beyond threshold
    dragGesture <- new Gtk.GestureDrag []
    _ <- on dragGesture #dragBegin $ \x y -> fireDragBegin ( x, y )
    _ <- on dragGesture #dragUpdate $ \ox oy -> fireDragUpdate ( ox, oy )
    _ <- on dragGesture #dragEnd $ \ox oy -> fireDragEnd ( ox, oy )
    Gtk.widgetAddController picture dragGesture

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

    -- Build network config
    let inputHandlers
          = InputHandlers { ihDragBegin  = dragBeginHandler
                          , ihDragUpdate = dragUpdateHandler
                          , ihDragEnd    = dragEndHandler
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
                          }

    -- Set up and activate FRP network
    network <- compile (setupNetwork networkConfig)
    actuate network

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
