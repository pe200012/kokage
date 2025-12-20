{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main Kokage event loop and application entry point.
-- This is the thin IO shell that wires everything together.
module Kokage
  ( -- * Main Entry Point
    kokageMain
    -- * Configuration
  , KokageConfig(..)
  , defaultConfig
    -- * Re-exports for convenience
  , module Kokage.Collision
  , module Kokage.Surface
  , module Kokage.Event
  ) where

import           Control.Monad              ( forM_, void )
import           Control.Monad.Trans.Maybe  ( MaybeT(runMaybeT, MaybeT) )

import           Data.GI.Base               ( AttrOp((:=)), new, on )
import           Data.Int                   ( Int32 )
import           Data.Maybe                 ( listToMaybe )
import qualified Data.Text                  as T

import qualified GI.Gdk                     as Gdk
import qualified GI.GdkPixbuf               as Pixbuf
import qualified GI.Gio                     as Gio
import qualified GI.Gtk                     as Gtk

import           Kokage.Collision
import           Kokage.Event
import           Kokage.Surface

import           Reactive.Banana            ( compile )
import           Reactive.Banana.Frameworks ( actuate, newAddHandler )

import           Types.Ghost                ( CollisionRegion(..)
                                            , Ghost(..)
                                            , Shell(..)
                                            , SurfaceDefinition(..)
                                            , loadGhost
                                            )

-- | Configuration for the Kokage application.
data KokageConfig
  = KokageConfig { configGhostPath :: !FilePath   -- ^ Path to the ghost directory
                 , configSurfaceId :: !Int        -- ^ Initial surface ID to display
                 }
  deriving ( Show, Eq )

-- | Default configuration.
defaultConfig :: KokageConfig
defaultConfig = KokageConfig { configGhostPath = "", configSurfaceId = 0 }

-- | Get the default shell (first shell) from a ghost.
getDefaultShell :: Ghost -> Maybe Shell
getDefaultShell ghost = listToMaybe (ghostShells ghost)

-- | Main entry point for Kokage.
-- Loads a ghost and runs the GTK event loop.
kokageMain :: KokageConfig -> IO ()
kokageMain config = do
  let gPath  = configGhostPath config
      surfId = configSurfaceId config

  -- Load the ghost
  putStrLn $ "Loading ghost from: " <> gPath
  mGhost <- loadGhost gPath

  case mGhost of
    Nothing    -> putStrLn "Error: Failed to load ghost"
    Just ghost -> do
      putStrLn $ "Loaded ghost: " <> gPath

      -- Get default shell
      case getDefaultShell ghost of
        Nothing    -> putStrLn "Error: No shells found in ghost"
        Just shell -> do
          putStrLn $ "Using shell: " <> shellPath shell

          -- Find requested surface
          let surfaces = shellSurfaces shell
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
              mPixbuf <- compositeSurface (shellPath shell) surfDef

              case mPixbuf of
                Nothing     -> putStrLn "Error: Failed to composite surface"
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

    -- Set picture as window content
    Gtk.windowSetChild window (Just picture)

    -- Create action to begin window move (passed to FRP network)
    let beginMove :: Double -> Double -> IO ()
        beginMove x y = void $ runMaybeT $ do
          surface <- MaybeT $ Gtk.nativeGetSurface window
          toplevel <- MaybeT $ Gdk.castTo Gdk.Toplevel surface
          disp <- MaybeT Gdk.displayGetDefault
          seat <- MaybeT $ Gdk.displayGetDefaultSeat disp
          device <- MaybeT $ Gdk.seatGetPointer seat
          MaybeT $ pure <$> Gdk.toplevelBeginMove toplevel device 0 x y 0

    -- Build network config
    let inputHandlers
          = InputHandlers { ihDragBegin  = dragBeginHandler
                          , ihDragUpdate = dragUpdateHandler
                          , ihDragEnd    = dragEndHandler
                          }
        networkConfig
          = NetworkConfig { ncWindow     = window
                          , ncInputs     = inputHandlers
                          , ncCollisions = collisions
                          , ncBeginMove  = beginMove
                          }

    -- Set up and activate FRP network
    network <- compile (setupNetwork networkConfig)
    actuate network

    -- Show window
    Gtk.windowPresent window

  -- Run application
  _ <- Gio.applicationRun app Nothing
  return ()
