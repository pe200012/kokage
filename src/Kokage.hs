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

import Control.Monad ( forM_ )
import Data.GI.Base ( AttrOp((:=)), new, on )
import Data.Int ( Int32 )
import Data.Maybe ( listToMaybe )
import qualified Data.Text as T

import qualified GI.Gdk as Gdk
import qualified GI.GdkPixbuf as Pixbuf
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

import Reactive.Banana ( compile )
import Reactive.Banana.Frameworks ( actuate, newAddHandler )

import Types.Ghost
  ( Ghost(..)
  , Shell(..)
  , SurfaceDefinition(..)
  , CollisionRegion(..)
  , loadGhost
  )

import Kokage.Collision
import Kokage.Surface
import Kokage.Event


-- | Configuration for the Kokage application.
data KokageConfig = KokageConfig
  { configGhostPath :: !FilePath   -- ^ Path to the ghost directory
  , configSurfaceId :: !Int        -- ^ Initial surface ID to display
  } deriving ( Show, Eq )

-- | Default configuration.
defaultConfig :: KokageConfig
defaultConfig = KokageConfig
  { configGhostPath = ""
  , configSurfaceId = 0
  }

-- | Get the default shell (first shell) from a ghost.
getDefaultShell :: Ghost -> Maybe Shell
getDefaultShell ghost = listToMaybe (ghostShells ghost)

-- | Main entry point for Kokage.
-- Loads a ghost and runs the GTK event loop.
kokageMain :: KokageConfig -> IO ()
kokageMain config = do
  let gPath = configGhostPath config
      surfId = configSurfaceId config

  -- Load the ghost
  putStrLn $ "Loading ghost from: " <> gPath
  mGhost <- loadGhost gPath

  case mGhost of
    Nothing -> putStrLn "Error: Failed to load ghost"
    Just ghost -> do
      putStrLn $ "Loaded ghost: " <> gPath

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
              putStrLn $ "Found surface " <> show surfId <> " with "
                       <> show (length $ sdElements surfDef) <> " elements, "
                       <> show (length $ sdCollisions surfDef) <> " collision regions"

              -- Log collision regions for debugging
              forM_ (sdCollisions surfDef) $ \cr ->
                putStrLn $ "  - Collision " <> show (crIndex cr)
                         <> ": " <> T.unpack (crName cr)

              -- Composite the surface
              mPixbuf <- compositeSurface (shellPath shell) surfDef

              case mPixbuf of
                Nothing -> putStrLn "Error: Failed to composite surface"
                Just pixbuf -> do
                  width <- Pixbuf.pixbufGetWidth pixbuf
                  height <- Pixbuf.pixbufGetHeight pixbuf
                  putStrLn $ "Composited surface: "
                           <> show width <> "x" <> show height

                  -- Run the GTK application
                  runGtkApp pixbuf width height (sdCollisions surfDef)

-- | Run the GTK application with the given pixbuf.
runGtkApp :: Pixbuf.Pixbuf -> Int32 -> Int32 -> [ CollisionRegion ] -> IO ()
runGtkApp pixbuf width height collisions = do
  -- Initialize GTK application
  app <- new Gtk.Application
    [ #applicationId := "com.kokage.app"
    , #flags := [ Gio.ApplicationFlagsFlagsNone ]
    ]

  -- Connect activate signal
  _ <- on app #activate $ do
    -- Create window
    window <- new Gtk.Window
      [ #application := app
      , #title := "Kokage"
      , #defaultWidth := width
      , #defaultHeight := height
      , #resizable := False
      ]

    -- Create texture from pixbuf for GTK4
    texture <- Gdk.textureNewForPixbuf pixbuf

    -- Create picture widget to display the texture
    picture <- new Gtk.Picture
      [ #paintable := texture
      , #canShrink := False
      ]

    -- Create click gesture and connect signal BEFORE adding to widget
    -- This avoids the "disowned pointer" warning since widgetAddController
    -- transfers ownership of the controller to the widget
    clickGesture <- new Gtk.GestureClick []
    (clickHandler, fireClick) <- newAddHandler
    _ <- on clickGesture #pressed $ \_ x y -> fireClick (x, y)

    -- Now add controller to widget (transfers ownership)
    Gtk.widgetAddController picture clickGesture

    -- Set picture as window content
    Gtk.windowSetChild window (Just picture)

    -- Set up FRP network with collision detection
    network <- compile (setupNetwork window clickHandler collisions)
    actuate network

    -- Show window
    Gtk.windowPresent window

  -- Run application
  _ <- Gio.applicationRun app Nothing
  return ()
