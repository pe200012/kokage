{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main ( main ) where

import           Control.Monad              ( unless, when )
import           Control.Monad.Trans.Maybe  ( MaybeT(..), runMaybeT )
import           Data.IORef                 ( newIORef, readIORef, writeIORef, IORef )
import           Data.List                  ( find )
import           System.IO                  ( hFlush, stdout )
import           Data.Map.Strict            ( Map )
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import           System.Environment         ( getArgs )
import           System.Exit                ( exitFailure )
import           System.Directory           ( makeAbsolute )

import qualified GI.Gdk                     as Gdk
import qualified GI.GdkPixbuf               as Pixbuf
import qualified GI.Gio                     as Gio
import qualified GI.GLib                    as GLib
import qualified GI.Gtk                     as Gtk
import           Data.GI.Base               ( AttrOp((:=)), new, on )

import           Kokage.Animation           ( tickAnimations, compositeAnimation, newAnimationState
                                            , AnimationState(..), ImageCache )
import           Kokage.Surface             ( compositeSurface, findSurfaceById )
import           Types.Ghost                ( Ghost(..), Shell(..), Surfaces(..)
                                            , SurfaceDefinition(..), Animation(..)
                                            , loadGhost
                                            )

main :: IO ()
main = do
  -- Initialize GTK Application
  app <- new Gtk.Application
    [ #applicationId := "com.kokage.animation-test"
    , #flags := [ Gio.ApplicationFlagsFlagsNone ]
    ]

  _ <- on app #activate $ do
    args <- getArgs
    case args of
      [ghostPath, surfIdStr] -> runVisualTest app ghostPath (read surfIdStr)
      [ghostPath]            -> runVisualTest app ghostPath 0
      _                      -> do
        putStrLn "Usage: animation-test <ghost-path> [surface-id]"
        Gtk.windowDestroy =<< new Gtk.Window [#application := app] -- Dummy to exit?
        -- Actually just return, loop wont start properly without a window usually
        return ()

  _ <- Gio.applicationRun app Nothing
  return ()

runVisualTest :: Gtk.Application -> FilePath -> Int -> IO ()
runVisualTest app ghostPath surfId = do
  absPath <- makeAbsolute ghostPath
  putStrLn $ "Loading ghost from: " <> absPath
  
  mGhost <- loadGhost absPath
  case mGhost of
    Nothing -> putStrLn "Error: Failed to load ghost."
    Just ghost -> do
      case ghostShells ghost of
        [] -> putStrLn "Error: No shells found."
        (shell:_) -> do
          putStrLn $ "Using shell: " <> shellPath shell
          
          let surfaces = shellSurfaces shell
              mSurfDef = findSurfaceById surfId surfaces
              
          case mSurfDef of
            Nothing -> putStrLn $ "Error: Surface " <> show surfId <> " not found."
            Just surfDef -> do
              startVisualSimulation app shell surfDef

startVisualSimulation :: Gtk.Application -> Shell -> SurfaceDefinition -> IO ()
startVisualSimulation app shell surfDef = do
  putStrLn $ "Surface " <> show (sdId surfDef) <> " loaded."
  putStrLn $ "Definitions: " <> show (length (sdAnimations surfDef)) <> " animations."
  hFlush stdout
  
  -- Create Window
  window <- new Gtk.Window
    [ #application := app
    , #title := "Animation Test"
    , #resizable := False
    ]
    
  -- Composite initial surface
  mBasePixbuf <- compositeSurface (shellPath shell) surfDef
  case mBasePixbuf of
    Nothing -> putStrLn "Error: Failed to composite base surface."
    Just basePixbuf -> do
      width <- Pixbuf.pixbufGetWidth basePixbuf
      height <- Pixbuf.pixbufGetHeight basePixbuf
      
      putStrLn $ "Surface dimensions: " <> show width <> "x" <> show height
      
      -- Create Image Widget
      texture <- Gdk.textureNewForPixbuf basePixbuf
      picture <- new Gtk.Picture 
        [ #paintable := texture
        , #canShrink := False 
        ]
      Gtk.windowSetChild window (Just picture)
      
      -- Initialize Animation State
      animState <- newAnimationState
      writeIORef (asBasePixbuf animState) (Just basePixbuf)
      
      putStrLn "Starting animation loop..."
      hFlush stdout
      
      -- Animation Loop (50ms)
      _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 50 $ do
        -- putStrLn "Tick..." -- Very verbose, maybe comment out if working
        
        activeAnims <- readIORef (asActiveAnims animState)
        timers <- readIORef (asPeriodicState animState)
        
        (newAnims, newTimers, needsRedraw) <- tickAnimations animState shell surfDef activeAnims timers 50
        
        writeIORef (asActiveAnims animState) newAnims
        writeIORef (asPeriodicState animState) newTimers
        
        unless (null newAnims) $ do
             -- Only print if count changed to reduce spam, or just print count
             when (length newAnims /= length activeAnims) $ do
                 putStrLn $ "Active anims: " ++ show (length newAnims)
                 hFlush stdout

        when needsRedraw $ do
          putStrLn "Redrawing frame..."
          hFlush stdout
          -- Composite (using the cache from animState)
          mFinalPixbuf <- compositeAnimation shell (asImageCache animState) basePixbuf newAnims
          case mFinalPixbuf of
            Nothing -> putStrLn "Composite failed"
            Just finalPixbuf -> do
              newTexture <- Gdk.textureNewForPixbuf finalPixbuf
              Gtk.pictureSetPaintable picture (Just newTexture)
              
        return True -- Continue loop
        
      Gtk.windowPresent window
      putStrLn "Window presented."
