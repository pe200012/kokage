{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main ( main ) where

import Prelude ()
import Relude hiding (on)

import qualified Data.Text                  as T
import           System.Directory           ( makeAbsolute )

import qualified GI.Gdk                     as Gdk
import qualified GI.GdkPixbuf               as Pixbuf
import qualified GI.Gio                     as Gio
import qualified GI.GLib                    as GLib
import qualified GI.Gtk                     as Gtk
import           Data.GI.Base               ( AttrOp((:=)), new, on )

import           Kokage.Animation           ( tickAnimations, compositeAnimation, newAnimationState
                                            , AnimationState(..) )
import           Kokage.Surface             ( compositeSurface, findSurfaceById )
import           Types.Ghost                ( Ghost(..), Shell(..), SurfaceDefinition(..)
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
    let usage = do
          putStrLn "Usage: animation-test <ghost-path> [surface-id] [scope]"
          putStrLn "  ghost-path  : Path to ghost directory"
          putStrLn "  surface-id  : Surface ID to display (default: 0)"
          putStrLn "  scope       : Character scope (0=sakura, 1=kero, 2+=char*) (default: 0)"
          putStrLn ""
          putStrLn "Examples:"
          putStrLn "  animation-test ./test-fdr/ghost/emily4          # sakura surface 0"
          putStrLn "  animation-test ./test-fdr/ghost/emily4 10       # sakura surface 10"
          putStrLn "  animation-test ./test-fdr/ghost/emily4 204 2    # char2 surface 204"
          Gtk.windowDestroy =<< new Gtk.Window [#application := app]

    args <- getArgs
    case args of
      [gPath, surfIdStr, scopeStr] ->
        case (readMaybe surfIdStr, readMaybe scopeStr) of
          (Just surfId, Just scope) -> runVisualTest app gPath surfId scope
          _                         -> do
            putStrLn "Error: surface-id and scope must be integers."
            usage

      [gPath, surfIdStr] ->
        case readMaybe surfIdStr of
          Just surfId -> runVisualTest app gPath surfId 0
          Nothing     -> do
            putStrLn "Error: surface-id must be an integer."
            usage

      [gPath] -> runVisualTest app gPath 0 0
      _       -> usage

  _ <- Gio.applicationRun app Nothing
  return ()

runVisualTest :: Gtk.Application -> FilePath -> Int -> Int -> IO ()
runVisualTest app gPath surfId scope = do
  absPath <- makeAbsolute gPath
  putStrLn $ "Loading ghost from: " <> absPath
  putStrLn $ "Surface ID: " <> show surfId
  putStrLn $ "Scope: " <> show scope <> " (" <> scopeName scope <> ")"
  
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
              startVisualSimulation app shell surfDef scope

scopeName :: Int -> String
scopeName 0 = "sakura"
scopeName 1 = "kero"
scopeName n = "char" <> show n

startVisualSimulation :: Gtk.Application -> Shell -> SurfaceDefinition -> Int -> IO ()
startVisualSimulation app shell surfDef scope = do
  putStrLn $ "Surface " <> show (sdId surfDef) <> " loaded."
  putStrLn $ "Definitions: " <> show (length (sdAnimations surfDef)) <> " animations."
  hFlush stdout
  
  -- Create Window
  window <- new Gtk.Window
    [ #application := app
    , #title := "Animation Test - " <> T.pack (scopeName scope) <> " surface " <> T.pack (show (sdId surfDef))
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
