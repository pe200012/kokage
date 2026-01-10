{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Sound playback module for Kokage.
-- Uses system audio players (paplay, aplay) for simplicity.
module Kokage.Sound
  ( -- * Sound State
    SoundState
  , newSoundState
    -- * Playback Control
  , playSound
  , stopSound
  , pauseSound
  , resumeSound
  , setVolume
  , isPlaying
  ) where

import           Control.Concurrent ( forkIO )
import           Control.Exception  ( catch )

import qualified Data.Text          as T

import           System.Directory   ( findExecutable )
import           System.Process     ( ProcessHandle
                                    , spawnProcess
                                    , terminateProcess
                                    , waitForProcess
                                    )

-- | State for sound playback
data SoundState
  = SoundState
  { ssCurrentProcess :: !(IORef (Maybe ProcessHandle))  -- ^ Currently playing process
  , ssCurrentFile    :: !(IORef (Maybe T.Text))         -- ^ Currently playing file
  , ssVolume         :: !(IORef Double)                 -- ^ Volume (0.0-1.0)
  , ssPlayerCmd      :: !(Maybe String)                 -- ^ Detected audio player command
  }

-- | Create a new sound state and detect available audio player
newSoundState :: IO SoundState
newSoundState = do
  procRef <- newIORef Nothing
  fileRef <- newIORef Nothing
  volRef <- newIORef 1.0

  -- Detect available audio player
  mPaplay <- findExecutable "paplay"
  mAplay <- findExecutable "aplay"
  mMpv <- findExecutable "mpv"
  mFfplay <- findExecutable "ffplay"

  let player = case ( mPaplay, mAplay, mMpv, mFfplay ) of
        ( Just p, _, _, _ ) -> Just p
        ( _, Just p, _, _ ) -> Just p
        ( _, _, Just p, _ ) -> Just p
        ( _, _, _, Just p ) -> Just p
        _ -> Nothing

  return
    SoundState { ssCurrentProcess = procRef
               , ssCurrentFile    = fileRef
               , ssVolume         = volRef
               , ssPlayerCmd      = player
               }

-- | Play a sound file
playSound :: SoundState -> T.Text -> IO ()
playSound ss file = do
  -- Stop any currently playing sound
  stopSound ss

  let mPlayer = ssPlayerCmd ss
  case mPlayer of
    Nothing     -> putStrLn "[Sound] No audio player found (tried paplay, aplay, mpv, ffplay)"
    Just player -> do
      let filePath = T.unpack file

      -- Build command based on player
      let args = case player of
            p
              | "paplay" `T.isInfixOf` T.pack p -> [ filePath ]
              | "aplay" `T.isInfixOf` T.pack p -> [ filePath ]
              | "mpv" `T.isInfixOf` T.pack p -> [ "--no-video", "--really-quiet", filePath ]
              | "ffplay" `T.isInfixOf` T.pack p
                -> [ "-nodisp", "-autoexit", "-loglevel", "quiet", filePath ]
              | otherwise -> [ filePath ]

      -- Spawn process
      mProc <- catch (Just <$> spawnProcess player args) (\(_ :: SomeException) -> return Nothing)

      case mProc of
        Just proc -> do
          writeIORef (ssCurrentProcess ss) (Just proc)
          writeIORef (ssCurrentFile ss) (Just file)
          putStrLn $ "[Sound] Playing: " <> filePath

          -- Start a thread to clean up when playback finishes
          void $ forkIO $ do
            _ <- waitForProcess proc
            writeIORef (ssCurrentProcess ss) Nothing
            writeIORef (ssCurrentFile ss) Nothing

        Nothing   -> putStrLn $ "[Sound] Failed to play: " <> filePath

-- | Stop currently playing sound
stopSound :: SoundState -> IO ()
stopSound ss = do
  mProc <- readIORef (ssCurrentProcess ss)
  case mProc of
    Just proc -> do
      catch (terminateProcess proc) (\(_ :: SomeException) -> return ())
      writeIORef (ssCurrentProcess ss) Nothing
      writeIORef (ssCurrentFile ss) Nothing
      putStrLn "[Sound] Stopped"
    Nothing   -> return ()

-- | Pause currently playing sound (not supported by all players)
pauseSound :: SoundState -> IO ()
pauseSound ss = do
  -- Most simple players don't support pause
  -- For now, just stop
  putStrLn "[Sound] Pause not supported, stopping instead"
  stopSound ss

-- | Resume paused sound (not supported by all players)
resumeSound :: SoundState -> IO ()
resumeSound ss = do
  -- Resume by replaying the file
  mFile <- readIORef (ssCurrentFile ss)
  case mFile of
    Just file -> playSound ss file
    Nothing   -> putStrLn "[Sound] Nothing to resume"

-- | Set volume (0.0-1.0)
setVolume :: SoundState -> Double -> IO ()
setVolume ss vol = do
  writeIORef (ssVolume ss) (max 0.0 (min 1.0 vol))
  -- Volume control would require pactl or similar
  putStrLn $ "[Sound] Volume set to " <> show (vol * 100) <> "%"

-- | Check if sound is currently playing
isPlaying :: SoundState -> IO Bool
isPlaying ss = isJust <$> readIORef (ssCurrentProcess ss)
