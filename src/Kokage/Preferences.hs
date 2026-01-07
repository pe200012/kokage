{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | User preferences system with persistence.
-- Stores and loads user settings from ~/.config/kokage/preferences.json
module Kokage.Preferences
  ( -- * Types
    Preferences(..)
  , defaultPreferences
    -- * IO Operations
  , loadPreferences
  , savePreferences
  , getPreferencesPath
    -- * Accessors
  , getScriptSpeed
  , setScriptSpeed
  , getSurfaceScale
  , setSurfaceScale
  , getBalloonFont
  , setBalloonFont
  , getMenuFont
  , setMenuFont
  , getUsePNA
  , setUsePNA
  , getSSTPAllow
  , setSSTPAllow
  , getSinkAfterTalk
  , setSinkAfterTalk
  , getRaiseBeforeTalk
  , setRaiseBeforeTalk
  , getShowCollisionArea
  , setShowCollisionArea
  ) where

import           Control.Exception                ( catch, SomeException )
import           Control.Monad                    ( when )

import           Data.Aeson                       ( FromJSON, ToJSON
                                                  , eitherDecodeFileStrict
                                                  , encodeFile
                                                  )
import           Data.IORef                       ( IORef, newIORef, readIORef
                                                  , writeIORef, modifyIORef' )
import           Data.Text                        ( Text )
import qualified Data.Text                        as T

import           GHC.Generics                     ( Generic )

import           System.Directory                 ( createDirectoryIfMissing
                                                  , doesFileExist
                                                  , getXdgDirectory
                                                  , XdgDirectory(..)
                                                  )
import           System.FilePath                  ( (</>) )

-- | User preferences for Kokage.
data Preferences
  = Preferences
  { -- Surface & Balloon settings
    prefSurfaceScale     :: !Int        -- ^ Surface scale percentage (10-1000, default 100)
  , prefScriptSpeed      :: !Int        -- ^ Text display speed (-1 to 8, default 1)
                                        -- ^ -1 = instant, 0 = no delay, 1-8 = progressively slower
  , prefBalloonFont      :: !Text       -- ^ Font for balloon text (default "Monospace")
  , prefMenuFont         :: !Text       -- ^ Font for menus (default "Sans")
  , prefUsePNA           :: !Bool       -- ^ Use PNA alpha masks (default True)
  , prefScaleBalloon     :: !Bool       -- ^ Scale balloon with surface (default False)
    -- Behavior settings
  , prefSSTPAllow        :: !Bool       -- ^ Allow external SSTP connections (default False)
  , prefSinkAfterTalk    :: !Bool       -- ^ Lower window after speech ends (default False)
  , prefRaiseBeforeTalk  :: !Bool       -- ^ Raise window when speech starts (default True)
    -- Debug settings
  , prefShowCollisionArea :: !Bool      -- ^ Show collision regions overlay (default False)
  , prefShowCollisionNames :: !Bool     -- ^ Show collision region names (default False)
  }
  deriving ( Eq, Show, Generic )

instance FromJSON Preferences
instance ToJSON Preferences

-- | Default preferences.
defaultPreferences :: Preferences
defaultPreferences = Preferences
  { prefSurfaceScale      = 100
  , prefScriptSpeed       = 1
  , prefBalloonFont       = "Monospace"
  , prefMenuFont          = "Sans"
  , prefUsePNA            = True
  , prefScaleBalloon      = False
  , prefSSTPAllow         = False
  , prefSinkAfterTalk     = False
  , prefRaiseBeforeTalk   = True
  , prefShowCollisionArea = False
  , prefShowCollisionNames = False
  }

-- | Get the preferences file path.
-- Uses XDG config directory: ~/.config/kokage/preferences.json
getPreferencesPath :: IO FilePath
getPreferencesPath = do
  configDir <- getXdgDirectory XdgConfig "kokage"
  return $ configDir </> "preferences.json"

-- | Load preferences from disk.
-- Returns default preferences if file doesn't exist or is invalid.
loadPreferences :: IO Preferences
loadPreferences = do
  path <- getPreferencesPath
  exists <- doesFileExist path
  if not exists
    then do
      putStrLn "[Preferences] No preferences file found, using defaults"
      return defaultPreferences
    else do
      result <- eitherDecodeFileStrict path
      case result of
        Left err -> do
          putStrLn $ "[Preferences] Error loading preferences: " <> err
          putStrLn "[Preferences] Using defaults"
          return defaultPreferences
        Right prefs -> do
          putStrLn $ "[Preferences] Loaded from " <> path
          return prefs

-- | Save preferences to disk.
savePreferences :: Preferences -> IO ()
savePreferences prefs = do
  path <- getPreferencesPath
  configDir <- getXdgDirectory XdgConfig "kokage"
  createDirectoryIfMissing True configDir
  encodeFile path prefs
  putStrLn $ "[Preferences] Saved to " <> path
  `catch` \(e :: SomeException) ->
    putStrLn $ "[Preferences] Error saving: " <> show e

-- | Get script speed (character delay).
-- Returns milliseconds per character based on prefScriptSpeed setting.
-- -1 = 0ms (instant), 0 = 5ms, 1 = 10ms (default), ..., 8 = 80ms
getScriptSpeed :: Preferences -> Int
getScriptSpeed prefs = case prefScriptSpeed prefs of
  -1 -> 0    -- Instant
  n  -> max 5 (n * 10)  -- 5ms minimum, 10ms per level

-- | Set script speed level (-1 to 8).
setScriptSpeed :: Int -> Preferences -> Preferences
setScriptSpeed n prefs = prefs { prefScriptSpeed = clamp (-1) 8 n }

-- | Get surface scale percentage.
getSurfaceScale :: Preferences -> Int
getSurfaceScale = prefSurfaceScale

-- | Set surface scale percentage (10-1000).
setSurfaceScale :: Int -> Preferences -> Preferences
setSurfaceScale n prefs = prefs { prefSurfaceScale = clamp 10 1000 n }

-- | Get balloon font name.
getBalloonFont :: Preferences -> Text
getBalloonFont = prefBalloonFont

-- | Set balloon font name.
setBalloonFont :: Text -> Preferences -> Preferences
setBalloonFont font prefs = prefs { prefBalloonFont = font }

-- | Get menu font name.
getMenuFont :: Preferences -> Text
getMenuFont = prefMenuFont

-- | Set menu font name.
setMenuFont :: Text -> Preferences -> Preferences
setMenuFont font prefs = prefs { prefMenuFont = font }

-- | Get use PNA setting.
getUsePNA :: Preferences -> Bool
getUsePNA = prefUsePNA

-- | Set use PNA setting.
setUsePNA :: Bool -> Preferences -> Preferences
setUsePNA b prefs = prefs { prefUsePNA = b }

-- | Get SSTP allow setting.
getSSTPAllow :: Preferences -> Bool
getSSTPAllow = prefSSTPAllow

-- | Set SSTP allow setting.
setSSTPAllow :: Bool -> Preferences -> Preferences
setSSTPAllow b prefs = prefs { prefSSTPAllow = b }

-- | Get sink after talk setting.
getSinkAfterTalk :: Preferences -> Bool
getSinkAfterTalk = prefSinkAfterTalk

-- | Set sink after talk setting.
setSinkAfterTalk :: Bool -> Preferences -> Preferences
setSinkAfterTalk b prefs = prefs { prefSinkAfterTalk = b }

-- | Get raise before talk setting.
getRaiseBeforeTalk :: Preferences -> Bool
getRaiseBeforeTalk = prefRaiseBeforeTalk

-- | Set raise before talk setting.
setRaiseBeforeTalk :: Bool -> Preferences -> Preferences
setRaiseBeforeTalk b prefs = prefs { prefRaiseBeforeTalk = b }

-- | Get show collision area setting.
getShowCollisionArea :: Preferences -> Bool
getShowCollisionArea = prefShowCollisionArea

-- | Set show collision area setting.
setShowCollisionArea :: Bool -> Preferences -> Preferences
setShowCollisionArea b prefs = prefs { prefShowCollisionArea = b }

-- | Clamp a value to a range.
clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = max lo (min hi x)
