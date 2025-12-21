{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wine bridge for communicating with SHIORI DLLs.
--
-- This module provides an interface to run Windows SHIORI DLLs via Wine,
-- using a simple IPC protocol over stdin/stdout with Base64-encoded payloads.
--
-- Protocol:
--   Commands (sent to bridge):
--     LOAD <dll_path> <ghost_path>
--     REQUEST <base64_encoded_request>
--     UNLOAD
--     QUIT
--
--   Responses (from bridge):
--     READY
--     OK
--     RESPONSE <base64_encoded_response>
--     RESPONSE (empty, for 204 No Content)
--     ERROR <message>
module Kokage.Shiori.WineBridge
  ( -- * Bridge Handle
    WineShiori(..)
    -- * Configuration
  , WineBridgeConfig(..)
  , defaultWineBridgeConfig
    -- * Lifecycle
  , startWineBridge
  , stopWineBridge
  , withWineBridge
    -- * Operations
  , loadShiori
  , unloadShiori
  , sendRequest
  , sendEvent
    -- * Internal (exported for testing)
  , toWinePath
  ) where

import           Control.Exception          ( SomeException
                                            , bracket
                                            , try
                                            )
import           Control.Monad              ( void, when )

import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Char8      as BS8
import qualified Data.Map.Strict            as Map
import           Data.Text                  ( Text )
import qualified Data.Text                  as T

import           System.Environment         ( getEnvironment )
import           System.FilePath            ( (</>) )
import           System.IO                  ( BufferMode(..)
                                            , Handle
                                            , hClose
                                            , hFlush
                                            , hGetLine
                                            , hPutStrLn
                                            , hSetBuffering
                                            )
import           System.Process             ( CreateProcess(..)
                                            , ProcessHandle
                                            , StdStream(..)
                                            , createProcess
                                            , proc
                                            , terminateProcess
                                            , waitForProcess
                                            )
import           System.Timeout             ( timeout )

import           Types.Shiori               ( ShioriEvent(..)
                                            , ShioriRequest(..)
                                            , ShioriResponse(..)
                                            , emptyResponse
                                            , eventToId
                                            , mkRequest
                                            , parseShioriResponseBS
                                            , serializeShioriRequestBS
                                            )

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for the Wine bridge
data WineBridgeConfig = WineBridgeConfig
  { wbcWineCommand  :: FilePath  -- ^ Wine executable (default: "wine")
  , wbcBridgePath   :: FilePath  -- ^ Path to shiori_bridge.exe
  , wbcBridgeSoPath :: FilePath  -- ^ Path to shiori_bridge.exe.so (preferred)
  , wbcSenderName   :: Text      -- ^ Sender name for SHIORI requests
  , wbcTimeoutMs    :: Int       -- ^ IPC timeout in milliseconds (default: 30000)
  }
  deriving ( Show, Eq )

-- | Default configuration
defaultWineBridgeConfig :: WineBridgeConfig
defaultWineBridgeConfig = WineBridgeConfig
  { wbcWineCommand  = "wine"
  , wbcBridgePath   = "wine-helper" </> "shiori_bridge.exe"
  , wbcBridgeSoPath = "wine-helper" </> "shiori_bridge.exe.so"
  , wbcSenderName   = "Kokage"
  , wbcTimeoutMs    = 30000  -- 30 seconds
  }

--------------------------------------------------------------------------------
-- Bridge Handle
--------------------------------------------------------------------------------

-- | Handle to a running Wine bridge process
data WineShiori = WineShiori
  { wsProcess   :: ProcessHandle     -- ^ Wine process handle
  , wsStdin     :: Handle            -- ^ Stdin for commands
  , wsStdout    :: Handle            -- ^ Stdout for responses
  , wsConfig    :: WineBridgeConfig  -- ^ Configuration used
  , wsDllPath   :: Maybe FilePath    -- ^ Currently loaded DLL path
  , wsGhostPath :: Maybe FilePath    -- ^ Ghost directory path
  }

--------------------------------------------------------------------------------
-- Lifecycle
--------------------------------------------------------------------------------

-- | Start the Wine bridge subprocess.
-- Prefers .exe.so (Winelib) if available, falls back to .exe via Wine.
startWineBridge :: WineBridgeConfig -> IO (Either String WineShiori)
startWineBridge config = do
  result <- try $ do
    -- Get current environment and add WINEDEBUG=-all
    currentEnv <- getEnvironment
    let modifiedEnv = ("WINEDEBUG", "-all") : filter ((/= "WINEDEBUG") . fst) currentEnv

    -- Use the .exe wrapper script which handles Wine loading
    -- The wrapper script respects WINELOADER env var and handles paths correctly
    let (cmd, args) = (wbcBridgePath config, [])

    let cp = (proc cmd args)
          { std_in  = CreatePipe
          , std_out = CreatePipe
          , std_err = Inherit  -- Let Wine debug go to terminal stderr
          , env     = Just modifiedEnv  -- Inherit env with WINEDEBUG added
          }

    (Just hIn, Just hOut, _, ph) <- createProcess cp

    -- Set line buffering for IPC
    hSetBuffering hIn LineBuffering
    hSetBuffering hOut LineBuffering

    -- Wait for READY signal from bridge with timeout
    -- Skip Wine debug output lines that may appear before READY
    -- Use a longer timeout for startup (Wine initialization can be slow)
    let startupTimeoutUs = wbcTimeoutMs config * 1000 * 2  -- Double the normal timeout
    
    -- Read lines until we get READY or timeout
    let waitForReady :: IO (Either String WineShiori)
        waitForReady = do
          mLine <- timeout startupTimeoutUs $ hGetLine hOut
          case mLine of
            Nothing -> do
              terminateProcess ph
              return $ Left "Startup timeout: bridge did not respond in time"
            Just line -> do
              let cleanLine = stripWhitespace line
              -- Skip Wine debug output (starts with "wine:" or "wineserver:")
              if "wine:" `isPrefixOf` cleanLine || "wineserver:" `isPrefixOf` cleanLine
                then waitForReady  -- Skip and try next line
                else if cleanLine == "READY"
                  then return $ Right WineShiori
                    { wsProcess   = ph
                    , wsStdin     = hIn
                    , wsStdout    = hOut
                    , wsConfig    = config
                    , wsDllPath   = Nothing
                    , wsGhostPath = Nothing
                    }
                  else do
                    terminateProcess ph
                    return $ Left $ "Unexpected bridge output: " ++ show line
    
    waitForReady

  case result of
    Left (e :: SomeException) -> return $ Left $ "Failed to start bridge: " ++ show e
    Right r -> return r

-- | Stop the Wine bridge subprocess gracefully
stopWineBridge :: WineShiori -> IO ()
stopWineBridge ws = do
  -- Send QUIT command (best effort)
  _ <- try (sendCommand ws "QUIT") :: IO (Either SomeException (Either String String))

  -- Close handles
  hClose (wsStdin ws)
  hClose (wsStdout ws)

  -- Wait for process to terminate
  void $ waitForProcess (wsProcess ws)

-- | Bracket-style resource management for Wine bridge
withWineBridge :: WineBridgeConfig -> (WineShiori -> IO a) -> IO (Either String a)
withWineBridge config action = do
  result <- startWineBridge config
  case result of
    Left err -> return $ Left err
    Right ws -> do
      a <- bracket (return ws) stopWineBridge action
      return $ Right a

--------------------------------------------------------------------------------
-- IPC Commands
--------------------------------------------------------------------------------

-- | Send a raw command and get response with timeout
sendCommand :: WineShiori -> String -> IO (Either String String)
sendCommand ws cmd = do
  let timeoutUs = wbcTimeoutMs (wsConfig ws) * 1000  -- Convert ms to microseconds
  result <- timeout timeoutUs $ try $ do
    hPutStrLn (wsStdin ws) cmd
    hFlush (wsStdin ws)
    hGetLine (wsStdout ws)
  case result of
    Nothing -> return $ Left "IPC timeout: operation took too long"
    Just (Left (e :: SomeException)) -> return $ Left $ "IPC error: " ++ show e
    Just (Right response) -> return $ Right response

--------------------------------------------------------------------------------
-- SHIORI Operations
--------------------------------------------------------------------------------

-- | Load a SHIORI DLL.
-- Returns an updated handle with the loaded DLL info.
loadShiori :: WineShiori 
           -> FilePath    -- ^ Path to SHIORI DLL
           -> FilePath    -- ^ Ghost directory path
           -> IO (Either String WineShiori)
loadShiori ws dllPath ghostPath = do
  -- Convert Unix paths to Wine Z: drive paths
  let wineDllPath   = toWinePath dllPath
      wineGhostPath = toWinePath ghostPath
      cmd           = "LOAD " ++ wineDllPath ++ " " ++ wineGhostPath

  response <- sendCommand ws cmd
  case response of
    Left err -> return $ Left err
    Right resp ->
      let respClean = stripWhitespace resp
      in if respClean == "OK"
         then return $ Right ws
           { wsDllPath   = Just dllPath
           , wsGhostPath = Just ghostPath
           }
         else return $ Left $ "Load failed: " ++ resp

-- | Unload the currently loaded SHIORI DLL
unloadShiori :: WineShiori -> IO (Either String WineShiori)
unloadShiori ws = do
  response <- sendCommand ws "UNLOAD"
  case response of
    Left err -> return $ Left err
    Right resp ->
      let respClean = stripWhitespace resp
      in if respClean == "OK"
         then return $ Right ws
           { wsDllPath   = Nothing
           , wsGhostPath = Nothing
           }
         else return $ Left $ "Unload failed: " ++ resp

-- | Send a SHIORI request and receive the response
sendRequest :: WineShiori -> ShioriRequest -> IO (Either String ShioriResponse)
sendRequest ws req = do
  -- Check if SHIORI is loaded
  when (wsDllPath ws == Nothing) $
    return ()  -- Will fail at bridge level
  
  -- Serialize and Base64 encode
  let reqBytes = serializeShioriRequestBS req
      reqB64   = B64.encode reqBytes
      cmd      = "REQUEST " ++ BS8.unpack reqB64

  response <- sendCommand ws cmd
  case response of
    Left err -> return $ Left err
    Right resp -> parseResponse resp

-- | Send a SHIORI event (convenience wrapper)
-- Creates a GET request for the event with the given references
sendEvent :: WineShiori 
          -> ShioriEvent      -- ^ Event to send
          -> Map.Map Int Text -- ^ Reference parameters
          -> IO (Either String ShioriResponse)
sendEvent ws event refs = do
  let sender = wbcSenderName (wsConfig ws)
      req    = mkRequest sender (eventToId event) refs
  sendRequest ws req

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Parse bridge response line
parseResponse :: String -> IO (Either String ShioriResponse)
parseResponse resp
  | "RESPONSE " `isPrefixOf` resp = do
      -- Has Base64 payload
      let b64Data = drop 9 resp  -- Skip "RESPONSE "
      case B64.decode (BS8.pack b64Data) of
        Left err        -> return $ Left $ "Base64 decode error: " ++ err
        Right respBytes -> 
          case parseShioriResponseBS respBytes of
            Left err  -> return $ Left $ "Parse error: " ++ T.unpack err
            Right res -> return $ Right res
  
  | stripWhitespace resp == "RESPONSE" =
      -- Empty response (204 No Content)
      return $ Right emptyResponse
  
  | "ERROR " `isPrefixOf` resp =
      return $ Left $ drop 6 resp
  
  | otherwise =
      return $ Left $ "Unexpected response: " ++ resp

-- | Convert Unix path to Wine Z: drive path
toWinePath :: FilePath -> FilePath
toWinePath path
  | "/" `isPrefixOf` path = "Z:" ++ map toBackslash path
  | otherwise             = map toBackslash path
  where
    toBackslash '/' = '\\'
    toBackslash c   = c

-- | Check if string starts with prefix
isPrefixOf :: String -> String -> Bool
isPrefixOf prefix str = take (length prefix) str == prefix

-- | Strip whitespace characters
stripWhitespace :: String -> String
stripWhitespace = filter (`notElem` (" \r\n\t" :: String))
