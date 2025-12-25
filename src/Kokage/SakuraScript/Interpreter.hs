{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SakuraScript Interpreter
--
-- This module executes parsed SakuraScript, handling:
-- - Text display with character-by-character animation
-- - Wait commands (\w, \_w)
-- - Balloon commands (newline, clear)
-- - Scope switching (for multi-character ghosts)
--
-- The interpreter runs asynchronously and can be interrupted.
module Kokage.SakuraScript.Interpreter
  ( -- * Interpreter State
    InterpreterState(..)
  , InterpreterConfig(..)
  , defaultInterpreterConfig
    -- * Execution
  , executeScript
  , executeScriptAsync
    -- * Callbacks
  , InterpreterCallbacks(..)
  , defaultCallbacks
  ) where

import           Control.Concurrent         ( forkIO, threadDelay )
import           Control.Monad              ( forM_, unless )
import           Data.IORef                 ( IORef, newIORef, readIORef, writeIORef )
import qualified Data.Text                  as T

import           Types.SakuraScript

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for the script interpreter
data InterpreterConfig = InterpreterConfig
  { ecCharDelay     :: !Int    -- ^ Delay between characters in milliseconds (default: 50)
  , ecDefaultWait   :: !Int    -- ^ Default wait unit in milliseconds (default: 50)
  , ecQuickMode     :: !Bool   -- ^ Quick mode - skip character delays (default: False)
  }
  deriving (Show, Eq)

-- | Default interpreter configuration
defaultInterpreterConfig :: InterpreterConfig
defaultInterpreterConfig = InterpreterConfig
  { ecCharDelay   = 50    -- 50ms per character = 20 chars/second
  , ecDefaultWait = 50    -- \w[1] = 50ms
  , ecQuickMode   = False
  }

--------------------------------------------------------------------------------
-- Callbacks
--------------------------------------------------------------------------------

-- | Callbacks for interpreter actions
-- The interpreter calls these when it needs to update the UI or state.
data InterpreterCallbacks = InterpreterCallbacks
  { cbAppendChar    :: Char -> IO ()          -- ^ Append a single character to balloon
  , cbAppendText    :: T.Text -> IO ()        -- ^ Append text to balloon (for quick mode)
  , cbNewline       :: IO ()                   -- ^ Insert a newline
  , cbClear         :: IO ()                   -- ^ Clear the balloon
  , cbSetScope      :: Int -> IO ()            -- ^ Switch to character scope (0=sakura, 1=kero, etc.)
  , cbSetSurface    :: Int -> Int -> IO ()     -- ^ Set surface (scope, surfaceId)
  , cbAddChoice     :: T.Text -> T.Text -> T.Text -> IO ()  -- ^ Add a choice (id, text, action)
  , cbClearChoices  :: IO ()                   -- ^ Clear all choices
  , cbOnComplete    :: IO ()                   -- ^ Called when script execution completes
  , cbOnInterrupt   :: IO ()                   -- ^ Called when script is interrupted
  }

-- | Default callbacks that do nothing
defaultCallbacks :: InterpreterCallbacks
defaultCallbacks = InterpreterCallbacks
  { cbAppendChar   = \_ -> return ()
  , cbAppendText   = \_ -> return ()
  , cbNewline      = return ()
  , cbClear        = return ()
  , cbSetScope     = \_ -> return ()
  , cbSetSurface   = \_ _ -> return ()
  , cbAddChoice    = \_ _ _ -> return ()
  , cbClearChoices = return ()
  , cbOnComplete   = return ()
  , cbOnInterrupt  = return ()
  }

--------------------------------------------------------------------------------
-- Interpreter State
--------------------------------------------------------------------------------

-- | Mutable state for the script interpreter
data InterpreterState = InterpreterState
  { esConfig       :: !InterpreterConfig        -- ^ Configuration
  , esCallbacks    :: !InterpreterCallbacks     -- ^ UI callbacks
  , esCurrentScope :: !(IORef Int)           -- ^ Current character scope (0=sakura, 1=kero)
  , esInterrupted  :: !(IORef Bool)          -- ^ Interrupt flag
  , esQuickMode    :: !(IORef Bool)          -- ^ Quick mode flag (runtime toggle)
  , esScriptStart  :: !(IORef Int)           -- ^ Script start time in ms (for \__w)
  }

-- | Create a new interpreter state
newInterpreterState :: InterpreterConfig -> InterpreterCallbacks -> IO InterpreterState
newInterpreterState config callbacks = do
  scopeRef     <- newIORef 0
  interruptRef <- newIORef False
  quickRef     <- newIORef (ecQuickMode config)
  startRef     <- newIORef 0
  return InterpreterState
    { esConfig       = config
    , esCallbacks    = callbacks
    , esCurrentScope = scopeRef
    , esInterrupted  = interruptRef
    , esQuickMode    = quickRef
    , esScriptStart  = startRef
    }

--------------------------------------------------------------------------------
-- Execution
--------------------------------------------------------------------------------

-- | Execute a script synchronously (blocks until complete or interrupted)
executeScript :: InterpreterConfig -> InterpreterCallbacks -> Script -> IO ()
executeScript config callbacks script = do
  state <- newInterpreterState config callbacks
  runScript state script
  cbOnComplete callbacks

-- | Execute a script asynchronously
-- Returns an IO action to interrupt the execution
executeScriptAsync :: InterpreterConfig -> InterpreterCallbacks -> Script -> IO (IO ())
executeScriptAsync config callbacks script = do
  state <- newInterpreterState config callbacks
  _ <- forkIO $ do
    runScript state script
    interrupted <- readIORef (esInterrupted state)
    if interrupted
      then cbOnInterrupt callbacks
      else cbOnComplete callbacks
  -- Return interrupt action
  return $ writeIORef (esInterrupted state) True

-- | Run the script
runScript :: InterpreterState -> Script -> IO ()
runScript state = mapM_ (executeElement state)

-- | Execute a single script element
executeElement :: InterpreterState -> SakuraScript -> IO ()
executeElement state cmd = do
  interrupted <- readIORef (esInterrupted state)
  unless interrupted $ executeCmd state cmd

-- | Execute a single command
executeCmd :: InterpreterState -> SakuraScript -> IO ()
executeCmd state cmd = case cmd of
  -- Text display
  SSText text -> displayText state text

  -- Escaped character
  SSEscape c -> displayChar state c

  -- Scope switching
  SSScope scopeCmd -> handleScope state scopeCmd

  -- Surface changes
  SSSurface surfaceCmd -> handleSurface state surfaceCmd

  -- Balloon commands
  SSBalloon balloonCmd -> handleBalloon state balloonCmd

  -- Wait commands
  SSWait waitCmd -> handleWait state waitCmd

  -- Event commands
  SSEvent eventCmd -> handleEvent state eventCmd

  -- Choice commands
  SSChoice choiceCmd -> handleChoice state choiceCmd
  SSFont _   -> return ()   -- TODO: Font styling
  SSSound _  -> return ()   -- TODO: Sound playback
  SSOpen _   -> return ()   -- TODO: Open commands
  SSMeta _   -> return ()   -- TODO: Meta commands
  SSEnvVar _ -> return ()   -- TODO: Environment variables

--------------------------------------------------------------------------------
-- Text Display
--------------------------------------------------------------------------------

-- | Display text with character-by-character animation
displayText :: InterpreterState -> T.Text -> IO ()
displayText state text = do
  quick <- readIORef (esQuickMode state)
  if quick
    then cbAppendText (esCallbacks state) text
    else forM_ (T.unpack text) $ \c -> do
      interrupted <- readIORef (esInterrupted state)
      unless interrupted $ displayChar state c

-- | Display a single character with delay
displayChar :: InterpreterState -> Char -> IO ()
displayChar state c = do
  cbAppendChar (esCallbacks state) c
  quick <- readIORef (esQuickMode state)
  unless quick $ do
    let delayMs = ecCharDelay (esConfig state)
    threadDelay (delayMs * 1000)

--------------------------------------------------------------------------------
-- Scope Handling
--------------------------------------------------------------------------------

-- | Handle scope switching commands
handleScope :: InterpreterState -> ScopeCmd -> IO ()
handleScope state scopeCmd = do
  let scopeIdx = case scopeCmd of
        ScopeMain     -> 0
        ScopeKero     -> 1
        ScopeIndex n  -> n
  writeIORef (esCurrentScope state) scopeIdx
  cbSetScope (esCallbacks state) scopeIdx

--------------------------------------------------------------------------------
-- Surface Handling
--------------------------------------------------------------------------------

-- | Handle surface commands
handleSurface :: InterpreterState -> SurfaceCmd -> IO ()
handleSurface state surfaceCmd = case surfaceCmd of
  SurfaceChange surfaceId -> do
    scope <- readIORef (esCurrentScope state)
    cbSetSurface (esCallbacks state) scope surfaceId

  -- Other surface commands not yet implemented
  _ -> return ()

--------------------------------------------------------------------------------
-- Balloon Handling
--------------------------------------------------------------------------------

-- | Handle balloon commands
handleBalloon :: InterpreterState -> BalloonCmd -> IO ()
handleBalloon state balloonCmd = case balloonCmd of
  Newline           -> cbNewline (esCallbacks state)
  NewlineHalf       -> cbNewline (esCallbacks state)  -- TODO: half-height newline
  NewlinePercent _  -> cbNewline (esCallbacks state)  -- TODO: percentage newline
  Clear             -> cbClear (esCallbacks state)
  BalloonHide       -> return ()  -- TODO: Hide balloon
  BalloonShow       -> return ()  -- TODO: Show balloon
  _                 -> return ()  -- Other balloon commands not yet implemented

--------------------------------------------------------------------------------
-- Wait Handling
--------------------------------------------------------------------------------

-- | Handle wait commands
handleWait :: InterpreterState -> WaitCmd -> IO ()
handleWait state waitCmd = case waitCmd of
  -- \w[n] - Wait n units (each unit = defaultWait ms)
  WaitSimple n -> do
    let delayMs = n * ecDefaultWait (esConfig state)
    waitWithInterrupt state delayMs

  -- \_w[n] - Wait n milliseconds
  WaitMs n -> waitWithInterrupt state n

  -- \__w[n] - Wait until n ms since script start
  WaitUntil n -> do
    -- TODO: Need to track script start time properly
    -- For now, just wait the specified time
    waitWithInterrupt state n

  -- \x - Wait for click, then clear
  ClickWait -> do
    -- TODO: Implement click wait
    cbClear (esCallbacks state)

  -- \_q - Wait for click, no clear (also quick session)
  ClickWaitNoClear -> do
    -- TODO: Implement click wait
    return ()

  -- \t - Start/end time-critical section
  TimeCriticalStart -> do
    -- In time-critical mode, skip character delays
    writeIORef (esQuickMode state) True

  TimeCriticalEnd -> do
    -- Restore normal mode
    writeIORef (esQuickMode state) (ecQuickMode (esConfig state))

  -- Quick session start/end
  QuickStart -> writeIORef (esQuickMode state) True
  QuickEnd   -> writeIORef (esQuickMode state) (ecQuickMode (esConfig state))

  -- Other wait commands not yet implemented
  _ -> return ()

-- | Wait for specified milliseconds, checking for interrupts
waitWithInterrupt :: InterpreterState -> Int -> IO ()
waitWithInterrupt state delayMs = do
  -- Wait in small chunks to allow interrupt checking
  let chunkMs = 50
      chunks  = delayMs `div` chunkMs
      remainder = delayMs `mod` chunkMs

  forM_ [1..chunks] $ \_ -> do
    interrupted <- readIORef (esInterrupted state)
    unless interrupted $ threadDelay (chunkMs * 1000)

  -- Wait remainder
  interrupted <- readIORef (esInterrupted state)
  unless interrupted $ threadDelay (remainder * 1000)

--------------------------------------------------------------------------------
-- Event Handling
--------------------------------------------------------------------------------

-- | Handle event commands
handleEvent :: InterpreterState -> EventCmd -> IO ()
handleEvent state eventCmd = case eventCmd of
  -- \e - Exit script
  EventExit -> writeIORef (esInterrupted state) True

  -- \- - Chain to another script (for now, just stop current)
  EventScript _ _ -> writeIORef (esInterrupted state) True

  -- \![vanish] - Vanish ghost (stop script)
  EventVanish -> writeIORef (esInterrupted state) True

  -- Other event commands not yet implemented
  _ -> return ()

--------------------------------------------------------------------------------
-- Choice Handling
--------------------------------------------------------------------------------

-- | Handle choice commands (user-interactive menu items)
handleChoice :: InterpreterState -> ChoiceCmd -> IO ()
handleChoice state choiceCmd = case choiceCmd of
  -- \q[text,action] - Basic choice
  Choice text action -> do
    let choiceId = text  -- Use text as ID if no explicit ID
    addChoiceWithAction state choiceId text action

  -- \q[id,text,action] - Choice with explicit ID
  ChoiceID choiceId text action ->
    addChoiceWithAction state choiceId text action

  -- \__q[text,script] - Script choice (execute script directly)
  ChoiceScript text script ->
    cbAddChoice (esCallbacks state) text text ("script:" <> script)

  -- \_q[text,action] - No timeout choice (same as basic for now)
  ChoiceNoTimeout text action ->
    addChoiceWithAction state text text action

  -- Choice with timeout - not yet implemented
  ChoiceTimeout _ -> return ()

  -- \_a[id,text] - Named anchor (inline clickable link)
  Anchor anchorId text ->
    cbAddChoice (esCallbacks state) anchorId text ("anchor:" <> anchorId)

  -- Anchor end marker - no action needed
  AnchorEnd -> return ()

  -- \* - Implicit choice block (multiple choices)
  ChoiceBlock choices ->
    forM_ choices $ \(text, action) ->
      addChoiceWithAction state text text action

-- | Helper to add a choice, converting ChoiceAction to action string
addChoiceWithAction :: InterpreterState -> T.Text -> T.Text -> ChoiceAction -> IO ()
addChoiceWithAction state choiceId text action = do
  let actionStr = choiceActionToText action
  cbAddChoice (esCallbacks state) choiceId text actionStr

-- | Convert ChoiceAction to a text representation for the callback
choiceActionToText :: ChoiceAction -> T.Text
choiceActionToText action = case action of
  ChoiceEvent eventId     -> "event:" <> eventId
  ChoiceScript' script    -> "script:" <> script
  ChoiceURL url           -> "url:" <> url
  ChoiceOnEvent ref extra -> "onevent:" <> ref <> "," <> extra
