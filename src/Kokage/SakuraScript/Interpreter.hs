{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SakuraScript Interpreter
--
-- This module executes parsed SakuraScript, handling:
-- - Text display with character-by-character animation
-- - Wait commands (\w, \_w)
-- - Balloon commands (newline, clear)
-- - Scope switching (for multi-character ghosts)
-- - Surface changes and animations
-- - Sound playback
-- - Font styling
-- - Event raising
-- - Meta commands
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
  ) where

import           Control.Concurrent       ( MVar
                                          , forkIO
                                          , newEmptyMVar
                                          , putMVar
                                          , takeMVar
                                          , threadDelay
                                          )
import           Control.Concurrent.Async ( race )
import           Control.Monad            ( forM_, unless, void, when )

import           Data.IORef               ( IORef, modifyIORef', newIORef, readIORef, writeIORef )
import qualified Data.Text                as T
import           Data.Time.Clock          ( getCurrentTime )
import           Data.Time.Clock.POSIX    ( utcTimeToPOSIXSeconds )

import           Kokage.Callbacks         ( CallbackEnv(..) )
import qualified Kokage.Callbacks         as CB
import           Types.SakuraScript

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for the script interpreter
data InterpreterConfig
  = InterpreterConfig
  { ecCharDelay   :: !Int    -- ^ Delay between characters in milliseconds (default: 50)
  , ecDefaultWait :: !Int    -- ^ Default wait unit in milliseconds (default: 50)
  , ecQuickMode   :: !Bool   -- ^ Quick mode - skip character delays (default: False)
  }
  deriving ( Show, Eq )

-- | Default interpreter configuration
defaultInterpreterConfig :: InterpreterConfig
defaultInterpreterConfig
  = InterpreterConfig { ecCharDelay   = 100    -- 100ms per character = 10 chars/second
                      , ecDefaultWait = 50    -- \w[1] = 50ms
                      , ecQuickMode   = False
                      }

--------------------------------------------------------------------------------
-- Interpreter State
--------------------------------------------------------------------------------

-- | Mutable state for the script interpreter
data InterpreterState
  = InterpreterState
  { esConfig       :: !InterpreterConfig        -- ^ Configuration
  , esCallbackEnv  :: !CallbackEnv              -- ^ Callback environment
  , esCurrentScope :: !(IORef Int)              -- ^ Current character scope (0=sakura, 1=kero)
  , esInterrupted  :: !(IORef Bool)             -- ^ Interrupt flag
  , esQuickMode    :: !(IORef Bool)             -- ^ Quick mode flag (\_q toggle)
  , esTimeCritical :: !(IORef Bool)             -- ^ Time-critical section (\t) - blocks mouse events
  , esScriptStart  :: !(IORef Integer)          -- ^ Script start time in ms (for \__w)
  , esCharCount    :: !(IORef Int)              -- ^ Character count for talk animations
  , esNoUserBreak  :: !(IORef Bool)             -- ^ User break disabled flag
  , esVerbatim     :: !(IORef Bool)             -- ^ Verbatim mode flag
  }

-- | Create a new interpreter state
newInterpreterState :: InterpreterConfig -> CallbackEnv -> IO InterpreterState
newInterpreterState config callbackEnv = do
  scopeRef <- newIORef 0
  interruptRef <- newIORef False
  quickRef <- newIORef (ecQuickMode config)
  timeCriticalRef <- newIORef False
  now <- getCurrentTime
  let startMs = round (utcTimeToPOSIXSeconds now * 1000)
  startRef <- newIORef startMs
  charCountRef <- newIORef 0
  noBreakRef <- newIORef False
  verbatimRef <- newIORef False
  return
    InterpreterState
    { esConfig       = config
    , esCallbackEnv  = callbackEnv
    , esCurrentScope = scopeRef
    , esInterrupted  = interruptRef
    , esQuickMode    = quickRef
    , esTimeCritical = timeCriticalRef
    , esScriptStart  = startRef
    , esCharCount    = charCountRef
    , esNoUserBreak  = noBreakRef
    , esVerbatim     = verbatimRef
    }

--------------------------------------------------------------------------------
-- Execution
--------------------------------------------------------------------------------

-- | Execute a script synchronously (blocks until complete or interrupted)
executeScript :: InterpreterConfig -> CallbackEnv -> Script -> IO ()
executeScript config callbackEnv script = do
  state <- newInterpreterState config callbackEnv
  runScript state script
  CB.cbOnComplete callbackEnv

-- | Execute a script asynchronously
-- Returns an IO action to interrupt the execution
executeScriptAsync :: InterpreterConfig -> CallbackEnv -> Script -> IO (IO ())
executeScriptAsync config callbackEnv script = do
  state <- newInterpreterState config callbackEnv
  interruptVar <- newEmptyMVar :: IO (MVar ())
  -- Race between script execution and interrupt signal
  void $ forkIO $ do
    result <- race (takeMVar interruptVar) (runScript state script)
    case result of
      Left ()  -> CB.cbOnInterrupt callbackEnv  -- Interrupted
      Right () -> CB.cbOnComplete callbackEnv   -- Completed normally
  -- Return interrupt action that signals the MVar
  return $ putMVar interruptVar ()

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
  SSText text          -> displayText state text

  -- Escaped character
  SSEscape c           -> displayChar state c

  -- Scope switching
  SSScope scopeCmd     -> handleScope state scopeCmd

  -- Surface changes
  SSSurface surfaceCmd -> handleSurface state surfaceCmd

  -- Balloon commands
  SSBalloon balloonCmd -> handleBalloon state balloonCmd

  -- Wait commands
  SSWait waitCmd       -> handleWait state waitCmd

  -- Event commands
  SSEvent eventCmd     -> handleEvent state eventCmd

  -- Choice commands
  SSChoice choiceCmd   -> handleChoice state choiceCmd

  -- Font commands
  SSFont fontCmd       -> handleFont state fontCmd

  -- Sound commands
  SSSound soundCmd     -> handleSound state soundCmd

  -- Open commands
  SSOpen openCmd       -> handleOpen state openCmd

  -- Meta commands
  SSMeta metaCmd       -> handleMeta state metaCmd

  -- Environment variables
  SSEnvVar envVar      -> handleEnvVar state envVar

--------------------------------------------------------------------------------
-- Text Display
--------------------------------------------------------------------------------

-- | Display text with character-by-character animation
displayText :: InterpreterState -> T.Text -> IO ()
displayText state text = do
  quick <- readIORef (esQuickMode state)
  if quick
    then CB.cbAppendText (esCallbackEnv state) text
    else forM_ (T.unpack text) $ \c -> do
      interrupted <- readIORef (esInterrupted state)
      unless interrupted $ displayChar state c

-- | Display a single character with delay
displayChar :: InterpreterState -> Char -> IO ()
displayChar state c = do
  CB.cbAppendChar (esCallbackEnv state) c
  quick <- readIORef (esQuickMode state)
  unless quick $ do
    let delayMs = ecCharDelay (esConfig state)
    threadDelay (delayMs * 1000)  -- Convert ms to μs (threadDelay takes microseconds)

--------------------------------------------------------------------------------
-- Scope Handling
--------------------------------------------------------------------------------

-- | Handle scope switching commands
handleScope :: InterpreterState -> ScopeCmd -> IO ()
handleScope state scopeCmd = do
  let scopeIdx = case scopeCmd of
        ScopeMain    -> 0
        ScopeKero    -> 1
        ScopeIndex n -> n
  writeIORef (esCurrentScope state) scopeIdx
  CB.cbSetScope (esCallbackEnv state) scopeIdx

--------------------------------------------------------------------------------
-- Surface Handling
--------------------------------------------------------------------------------

-- | Handle surface commands
handleSurface :: InterpreterState -> SurfaceCmd -> IO ()
handleSurface state surfaceCmd = do
  scope <- readIORef (esCurrentScope state)
  case surfaceCmd of
    SurfaceChange surfaceId
      | surfaceId < 0 -> CB.cbHideCharacter (esCallbackEnv state) scope  -- \s[-1] hides character
      | otherwise     -> CB.cbSetSurface (esCallbackEnv state) scope surfaceId

    SurfaceChangeAlias _alias ->
      -- Alias lookup should be done at a higher level
      return ()

    SurfaceAnim animId action -> case action of
      AnimStart  -> CB.cbAnimStart (esCallbackEnv state) scope animId
      AnimStop   -> CB.cbAnimStop (esCallbackEnv state) scope animId
      AnimPause  -> CB.cbAnimStop (esCallbackEnv state) scope animId  -- Use stop for pause
      AnimResume -> CB.cbAnimStart (esCallbackEnv state) scope animId -- Use start for resume

    SurfaceAnimWait animId -> CB.cbAnimWait (esCallbackEnv state) scope animId

    SurfaceAnimClear mAnimId -> CB.cbAnimClear (esCallbackEnv state) scope mAnimId

    SurfaceAnimPause mAnimId -> case mAnimId of
      Just aid -> CB.cbAnimStop (esCallbackEnv state) scope aid
      Nothing  -> CB.cbAnimClear (esCallbackEnv state) scope Nothing

    SurfaceAnimResume mAnimId -> forM_ mAnimId (CB.cbAnimStart (esCallbackEnv state) scope)

    SurfaceAnimOffset _animId _x _y ->
      -- Animation offset is handled at rendering level
      return ()

    SurfaceBind category part enabled -> do
      let catText = case category of
            BindClothes   -> "clothes"
            BindAccessory -> "accessory"
            BindOther t   -> t
      CB.cbBindToggle (esCallbackEnv state) scope catText part enabled

    SurfaceLockRepaint _locked ->
      -- Repaint locking is handled at rendering level
      return ()

    SurfaceAlignment _target _align ->
      -- Alignment is handled at window level
      return ()

    SurfaceScaling _target _scale ->
      -- Scaling is handled at rendering level
      return ()

    SurfaceAlpha _alpha ->
      -- Alpha is handled at rendering level
      return ()

    SurfaceMove moveSpec -> do
      let x = cursorPosToInt (moveX moveSpec)
          y = cursorPosToInt (moveY moveSpec)
      CB.cbMove (esCallbackEnv state) scope x y (moveTime moveSpec) (moveAsync moveSpec)

    SurfaceOffset _x _y ->
      -- Offset is handled at rendering level
      return ()

-- | Convert CursorPos to Int (simplified)
cursorPosToInt :: CursorPos -> Int
cursorPosToInt pos = case pos of
  PosAbsolute n   -> n
  PosRelative n   -> n
  PosEm d         -> round (d * 16)  -- Approximate em to pixels
  PosLineHeight d -> round (d * 20) -- Approximate line height
  PosPercent _    -> 0               -- Needs context
  PosUnchanged    -> 0

--------------------------------------------------------------------------------
-- Balloon Handling
--------------------------------------------------------------------------------

-- | Handle balloon commands
handleBalloon :: InterpreterState -> BalloonCmd -> IO ()
handleBalloon state balloonCmd = do
  scope <- readIORef (esCurrentScope state)
  case balloonCmd of
    BalloonChange n ->
      -- Balloon ID calculation: balloonId = n / 2
      -- \b[0], \b[1] -> balloonId = 0 (default)
      -- \b[2], \b[3] -> balloonId = 1 (choice surface)
      -- etc.
      let
          balloonId = n `div` 2
        in
          CB.cbSetBalloon (esCallbackEnv state) scope balloonId

    BalloonHide -> CB.cbHideBalloon (esCallbackEnv state) scope

    BalloonShow -> CB.cbShowBalloon (esCallbackEnv state) scope

    BalloonImage _spec ->
      -- Image display is handled at balloon level
      return ()

    Newline -> CB.cbNewline (esCallbackEnv state)

    NewlineHalf -> CB.cbNewlineHalf (esCallbackEnv state)

    NewlinePercent pct -> CB.cbNewlinePercent (esCallbackEnv state) pct

    Clear -> CB.cbClear (esCallbackEnv state)

    ClearQuick -> do
      CB.cbClear (esCallbackEnv state)
      writeIORef (esQuickMode state) True

    ClearChars n -> CB.cbClearChars (esCallbackEnv state) n

    ClearLines _n ->
      -- Clear lines handled at balloon level
      return ()

    CursorMove xPos yPos -> do
      let x = cursorPosToInt xPos
          y = cursorPosToInt yPos
      CB.cbMoveCursor (esCallbackEnv state) x y

    AutoScrollDisable -> return ()

    AutoScrollEnable -> return ()

    BalloonOffset _x _y -> return ()

    BalloonAlign _dir -> return ()

    BalloonTimeout _timeout -> return ()

    Marker _target _prop _value -> return ()

    OnlineModeStart -> return ()

    OnlineModeEnd -> return ()

    NoUserBreakStart -> writeIORef (esNoUserBreak state) True

    NoUserBreakEnd -> writeIORef (esNoUserBreak state) False

    VerbatimStart -> writeIORef (esVerbatim state) True

    VerbatimEnd -> writeIORef (esVerbatim state) False

    SyncSection _mScopes -> return ()

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
  WaitUntil targetMs -> do
    startMs <- readIORef (esScriptStart state)
    now <- getCurrentTime
    let nowMs     = round (utcTimeToPOSIXSeconds now * 1000)
        elapsed   = nowMs - startMs
        remaining = fromIntegral targetMs - elapsed
    when (remaining > 0) $ waitWithInterrupt state (fromIntegral remaining)

  -- Wait for animation to complete
  WaitAnimComplete animId -> do
    scope <- readIORef (esCurrentScope state)
    CB.cbAnimWait (esCallbackEnv state) scope animId

  -- \x - Wait for click, then clear
  ClickWait -> do
    CB.cbOnClickWait
    CB.cbClear (esCallbackEnv state)

  -- \_q - Wait for click, no clear (also quick session)
  ClickWaitNoClear -> CB.cbOnClickWait

  -- \t - Time-critical section
  -- Blocks all OnMouse* events when enabled
  TimeCriticalStart -> do
    current <- readIORef (esTimeCritical state)
    let newVal = not current
    writeIORef (esTimeCritical state) newVal
    CB.cbSetTimeCritical (esCallbackEnv state) newVal

  TimeCriticalEnd -> return ()  -- Only TimeCriticalStart toggles; this is a no-op

  -- \_q - Toggle quick session (text displays immediately)
  QuickStart -> modifyIORef' (esQuickMode state) not
  QuickEnd -> writeIORef (esQuickMode state) (ecQuickMode (esConfig state))

  -- Sync commands
  SyncStart _name -> return ()
  SyncEnd _name -> return ()
  SyncScopes _scopes -> return ()

-- | Wait for specified milliseconds, checking for interrupts
waitWithInterrupt :: InterpreterState -> Int -> IO ()
waitWithInterrupt state delayMs = do
  -- Wait in small chunks to allow interrupt checking
  let chunkMs   = 50
      chunks    = delayMs `div` chunkMs
      remainder = delayMs `mod` chunkMs

  forM_ [ 1 .. chunks ] $ \_ -> do
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

  -- Close ghost
  EventClose -> writeIORef (esInterrupted state) True

  -- \- - Chain to another script (for now, just stop current)
  EventScript _ghost _script -> writeIORef (esInterrupted state) True

  -- \![raise,...] - Raise event
  EventRaise eventName refs -> CB.cbRaiseEvent eventName refs

  -- \![embed,...] - Embed script (handled at parse level)
  EventEmbed _script -> return ()

  -- \![notify,...] - Send notification
  EventNotify name refs _opts -> CB.cbNotify name refs

  -- \![timerraise,...] - Timed event
  EventTimerRaise name delayMs _opts -> CB.cbTimerRaise name delayMs

  -- Cancel timer
  EventTimerCancel name -> CB.cbTimerCancel name

  -- Update commands
  EventUpdate _updateCmd -> return ()

  -- Change ghost
  EventGhostChange ghostName -> CB.cbGhostChange ghostName

  -- Change shell
  EventShellChange shellName -> CB.cbShellChange shellName

  -- Change balloon
  EventBalloonChange balloonName -> CB.cbBalloonStyleChange balloonName

  -- \![vanish] - Vanish ghost (stop script)
  EventVanish -> writeIORef (esInterrupted state) True

--------------------------------------------------------------------------------
-- Font Handling
--------------------------------------------------------------------------------

-- | Handle font commands
handleFont :: InterpreterState -> FontCmd -> IO ()
handleFont state = CB.cbSetFont (esCallbackEnv state)

--------------------------------------------------------------------------------
-- Sound Handling
--------------------------------------------------------------------------------

-- | Handle sound commands
handleSound :: InterpreterState -> SoundCmd -> IO ()
handleSound state soundCmd = case soundCmd of
  SoundPlay file -> CB.cbPlaySound (esCallbackEnv state) file

  SoundStop -> CB.cbStopSound (esCallbackEnv state)

  SoundAction action file _args -> CB.cbSoundAction (esCallbackEnv state) action file

--------------------------------------------------------------------------------
-- Open Handling
--------------------------------------------------------------------------------

-- | Handle open commands
handleOpen :: InterpreterState -> OpenCmd -> IO ()
handleOpen state openCmd = case openCmd of
  OpenURL url -> CB.cbOpenURL url

  OpenBrowser url -> CB.cbOpenURL url

  OpenMailer _address -> return ()

  OpenFile file -> CB.cbOpenFile file

  OpenEditor file -> CB.cbOpenFile file

  OpenInputBox eventId opts -> CB.cbOpenInputBox eventId opts

  OpenDialog msg opt -> CB.cbOpenDialog msg opt

  OpenCommunicate _ghost _opt -> return ()

  OpenTeachBox -> return ()

  OpenConfigMenu -> return ()

--------------------------------------------------------------------------------
-- Meta Handling
--------------------------------------------------------------------------------

-- | Handle meta commands
handleMeta :: InterpreterState -> MetaCmd -> IO ()
handleMeta state metaCmd = case metaCmd of
  MetaSet prop -> CB.cbSetProperty prop

  MetaGet _prop ->
    -- Get is typically handled inline, not as a command
    return ()

  MetaReload target -> CB.cbReload target

  MetaExecute execCmd -> CB.cbExecute execCmd

  MetaPassiveMode enabled -> CB.cbSetPassiveMode enabled

  MetaInductionMode _enabled -> return ()

  MetaLock component -> CB.cbLock component

  MetaUnlock component -> CB.cbUnlock component

--------------------------------------------------------------------------------
-- Environment Variable Handling
--------------------------------------------------------------------------------

-- | Handle environment variable substitution
handleEnvVar :: InterpreterState -> EnvVar -> IO ()
handleEnvVar state envVar = do
  value <- CB.cbGetEnvVar (esCallbackEnv state) envVar
  unless (T.null value) $ displayText state value

-- | Handle choice commands (user-interactive menu items)
handleChoice :: InterpreterState -> ChoiceCmd -> IO ()
handleChoice state choiceCmd = case choiceCmd of
  -- \q[text,action] - Basic choice
  Choice text action -> do
    let choiceId = text  -- Use text as ID if no explicit ID
    addChoiceWithAction state choiceId text action

  -- \q[id,text,action] - Choice with explicit ID
  ChoiceID choiceId text action -> addChoiceWithAction state choiceId text action

  -- \__q[text,script] - Script choice (execute script directly)
  ChoiceScript text script -> CB.cbAddChoice (esCallbackEnv state) text text ("script:" <> script)

  -- \_q[text,action] - No timeout choice (same as basic for now)
  ChoiceNoTimeout text action -> addChoiceWithAction state text text action

  -- Choice with timeout - not yet implemented
  ChoiceTimeout _ -> return ()

  -- \_a[id,text] - Named anchor (inline clickable link)
  Anchor anchorId text -> CB.cbAddChoice (esCallbackEnv state) anchorId text ("anchor:" <> anchorId)

  -- Anchor end marker - no action needed
  AnchorEnd -> return ()

  -- \* - Implicit choice block (multiple choices)
  ChoiceBlock
    choices -> forM_ choices $ \( text, action ) -> addChoiceWithAction state text text action

  -- \![*] - Choice marker (visual bullet point, no action needed)
  ChoiceMarker -> return ()

-- | Helper to add a choice, converting ChoiceAction to action string
addChoiceWithAction :: InterpreterState -> T.Text -> T.Text -> ChoiceAction -> IO ()
addChoiceWithAction state choiceId text action = do
  let actionStr = choiceActionToText action
  CB.cbAddChoice (esCallbackEnv state) choiceId text actionStr

-- | Convert ChoiceAction to a text representation for the callback
choiceActionToText :: ChoiceAction -> T.Text
choiceActionToText action = case action of
  ChoiceEvent eventId     -> "event:" <> eventId
  ChoiceScript' script    -> "script:" <> script
  ChoiceURL url           -> "url:" <> url
  ChoiceOnEvent ref extra -> "onevent:" <> ref <> "," <> extra
