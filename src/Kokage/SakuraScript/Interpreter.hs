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
    -- * Callbacks
  , InterpreterCallbacks(..)
  , defaultCallbacks
  ) where

import           Control.Concurrent         ( threadDelay, newEmptyMVar, putMVar, MVar, forkIO, takeMVar )
import           Control.Concurrent.Async   ( race )
import           Control.Monad              ( forM_, unless, void, when )
import           Data.IORef                 ( IORef, newIORef, readIORef, writeIORef, modifyIORef' )
import           Data.Time.Clock            ( getCurrentTime, diffUTCTime )
import           Data.Time.Clock.POSIX      ( utcTimeToPOSIXSeconds )
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
  { ecCharDelay   = 100    -- 100ms per character = 10 chars/second
  , ecDefaultWait = 50    -- \w[1] = 50ms
  , ecQuickMode   = False
  }

--------------------------------------------------------------------------------
-- Callbacks
--------------------------------------------------------------------------------

-- | Callbacks for interpreter actions
-- The interpreter calls these when it needs to update the UI or state.
data InterpreterCallbacks = InterpreterCallbacks
  { cbAppendChar    :: Char -> IO ()              -- ^ Append a single character to balloon
  , cbAppendText    :: T.Text -> IO ()            -- ^ Append text to balloon (for quick mode)
  , cbNewline       :: IO ()                       -- ^ Insert a newline
  , cbNewlineHalf   :: IO ()                       -- ^ Insert a half-height newline
  , cbNewlinePercent :: Int -> IO ()              -- ^ Insert a percentage newline
  , cbClear         :: IO ()                       -- ^ Clear the balloon
  , cbClearChars    :: Int -> IO ()               -- ^ Clear n characters
  , cbSetScope      :: Int -> IO ()                -- ^ Switch to character scope (0=sakura, 1=kero, etc.)
  , cbSetSurface    :: Int -> Int -> IO ()         -- ^ Set surface (scope, surfaceId)
  , cbSetBalloon    :: Int -> Int -> IO ()         -- ^ Set balloon (scope, balloonId)
  , cbHideBalloon   :: Int -> IO ()                -- ^ Hide balloon for scope
  , cbShowBalloon   :: Int -> IO ()                -- ^ Show balloon for scope
  , cbMoveCursor    :: Int -> Int -> IO ()         -- ^ Move cursor to position (x, y)
  , cbAddChoice     :: T.Text -> T.Text -> T.Text -> IO ()  -- ^ Add a choice (id, text, action)
  , cbClearChoices  :: IO ()                       -- ^ Clear all choices
  -- Animation callbacks
  , cbAnimStart     :: Int -> Int -> IO ()         -- ^ Start animation (scope, animId)
  , cbAnimStop      :: Int -> Int -> IO ()         -- ^ Stop animation (scope, animId)
  , cbAnimWait      :: Int -> Int -> IO ()         -- ^ Wait for animation (scope, animId)
  , cbAnimClear     :: Int -> Maybe Int -> IO ()   -- ^ Clear animations (scope, Maybe animId)
  , cbBindToggle    :: Int -> T.Text -> T.Text -> Bool -> IO ()  -- ^ Toggle bind (scope, category, part, enabled)
  -- Move callbacks
  , cbMove          :: Int -> Int -> Int -> Maybe Int -> Bool -> IO ()  -- ^ Move (scope, x, y, time, async)
  -- Font callbacks
  , cbSetFont       :: FontCmd -> IO ()            -- ^ Set font properties
  -- Sound callbacks
  , cbPlaySound     :: T.Text -> IO ()             -- ^ Play sound file
  , cbStopSound     :: IO ()                       -- ^ Stop all sounds
  , cbSoundAction   :: SoundAction -> T.Text -> IO ()  -- ^ Sound action
  -- Event callbacks
  , cbRaiseEvent    :: T.Text -> [T.Text] -> IO () -- ^ Raise event (name, refs)
  , cbNotify        :: T.Text -> [T.Text] -> IO () -- ^ Send notification
  , cbTimerRaise    :: T.Text -> Int -> IO ()      -- ^ Set timed event (name, ms)
  , cbTimerCancel   :: T.Text -> IO ()             -- ^ Cancel timer
  , cbGhostChange   :: T.Text -> IO ()             -- ^ Change to another ghost
  , cbShellChange   :: T.Text -> IO ()             -- ^ Change shell
  , cbBalloonStyleChange :: T.Text -> IO ()        -- ^ Change balloon style
  -- Open callbacks
  , cbOpenURL       :: T.Text -> IO ()             -- ^ Open URL in browser
  , cbOpenFile      :: T.Text -> IO ()             -- ^ Open file
  , cbOpenInputBox  :: T.Text -> [InputBoxOpt] -> IO ()  -- ^ Open input dialog
  , cbOpenDialog    :: T.Text -> DialogOpt -> IO () -- ^ Open dialog
  -- Meta callbacks
  , cbSetProperty   :: SetProperty -> IO ()        -- ^ Set property
  , cbGetProperty   :: GetProperty -> IO T.Text    -- ^ Get property value
  , cbReload        :: ReloadTarget -> IO ()       -- ^ Reload target
  , cbExecute       :: ExecuteCmd -> IO ()         -- ^ Execute command
  , cbSetPassiveMode :: Bool -> IO ()              -- ^ Set passive mode
  , cbLock          :: T.Text -> IO ()             -- ^ Lock component
  , cbUnlock        :: T.Text -> IO ()             -- ^ Unlock component
  -- EnvVar callback
  , cbGetEnvVar     :: EnvVar -> IO T.Text         -- ^ Get environment variable value
  -- Completion callbacks
  , cbOnComplete    :: IO ()                       -- ^ Called when script execution completes
  , cbOnInterrupt   :: IO ()                       -- ^ Called when script is interrupted
  , cbOnClickWait   :: IO ()                       -- ^ Called when waiting for click
  }

-- | Default callbacks that do nothing
defaultCallbacks :: InterpreterCallbacks
defaultCallbacks = InterpreterCallbacks
  { cbAppendChar   = \_ -> return ()
  , cbAppendText   = \_ -> return ()
  , cbNewline      = return ()
  , cbNewlineHalf  = return ()
  , cbNewlinePercent = \_ -> return ()
  , cbClear        = return ()
  , cbClearChars   = \_ -> return ()
  , cbSetScope     = \_ -> return ()
  , cbSetSurface   = \_ _ -> return ()
  , cbSetBalloon   = \_ _ -> return ()
  , cbHideBalloon  = \_ -> return ()
  , cbShowBalloon  = \_ -> return ()
  , cbMoveCursor   = \_ _ -> return ()
  , cbAddChoice    = \_ _ _ -> return ()
  , cbClearChoices = return ()
  , cbAnimStart    = \_ _ -> return ()
  , cbAnimStop     = \_ _ -> return ()
  , cbAnimWait     = \_ _ -> return ()
  , cbAnimClear    = \_ _ -> return ()
  , cbBindToggle   = \_ _ _ _ -> return ()
  , cbMove         = \_ _ _ _ _ -> return ()
  , cbSetFont      = \_ -> return ()
  , cbPlaySound    = \_ -> return ()
  , cbStopSound    = return ()
  , cbSoundAction  = \_ _ -> return ()
  , cbRaiseEvent   = \_ _ -> return ()
  , cbNotify       = \_ _ -> return ()
  , cbTimerRaise   = \_ _ -> return ()
  , cbTimerCancel  = \_ -> return ()
  , cbGhostChange  = \_ -> return ()
  , cbShellChange  = \_ -> return ()
  , cbBalloonStyleChange = \_ -> return ()
  , cbOpenURL      = \_ -> return ()
  , cbOpenFile     = \_ -> return ()
  , cbOpenInputBox = \_ _ -> return ()
  , cbOpenDialog   = \_ _ -> return ()
  , cbSetProperty  = \_ -> return ()
  , cbGetProperty  = \_ -> return ""
  , cbReload       = \_ -> return ()
  , cbExecute      = \_ -> return ()
  , cbSetPassiveMode = \_ -> return ()
  , cbLock         = \_ -> return ()
  , cbUnlock       = \_ -> return ()
  , cbGetEnvVar    = \_ -> return ""
  , cbOnComplete   = return ()
  , cbOnInterrupt  = return ()
  , cbOnClickWait  = return ()
  }

--------------------------------------------------------------------------------
-- Interpreter State
--------------------------------------------------------------------------------

-- | Mutable state for the script interpreter
data InterpreterState = InterpreterState
  { esConfig       :: !InterpreterConfig        -- ^ Configuration
  , esCallbacks    :: !InterpreterCallbacks     -- ^ UI callbacks
  , esCurrentScope :: !(IORef Int)              -- ^ Current character scope (0=sakura, 1=kero)
  , esInterrupted  :: !(IORef Bool)             -- ^ Interrupt flag
  , esQuickMode    :: !(IORef Bool)             -- ^ Quick mode flag (runtime toggle)
  , esScriptStart  :: !(IORef Integer)          -- ^ Script start time in ms (for \__w)
  , esCharCount    :: !(IORef Int)              -- ^ Character count for talk animations
  , esNoUserBreak  :: !(IORef Bool)             -- ^ User break disabled flag
  , esVerbatim     :: !(IORef Bool)             -- ^ Verbatim mode flag
  }

-- | Create a new interpreter state
newInterpreterState :: InterpreterConfig -> InterpreterCallbacks -> IO InterpreterState
newInterpreterState config callbacks = do
  scopeRef     <- newIORef 0
  interruptRef <- newIORef False
  quickRef     <- newIORef (ecQuickMode config)
  now          <- getCurrentTime
  let startMs = round (utcTimeToPOSIXSeconds now * 1000)
  startRef     <- newIORef startMs
  charCountRef <- newIORef 0
  noBreakRef   <- newIORef False
  verbatimRef  <- newIORef False
  return InterpreterState
    { esConfig       = config
    , esCallbacks    = callbacks
    , esCurrentScope = scopeRef
    , esInterrupted  = interruptRef
    , esQuickMode    = quickRef
    , esScriptStart  = startRef
    , esCharCount    = charCountRef
    , esNoUserBreak  = noBreakRef
    , esVerbatim     = verbatimRef
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
  interruptVar <- newEmptyMVar :: IO (MVar ())
  -- Race between script execution and interrupt signal
  void $ forkIO $ do
    result <- race (takeMVar interruptVar) (runScript state script)
    case result of
      Left ()  -> cbOnInterrupt callbacks  -- Interrupted
      Right () -> cbOnComplete callbacks   -- Completed normally
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

  -- Font commands
  SSFont fontCmd -> handleFont state fontCmd

  -- Sound commands
  SSSound soundCmd -> handleSound state soundCmd

  -- Open commands
  SSOpen openCmd -> handleOpen state openCmd

  -- Meta commands
  SSMeta metaCmd -> handleMeta state metaCmd

  -- Environment variables
  SSEnvVar envVar -> handleEnvVar state envVar

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
    threadDelay (delayMs * 1000)  -- Convert ms to μs (threadDelay takes microseconds)

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
handleSurface state surfaceCmd = do
  scope <- readIORef (esCurrentScope state)
  case surfaceCmd of
    SurfaceChange surfaceId ->
      cbSetSurface (esCallbacks state) scope surfaceId

    SurfaceChangeAlias _alias ->
      -- Alias lookup should be done at a higher level
      return ()

    SurfaceAnim animId action -> case action of
      AnimStart  -> cbAnimStart (esCallbacks state) scope animId
      AnimStop   -> cbAnimStop (esCallbacks state) scope animId
      AnimPause  -> cbAnimStop (esCallbacks state) scope animId  -- Use stop for pause
      AnimResume -> cbAnimStart (esCallbacks state) scope animId -- Use start for resume

    SurfaceAnimWait animId ->
      cbAnimWait (esCallbacks state) scope animId

    SurfaceAnimClear mAnimId ->
      cbAnimClear (esCallbacks state) scope mAnimId

    SurfaceAnimPause mAnimId ->
      case mAnimId of
        Just aid -> cbAnimStop (esCallbacks state) scope aid
        Nothing  -> cbAnimClear (esCallbacks state) scope Nothing

    SurfaceAnimResume mAnimId ->
      case mAnimId of
        Just aid -> cbAnimStart (esCallbacks state) scope aid
        Nothing  -> return ()

    SurfaceAnimOffset _animId _x _y ->
      -- Animation offset is handled at rendering level
      return ()

    SurfaceBind category part enabled -> do
      let catText = case category of
            BindClothes   -> "clothes"
            BindAccessory -> "accessory"
            BindOther t   -> t
      cbBindToggle (esCallbacks state) scope catText part enabled

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
      cbMove (esCallbacks state) scope x y (moveTime moveSpec) (moveAsync moveSpec)

    SurfaceOffset _x _y ->
      -- Offset is handled at rendering level
      return ()

-- | Convert CursorPos to Int (simplified)
cursorPosToInt :: CursorPos -> Int
cursorPosToInt pos = case pos of
  PosAbsolute n  -> n
  PosRelative n  -> n
  PosEm d        -> round (d * 16)  -- Approximate em to pixels
  PosLineHeight d -> round (d * 20) -- Approximate line height
  PosPercent _   -> 0               -- Needs context
  PosUnchanged   -> 0

--------------------------------------------------------------------------------
-- Balloon Handling
--------------------------------------------------------------------------------

-- | Handle balloon commands
handleBalloon :: InterpreterState -> BalloonCmd -> IO ()
handleBalloon state balloonCmd = do
  scope <- readIORef (esCurrentScope state)
  case balloonCmd of
    BalloonChange balloonId ->
      cbSetBalloon (esCallbacks state) scope balloonId

    BalloonHide ->
      cbHideBalloon (esCallbacks state) scope

    BalloonShow ->
      cbShowBalloon (esCallbacks state) scope

    BalloonImage _spec ->
      -- Image display is handled at balloon level
      return ()

    Newline ->
      cbNewline (esCallbacks state)

    NewlineHalf ->
      cbNewlineHalf (esCallbacks state)

    NewlinePercent pct ->
      cbNewlinePercent (esCallbacks state) pct

    Clear ->
      cbClear (esCallbacks state)

    ClearChars n ->
      cbClearChars (esCallbacks state) n

    ClearLines _n ->
      -- Clear lines handled at balloon level
      return ()

    CursorMove xPos yPos -> do
      let x = cursorPosToInt xPos
          y = cursorPosToInt yPos
      cbMoveCursor (esCallbacks state) x y

    AutoScrollDisable ->
      return ()

    AutoScrollEnable ->
      return ()

    BalloonOffset _x _y ->
      return ()

    BalloonAlign _dir ->
      return ()

    BalloonTimeout _timeout ->
      return ()

    Marker _target _prop _value ->
      return ()

    OnlineModeStart ->
      return ()

    OnlineModeEnd ->
      return ()

    NoUserBreakStart ->
      writeIORef (esNoUserBreak state) True

    NoUserBreakEnd ->
      writeIORef (esNoUserBreak state) False

    VerbatimStart ->
      writeIORef (esVerbatim state) True

    VerbatimEnd ->
      writeIORef (esVerbatim state) False

    SyncSection _mScopes ->
      return ()

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
    let nowMs = round (utcTimeToPOSIXSeconds now * 1000)
        elapsed = nowMs - startMs
        remaining = fromIntegral targetMs - elapsed
    when (remaining > 0) $
      waitWithInterrupt state (fromIntegral remaining)

  -- Wait for animation to complete
  WaitAnimComplete animId -> do
    scope <- readIORef (esCurrentScope state)
    cbAnimWait (esCallbacks state) scope animId

  -- \x - Wait for click, then clear
  ClickWait -> do
    cbOnClickWait (esCallbacks state)
    cbClear (esCallbacks state)

  -- \_q - Wait for click, no clear (also quick session)
  ClickWaitNoClear ->
    cbOnClickWait (esCallbacks state)

  -- \t - Start/end time-critical section
  TimeCriticalStart ->
    writeIORef (esQuickMode state) True

  TimeCriticalEnd ->
    writeIORef (esQuickMode state) (ecQuickMode (esConfig state))

  -- Quick session start/end
  QuickStart -> writeIORef (esQuickMode state) True
  QuickEnd   -> writeIORef (esQuickMode state) (ecQuickMode (esConfig state))

  -- Sync commands
  SyncStart _name -> return ()
  SyncEnd _name -> return ()
  SyncScopes _scopes -> return ()

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

  -- Close ghost
  EventClose -> writeIORef (esInterrupted state) True

  -- \- - Chain to another script (for now, just stop current)
  EventScript _ghost _script -> writeIORef (esInterrupted state) True

  -- \![raise,...] - Raise event
  EventRaise eventName refs ->
    cbRaiseEvent (esCallbacks state) eventName refs

  -- \![embed,...] - Embed script (handled at parse level)
  EventEmbed _script -> return ()

  -- \![notify,...] - Send notification
  EventNotify name refs _opts ->
    cbNotify (esCallbacks state) name refs

  -- \![timerraise,...] - Timed event
  EventTimerRaise name delayMs _opts ->
    cbTimerRaise (esCallbacks state) name delayMs

  -- Cancel timer
  EventTimerCancel name ->
    cbTimerCancel (esCallbacks state) name

  -- Update commands
  EventUpdate _updateCmd -> return ()

  -- Change ghost
  EventGhostChange ghostName ->
    cbGhostChange (esCallbacks state) ghostName

  -- Change shell
  EventShellChange shellName ->
    cbShellChange (esCallbacks state) shellName

  -- Change balloon
  EventBalloonChange balloonName ->
    cbBalloonStyleChange (esCallbacks state) balloonName

  -- \![vanish] - Vanish ghost (stop script)
  EventVanish -> writeIORef (esInterrupted state) True

--------------------------------------------------------------------------------
-- Font Handling
--------------------------------------------------------------------------------

-- | Handle font commands
handleFont :: InterpreterState -> FontCmd -> IO ()
handleFont state fontCmd =
  cbSetFont (esCallbacks state) fontCmd

--------------------------------------------------------------------------------
-- Sound Handling
--------------------------------------------------------------------------------

-- | Handle sound commands
handleSound :: InterpreterState -> SoundCmd -> IO ()
handleSound state soundCmd = case soundCmd of
  SoundPlay file ->
    cbPlaySound (esCallbacks state) file

  SoundStop ->
    cbStopSound (esCallbacks state)

  SoundAction action file _args ->
    cbSoundAction (esCallbacks state) action file

--------------------------------------------------------------------------------
-- Open Handling
--------------------------------------------------------------------------------

-- | Handle open commands
handleOpen :: InterpreterState -> OpenCmd -> IO ()
handleOpen state openCmd = case openCmd of
  OpenURL url ->
    cbOpenURL (esCallbacks state) url

  OpenBrowser url ->
    cbOpenURL (esCallbacks state) url

  OpenMailer _address ->
    return ()

  OpenFile file ->
    cbOpenFile (esCallbacks state) file

  OpenEditor file ->
    cbOpenFile (esCallbacks state) file

  OpenInputBox eventId opts ->
    cbOpenInputBox (esCallbacks state) eventId opts

  OpenDialog msg opt ->
    cbOpenDialog (esCallbacks state) msg opt

  OpenCommunicate _ghost _opt ->
    return ()

  OpenTeachBox ->
    return ()

  OpenConfigMenu ->
    return ()

--------------------------------------------------------------------------------
-- Meta Handling
--------------------------------------------------------------------------------

-- | Handle meta commands
handleMeta :: InterpreterState -> MetaCmd -> IO ()
handleMeta state metaCmd = case metaCmd of
  MetaSet prop ->
    cbSetProperty (esCallbacks state) prop

  MetaGet _prop ->
    -- Get is typically handled inline, not as a command
    return ()

  MetaReload target ->
    cbReload (esCallbacks state) target

  MetaExecute execCmd ->
    cbExecute (esCallbacks state) execCmd

  MetaPassiveMode enabled ->
    cbSetPassiveMode (esCallbacks state) enabled

  MetaInductionMode _enabled ->
    return ()

  MetaLock component ->
    cbLock (esCallbacks state) component

  MetaUnlock component ->
    cbUnlock (esCallbacks state) component

--------------------------------------------------------------------------------
-- Environment Variable Handling
--------------------------------------------------------------------------------

-- | Handle environment variable substitution
handleEnvVar :: InterpreterState -> EnvVar -> IO ()
handleEnvVar state envVar = do
  value <- cbGetEnvVar (esCallbacks state) envVar
  unless (T.null value) $
    displayText state value

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
