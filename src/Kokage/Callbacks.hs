{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Callback functions for the SakuraScript interpreter.
-- All callbacks are defined as top-level functions receiving a 'CallbackEnv'.
module Kokage.Callbacks
  ( -- * Environment
    CallbackEnv (..)
    -- * Text callbacks
  , cbAppendChar
  , cbAppendText
  , cbNewline
  , cbNewlineHalf
  , cbNewlinePercent
  , cbClear
  , cbClearChars
    -- * Scope/Surface callbacks
  , cbSetScope
  , cbSetSurface
  , cbHideCharacter
    -- * Balloon callbacks
  , cbSetBalloon
  , cbHideBalloon
  , cbShowBalloon
  , cbMoveCursor
  , cbAddChoice
  , cbClearChoices
    -- * Animation callbacks
  , cbAnimStart
  , cbAnimStop
  , cbAnimWait
  , cbAnimClear
  , cbBindToggle
    -- * Move callback
  , cbMove
    -- * Font callback
  , cbSetFont
    -- * Sound callbacks
  , cbPlaySound
  , cbStopSound
  , cbSoundAction
    -- * Event callbacks
  , cbRaiseEvent
  , cbNotify
  , cbTimerRaise
  , cbTimerCancel
  , cbGhostChange
  , cbShellChange
  , cbBalloonStyleChange
    -- * Open callbacks
  , cbOpenURL
  , cbOpenFile
  , cbOpenInputBox
  , cbOpenDialog
    -- * Meta callbacks
  , cbSetProperty
  , cbGetProperty
  , cbReload
  , cbExecute
  , cbSetPassiveMode
  , cbLock
  , cbUnlock
    -- * Environment variable callback
  , cbGetEnvVar
    -- * Completion callbacks
  , cbOnComplete
  , cbOnInterrupt
  , cbOnClickWait
    -- * Time-critical callback
  , cbSetTimeCritical
  ) where

import Prelude ()
import Relude

import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import           Data.Time.Calendar         ( toGregorian )
import           Data.Time.Calendar.WeekDate ( toWeekDate )
import           Data.Time.Clock            ( getCurrentTime, utctDay, utctDayTime )
import           Data.Time.LocalTime        ( TimeOfDay (..), timeToTimeOfDay )
import qualified GI.Gdk                     as Gdk
import qualified GI.Gio                     as Gio
import qualified GI.GLib                    as GLib
import qualified GI.Gtk                     as Gtk
import           Data.GI.Base               ( AttrOp (..) )

import           Kokage.Animation           ( ActiveAnim (..)
                                            , asActiveAnims
                                            , clearAnimations
                                            , getEnabledBinds
                                            , stopAnimation
                                            , toggleBind
                                            )
import           Kokage.Balloon             ( BalloonChoice (..)
                                            , BalloonState
                                            , addChoice
                                            , appendChar
                                            , appendNewline
                                            , appendNewlineHalf
                                            , appendNewlinePercent
                                            , appendText
                                            , clearBalloon
                                            , clearChars
                                            , clearChoices
                                            , getFontSize
                                            , hideBalloon
                                            , moveCursor
                                            , resetFont
                                            , setBalloonId
                                            , setFontBold
                                            , setFontColor
                                            , setFontItalic
                                            , setFontName
                                            , setFontSize
                                            , setFontStrike
                                            , setFontSub
                                            , setFontSup
                                            , setFontUnderline
                                            , showBalloon
                                            )
import           Kokage.Character           ( CharacterState
                                            , csAnimState
                                            , csCurrentSurface
                                            , getCharacterBalloon
                                            , hideCharacter
                                            , setCharacterPosition
                                            )
import           Types.SakuraScript         ( Color (..)
                                            , DialogOpt (..)
                                            , EnvVar (..)
                                            , ExecuteCmd (..)
                                            , FontCmd (..)
                                            , FontSize (..)
                                            , FontToggle (..)
                                            , GetProperty (..)
                                            , InputBoxOpt (..)
                                            , ReloadTarget (..)
                                            , SetProperty (..)
                                            , SoundAction (..)
                                            )
import           Kokage.Sound               ( SoundState, playSound, stopSound )
import           Types.Ghost.Surface        ( Animation (..) )

-- | Environment containing all dependencies needed by callbacks.
data CallbackEnv = CallbackEnv
  { ceCharacters       :: !(Map.Map Int CharacterState)
    -- ^ All character states indexed by scope
  , ceCurrentScopeRef  :: !(IORef Int)
    -- ^ Current active scope (0=sakura, 1=kero)
  , ceSoundState       :: !SoundState
    -- ^ Sound playback state
  , ceFireTimeCritical :: !(Bool -> IO ())
    -- ^ Fire time-critical state change event
  , ceChangeSurface    :: !(Int -> Int -> IO ())
    -- ^ Change surface for a scope
  , ceHideBalloonIfNoChoices :: !(IO ())
    -- ^ Hide balloon if no choices are present
  , ceCancelBalloonHideTimer :: !(IO ())
    -- ^ Cancel any pending balloon hide timer
  }

-- | Get the balloon for the current scope.
getCurrentBalloon :: CallbackEnv -> IO BalloonState
getCurrentBalloon env = do
  scope <- readIORef (ceCurrentScopeRef env)
  case Map.lookup scope (ceCharacters env) of
    Just cs -> return $ getCharacterBalloon cs
    Nothing -> case Map.lookup 0 (ceCharacters env) of
      Just cs -> return $ getCharacterBalloon cs
      Nothing -> error "No character found for balloon"

-- -----------------------------------------------------------------------------
-- Text callbacks
-- -----------------------------------------------------------------------------

cbAppendChar :: CallbackEnv -> Char -> IO ()
cbAppendChar env c = getCurrentBalloon env >>= \b -> appendChar b c

cbAppendText :: CallbackEnv -> T.Text -> IO ()
cbAppendText env t = getCurrentBalloon env >>= \b -> appendText b t

cbNewline :: CallbackEnv -> IO ()
cbNewline env = getCurrentBalloon env >>= appendNewline

cbNewlineHalf :: CallbackEnv -> IO ()
cbNewlineHalf env = getCurrentBalloon env >>= appendNewlineHalf

cbNewlinePercent :: CallbackEnv -> Int -> IO ()
cbNewlinePercent env pct = getCurrentBalloon env >>= \b -> appendNewlinePercent b pct

cbClear :: CallbackEnv -> IO ()
cbClear env = getCurrentBalloon env >>= clearBalloon

cbClearChars :: CallbackEnv -> Int -> IO ()
cbClearChars env n = getCurrentBalloon env >>= \b -> clearChars b n

-- -----------------------------------------------------------------------------
-- Scope/Surface callbacks
-- -----------------------------------------------------------------------------

cbSetScope :: CallbackEnv -> Int -> IO ()
cbSetScope env scope = do
  writeIORef (ceCurrentScopeRef env) scope
  putStrLn $ "[Scope] Switched to scope " <> show scope

cbSetSurface :: CallbackEnv -> Int -> Int -> IO ()
cbSetSurface = ceChangeSurface

cbHideCharacter :: CallbackEnv -> Int -> IO ()
cbHideCharacter env scope = do
  putStrLn $ "[Character] Hiding scope " <> show scope
  case Map.lookup scope (ceCharacters env) of
    Just cs -> hideCharacter cs
    Nothing -> putStrLn $ "[Character] Scope " <> show scope <> " not found"

-- -----------------------------------------------------------------------------
-- Balloon callbacks
-- -----------------------------------------------------------------------------

cbSetBalloon :: CallbackEnv -> Int -> Int -> IO ()
cbSetBalloon env scope balloonId = do
  putStrLn $ "[Balloon] Set balloon " <> show balloonId <> " for scope " <> show scope
  case Map.lookup scope (ceCharacters env) of
    Just cs -> setBalloonId (getCharacterBalloon cs) balloonId
    Nothing -> putStrLn $ "[Balloon] Scope " <> show scope <> " not found"

cbHideBalloon :: CallbackEnv -> Int -> IO ()
cbHideBalloon env scope =
  case Map.lookup scope (ceCharacters env) of
    Just cs -> hideBalloon (getCharacterBalloon cs)
    Nothing -> return ()

cbShowBalloon :: CallbackEnv -> Int -> IO ()
cbShowBalloon env scope = do
  -- Cancel any pending hide timer when showing balloon
  ceCancelBalloonHideTimer env
  case Map.lookup scope (ceCharacters env) of
    Just cs -> showBalloon (getCharacterBalloon cs)
    Nothing -> return ()

cbMoveCursor :: CallbackEnv -> Int -> Int -> IO ()
cbMoveCursor env x y = do
  b <- getCurrentBalloon env
  moveCursor b x y

cbAddChoice :: CallbackEnv -> T.Text -> T.Text -> T.Text -> IO ()
cbAddChoice env choiceId text action = do
  b <- getCurrentBalloon env
  addChoice b (BalloonChoice text choiceId action)

cbClearChoices :: CallbackEnv -> IO ()
cbClearChoices env = getCurrentBalloon env >>= clearChoices

-- -----------------------------------------------------------------------------
-- Animation callbacks
-- -----------------------------------------------------------------------------

cbAnimStart :: CallbackEnv -> Int -> Int -> IO ()
cbAnimStart _env scope animId' = do
  putStrLn $ "[Anim] Start animation " <> show animId' <> " on scope " <> show scope
  -- Animation start requires surface definition lookup which is done at Character level

cbAnimStop :: CallbackEnv -> Int -> Int -> IO ()
cbAnimStop env scope animId' = do
  putStrLn $ "[Anim] Stop animation " <> show animId' <> " on scope " <> show scope
  case Map.lookup scope (ceCharacters env) of
    Just cs -> do
      let animState = csAnimState cs
      activeAnims <- readIORef (asActiveAnims animState)
      let newAnims = stopAnimation activeAnims animId'
      writeIORef (asActiveAnims animState) newAnims
    Nothing -> return ()

cbAnimWait :: CallbackEnv -> Int -> Int -> IO ()
cbAnimWait env scope animId' = do
  putStrLn $ "[Anim] Wait for animation " <> show animId' <> " on scope " <> show scope
  case Map.lookup scope (ceCharacters env) of
    Just cs -> do
      let animState = csAnimState cs
          waitLoop  = do
            activeAnims <- readIORef (asActiveAnims animState)
            let isRunning = any (\a -> animId' == animId (aaDef a)) activeAnims
            when isRunning $ do
              GLib.usleep 50000  -- Wait 50ms
              waitLoop
      waitLoop
    Nothing -> return ()

cbAnimClear :: CallbackEnv -> Int -> Maybe Int -> IO ()
cbAnimClear env scope mAnimId = do
  putStrLn $ "[Anim] Clear animations on scope " <> show scope
  case Map.lookup scope (ceCharacters env) of
    Just cs -> do
      let animState = csAnimState cs
      case mAnimId of
        Just aid -> do
          activeAnims <- readIORef (asActiveAnims animState)
          let newAnims = stopAnimation activeAnims aid
          writeIORef (asActiveAnims animState) newAnims
        Nothing  -> clearAnimations animState
    Nothing -> return ()

cbBindToggle :: CallbackEnv -> Int -> T.Text -> T.Text -> Bool -> IO ()
cbBindToggle env scope category part enabled = do
  putStrLn
    $ "[Bind] Toggle "
    <> T.unpack category
    <> "/"
    <> T.unpack part
    <> " = "
    <> show enabled
    <> " on scope "
    <> show scope
  case Map.lookup scope (ceCharacters env) of
    Just cs -> do
      let animState = csAnimState cs
      -- Find animation ID by category/part name (simplified: use hash)
      let animId' = abs $ T.length category * 1000 + T.length part
      if enabled
        then do
          binds <- getEnabledBinds animState
          unless (Set.member animId' binds) $ toggleBind animState animId'
        else do
          binds <- getEnabledBinds animState
          when (Set.member animId' binds) $ toggleBind animState animId'
    Nothing -> return ()

-- -----------------------------------------------------------------------------
-- Move callback
-- -----------------------------------------------------------------------------

cbMove :: CallbackEnv -> Int -> Int -> Int -> Maybe Int -> Bool -> IO ()
cbMove env scope x y mTime async = do
  putStrLn
    $ "[Move] Move scope "
    <> show scope
    <> " to ("
    <> show x
    <> ", "
    <> show y
    <> ")"
    <> maybe "" (\t -> " in " <> show t <> "ms") mTime
    <> if async then " (async)" else ""
  case Map.lookup scope (ceCharacters env) of
    Just cs -> setCharacterPosition cs (fromIntegral x) (fromIntegral y)
    Nothing -> return ()

-- -----------------------------------------------------------------------------
-- Font callback
-- -----------------------------------------------------------------------------

cbSetFont :: CallbackEnv -> FontCmd -> IO ()
cbSetFont env fontCmd = do
  b <- getCurrentBalloon env
  case fontCmd of
    FontName name        -> setFontName b name
    FontHeight fontSize  -> case fontSize of
      FontSizeAbsolute size  -> setFontSize b size
      FontSizeRelative delta -> do
        currentSize <- getFontSize b
        setFontSize b (max 1 (currentSize + delta))
      FontSizePercent pct    -> do
        let defaultSize = 12
        setFontSize b (max 1 ((defaultSize * pct) `div` 100))
      FontSizeDefault        -> setFontSize b 12
    FontColor color      -> case color of
      ColorRGB r g b'        -> setFontColor b (fromIntegral r / 255.0) (fromIntegral g / 255.0) (fromIntegral b' / 255.0)
      ColorRGBPercent r g b' -> setFontColor b r g b'
      ColorHex hexStr        -> case parseHexColor hexStr of
        Just (r, g, b') -> setFontColor b r g b'
        Nothing         -> return ()
      ColorName name         -> case lookupColorName name of
        Just (r, g, b') -> setFontColor b r g b'
        Nothing         -> return ()
      ColorDefault           -> resetFont b
      ColorDisable           -> return ()
    FontBold toggle      -> applyToggle (setFontBold b) toggle
    FontItalic toggle    -> applyToggle (setFontItalic b) toggle
    FontUnderline toggle -> applyToggle (setFontUnderline b) toggle
    FontStrike toggle    -> applyToggle (setFontStrike b) toggle
    FontDefault          -> resetFont b
    FontSub toggle       -> applyToggle (setFontSub b) toggle
    FontSup toggle       -> applyToggle (setFontSup b) toggle
    FontAlign _          -> return ()
    FontVAlign _         -> return ()
    FontShadowColor _    -> return ()
    FontDisable _        -> return ()
    FontCursor _         -> return ()
    FontAnchorNormal _   -> return ()
    FontAnchorHover _    -> return ()
    FontChoiceNormal _   -> return ()
    FontChoiceHover _    -> return ()
  where
    applyToggle :: (Bool -> IO ()) -> FontToggle -> IO ()
    applyToggle f toggle = case toggle of
      ToggleOn      -> f True
      ToggleOff     -> f False
      ToggleDefault -> f False
      ToggleDisable -> return ()

-- -----------------------------------------------------------------------------
-- Sound callbacks
-- -----------------------------------------------------------------------------

cbPlaySound :: CallbackEnv -> T.Text -> IO ()
cbPlaySound env = playSound (ceSoundState env)

cbStopSound :: CallbackEnv -> IO ()
cbStopSound env = stopSound (ceSoundState env)

cbSoundAction :: CallbackEnv -> SoundAction -> T.Text -> IO ()
cbSoundAction env action file = case action of
  SoundActionPlay      -> playSound (ceSoundState env) file
  SoundActionLoop      -> playSound (ceSoundState env) file
  SoundActionPause     -> stopSound (ceSoundState env)
  SoundActionResume    -> playSound (ceSoundState env) file
  SoundActionSoundStop -> stopSound (ceSoundState env)
  SoundActionWait      -> return ()

-- -----------------------------------------------------------------------------
-- Event callbacks
-- -----------------------------------------------------------------------------

cbRaiseEvent :: T.Text -> [T.Text] -> IO ()
cbRaiseEvent eventName refs = do
  putStrLn $ "[Event] Raise: " <> T.unpack eventName <> " with refs: " <> show refs

cbNotify :: T.Text -> [T.Text] -> IO ()
cbNotify name refs = do
  putStrLn $ "[Event] Notify: " <> T.unpack name <> " with refs: " <> show refs

cbTimerRaise :: T.Text -> Int -> IO ()
cbTimerRaise name delayMs = do
  putStrLn $ "[Event] Timer raise: " <> T.unpack name <> " in " <> show delayMs <> "ms"
  void $ GLib.timeoutAdd GLib.PRIORITY_DEFAULT (fromIntegral delayMs) $ do
    putStrLn $ "[Event] Timer fired: " <> T.unpack name
    return False

cbTimerCancel :: T.Text -> IO ()
cbTimerCancel name = do
  putStrLn $ "[Event] Timer cancel: " <> T.unpack name

cbGhostChange :: T.Text -> IO ()
cbGhostChange ghostName = do
  putStrLn $ "[Event] Ghost change: " <> T.unpack ghostName

cbShellChange :: T.Text -> IO ()
cbShellChange shellName = do
  putStrLn $ "[Event] Shell change: " <> T.unpack shellName

cbBalloonStyleChange :: T.Text -> IO ()
cbBalloonStyleChange balloonName = do
  putStrLn $ "[Event] Balloon style change: " <> T.unpack balloonName

-- -----------------------------------------------------------------------------
-- Open callbacks
-- -----------------------------------------------------------------------------

cbOpenURL :: T.Text -> IO ()
cbOpenURL url = do
  putStrLn $ "[Open] URL: " <> T.unpack url
  void $ Gio.appInfoLaunchDefaultForUri url (Nothing :: Maybe Gio.AppLaunchContext)

cbOpenFile :: T.Text -> IO ()
cbOpenFile file = do
  putStrLn $ "[Open] File: " <> T.unpack file
  let uri = "file://" <> file
  void $ Gio.appInfoLaunchDefaultForUri uri (Nothing :: Maybe Gio.AppLaunchContext)

cbOpenInputBox :: T.Text -> [InputBoxOpt] -> IO ()
cbOpenInputBox eventId _opts = do
  putStrLn $ "[Open] Input box for event: " <> T.unpack eventId
  dialog <- Gtk.new Gtk.Window
    [ #title := "Input", #modal := True, #defaultWidth := 300, #defaultHeight := 100 ]
  box <- Gtk.new Gtk.Box
    [ #orientation := Gtk.OrientationVertical, #spacing := 10
    , #marginTop := 10, #marginBottom := 10, #marginStart := 10, #marginEnd := 10 ]
  entry <- Gtk.new Gtk.Entry [ #placeholderText := "Enter text..." ]
  buttonBox <- Gtk.new Gtk.Box
    [ #orientation := Gtk.OrientationHorizontal, #spacing := 10, #halign := Gtk.AlignEnd ]
  okBtn <- Gtk.new Gtk.Button [ #label := "OK" ]
  cancelBtn <- Gtk.new Gtk.Button [ #label := "Cancel" ]

  Gtk.boxAppend buttonBox okBtn
  Gtk.boxAppend buttonBox cancelBtn
  Gtk.boxAppend box entry
  Gtk.boxAppend box buttonBox
  Gtk.windowSetChild dialog (Just box)

  void $ Gtk.on okBtn #clicked $ do
    inputText <- Gtk.editableGetText entry
    putStrLn $ "[Open] Input result: " <> T.unpack inputText
    Gtk.windowClose dialog

  void $ Gtk.on cancelBtn #clicked $ do
    putStrLn "[Open] Input cancelled"
    Gtk.windowClose dialog

  Gtk.windowPresent dialog

cbOpenDialog :: T.Text -> DialogOpt -> IO ()
cbOpenDialog msg _opt = do
  putStrLn $ "[Open] Dialog: " <> T.unpack msg
  dialog <- Gtk.new Gtk.Window
    [ #title := "Message", #modal := True, #defaultWidth := 300, #defaultHeight := 150 ]
  box <- Gtk.new Gtk.Box
    [ #orientation := Gtk.OrientationVertical, #spacing := 10
    , #marginTop := 20, #marginBottom := 10, #marginStart := 20, #marginEnd := 20 ]
  label <- Gtk.new Gtk.Label [ #label := msg, #wrap := True ]
  okBtn <- Gtk.new Gtk.Button [ #label := "OK", #halign := Gtk.AlignCenter ]

  Gtk.boxAppend box label
  Gtk.boxAppend box okBtn
  Gtk.windowSetChild dialog (Just box)

  void $ Gtk.on okBtn #clicked $ Gtk.windowClose dialog

  Gtk.windowPresent dialog

-- -----------------------------------------------------------------------------
-- Meta callbacks
-- -----------------------------------------------------------------------------

cbSetProperty :: SetProperty -> IO ()
cbSetProperty prop = do
  putStrLn $ "[Meta] Set property: " <> show prop

cbGetProperty :: GetProperty -> IO T.Text
cbGetProperty prop = do
  putStrLn $ "[Meta] Get property: " <> show prop
  return ""

cbReload :: ReloadTarget -> IO ()
cbReload target = do
  putStrLn $ "[Meta] Reload: " <> show target

cbExecute :: ExecuteCmd -> IO ()
cbExecute execCmd = do
  putStrLn $ "[Meta] Execute: " <> show execCmd

cbSetPassiveMode :: Bool -> IO ()
cbSetPassiveMode enabled = do
  putStrLn $ "[Meta] Passive mode: " <> show enabled

cbLock :: T.Text -> IO ()
cbLock component = do
  putStrLn $ "[Meta] Lock: " <> T.unpack component

cbUnlock :: T.Text -> IO ()
cbUnlock component = do
  putStrLn $ "[Meta] Unlock: " <> T.unpack component

-- -----------------------------------------------------------------------------
-- EnvVar callback
-- -----------------------------------------------------------------------------

cbGetEnvVar :: CallbackEnv -> EnvVar -> IO T.Text
cbGetEnvVar env envVar = do
  now <- getCurrentTime
  let (year, month, day)         = toGregorian (utctDay now)
      TimeOfDay hour minute sec  = timeToTimeOfDay (utctDayTime now)
  case envVar of
    EnvYear         -> return $ T.pack $ show year
    EnvMonth        -> return $ T.pack $ show month
    EnvDay          -> return $ T.pack $ show day
    EnvHour         -> return $ T.pack $ show hour
    EnvMinute       -> return $ T.pack $ show minute
    EnvSecond       -> return $ T.pack $ show (truncate sec :: Int)
    EnvWeekday      -> do
      let (_, _, dow) = toWeekDate (utctDay now)
          weekdays = ["日", "月", "火", "水", "木", "金", "土"]
      return $ T.pack $ fromMaybe "?" $ weekdays !!? (dow `mod` 7)
    EnvSelfname     -> return "Emily"
    EnvSelfname2    -> return ""
    EnvKeroname     -> return ""
    EnvGhostname    -> return "Kokage Ghost"
    EnvShellname    -> return "master"
    EnvUsername     -> do
      mUser <- lookupEnv "USER"
      return $ T.pack $ fromMaybe "User" mUser
    EnvOS           -> return "Linux"
    EnvScreenWidth  -> getScreenDimension True
    EnvScreenHeight -> getScreenDimension False
    EnvSurface      -> do
      scope <- readIORef (ceCurrentScopeRef env)
      getCharacterSurfaceId env scope
    EnvSurface0     -> getCharacterSurfaceId env 0
    EnvSurface1     -> getCharacterSurfaceId env 1
    EnvCustom name  -> do
      putStrLn $ "[EnvVar] Custom: " <> T.unpack name
      return ""

getScreenDimension :: Bool -> IO T.Text
getScreenDimension isWidth = do
  mDisplay <- Gdk.displayGetDefault
  case mDisplay of
    Nothing      -> return $ if isWidth then "1920" else "1080"
    Just display -> do
      monitors <- Gdk.displayGetMonitors display
      n <- Gio.listModelGetNItems monitors
      if n > 0
        then do
          mMonitor <- Gio.listModelGetItem monitors 0
          case mMonitor of
            Nothing  -> return $ if isWidth then "1920" else "1080"
            Just obj -> do
              monitor <- Gdk.unsafeCastTo Gdk.Monitor obj
              geom <- Gdk.monitorGetGeometry monitor
              val <- if isWidth
                then Gdk.getRectangleWidth geom
                else Gdk.getRectangleHeight geom
              return $ T.pack $ show val
        else return $ if isWidth then "1920" else "1080"

getCharacterSurfaceId :: CallbackEnv -> Int -> IO T.Text
getCharacterSurfaceId env scope =
  case Map.lookup scope (ceCharacters env) of
    Just cs -> do
      surfId <- readIORef (csCurrentSurface cs)
      return $ T.pack $ show surfId
    Nothing -> return "0"

-- -----------------------------------------------------------------------------
-- Completion callbacks
-- -----------------------------------------------------------------------------

cbOnComplete :: CallbackEnv -> IO ()
cbOnComplete env = do
  ceFireTimeCritical env False
  putStrLn "[Script] Execution complete"
  ceHideBalloonIfNoChoices env

cbOnInterrupt :: CallbackEnv -> IO ()
cbOnInterrupt env = do
  ceFireTimeCritical env False
  putStrLn "[Script] Execution interrupted"
  ceHideBalloonIfNoChoices env

cbOnClickWait :: IO ()
cbOnClickWait = do
  putStrLn "[Script] Click wait triggered"

cbSetTimeCritical :: CallbackEnv -> Bool -> IO ()
cbSetTimeCritical env enabled = do
  ceFireTimeCritical env enabled
  putStrLn $ "[Script] Time-critical mode: " <> show enabled

-- -----------------------------------------------------------------------------
-- Color helpers
-- -----------------------------------------------------------------------------

-- | Parse a hex color string like "#FF0000" or "FF0000" to RGB values (0.0-1.0)
parseHexColor :: T.Text -> Maybe (Double, Double, Double)
parseHexColor hexStr = do
  let hex = T.dropWhile (== '#') hexStr
  case T.length hex of
    6 -> do
      r <- parseHexByte (T.take 2 hex)
      g <- parseHexByte (T.take 2 (T.drop 2 hex))
      b <- parseHexByte (T.drop 4 hex)
      Just (fromIntegral r / 255.0, fromIntegral g / 255.0, fromIntegral b / 255.0)
    3 -> do
      r <- parseHexNibble (T.take 1 hex)
      g <- parseHexNibble (T.take 1 (T.drop 1 hex))
      b <- parseHexNibble (T.drop 2 hex)
      Just (fromIntegral (r * 17) / 255.0, fromIntegral (g * 17) / 255.0, fromIntegral (b * 17) / 255.0)
    _ -> Nothing
  where
    parseHexByte :: T.Text -> Maybe Int
    parseHexByte t = case reads ("0x" ++ T.unpack t) of
      [(n, "")] -> Just n
      _         -> Nothing
    parseHexNibble :: T.Text -> Maybe Int
    parseHexNibble t = case reads ("0x" ++ T.unpack t) of
      [(n, "")] -> Just n
      _         -> Nothing

-- | Look up a named color to RGB values (0.0-1.0)
lookupColorName :: T.Text -> Maybe (Double, Double, Double)
lookupColorName name = Map.lookup (T.toLower name) colorNames
  where
    colorNames :: Map.Map T.Text (Double, Double, Double)
    colorNames = Map.fromList
      [ ("black", (0.0, 0.0, 0.0)), ("white", (1.0, 1.0, 1.0))
      , ("red", (1.0, 0.0, 0.0)), ("green", (0.0, 0.5, 0.0))
      , ("blue", (0.0, 0.0, 1.0)), ("yellow", (1.0, 1.0, 0.0))
      , ("cyan", (0.0, 1.0, 1.0)), ("magenta", (1.0, 0.0, 1.0))
      , ("gray", (0.5, 0.5, 0.5)), ("grey", (0.5, 0.5, 0.5))
      , ("orange", (1.0, 0.65, 0.0)), ("pink", (1.0, 0.75, 0.8))
      , ("purple", (0.5, 0.0, 0.5)), ("brown", (0.65, 0.16, 0.16))
      , ("navy", (0.0, 0.0, 0.5)), ("teal", (0.0, 0.5, 0.5))
      , ("olive", (0.5, 0.5, 0.0)), ("maroon", (0.5, 0.0, 0.0))
      , ("silver", (0.75, 0.75, 0.75)), ("lime", (0.0, 1.0, 0.0))
      , ("aqua", (0.0, 1.0, 1.0)), ("fuchsia", (1.0, 0.0, 1.0))
      ]

