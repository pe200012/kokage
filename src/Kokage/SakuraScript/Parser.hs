{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Sakura Script parser using Megaparsec
-- Reference: https://ssp.shillest.net/ukadoc/manual/list_sakura_script.html
module Kokage.SakuraScript.Parser
  ( -- * Parsing
    parseScript
  , parseSakuraScript
    -- * Parser type
  , Parser
  , SSParseError
    -- * Individual parsers (for testing)
  , pScript
  , pSakuraScript
  , pScopeCmd
  , pSurfaceCmd
  , pBalloonCmd
  , pWaitCmd
  , pChoiceCmd
  , pFontCmd
  , pEventCmd
  , pSoundCmd
  , pOpenCmd
  , pMetaCmd
  , pEnvVar
  ) where

import           Control.Monad              ( void )

import           Data.Text                  ( Text )
import qualified Data.Text                  as T
import           Data.Void                  ( Void )

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Types.SakuraScript

--------------------------------------------------------------------------------
-- Parser type
--------------------------------------------------------------------------------

-- | Parser type alias
type Parser = Parsec Void Text

-- | Parse error type alias
type SSParseError = ParseErrorBundle Text Void

--------------------------------------------------------------------------------
-- Top-level parsing functions
--------------------------------------------------------------------------------

-- | Parse a complete Sakura Script
parseScript :: Text -> Either SSParseError Script
parseScript = parse (pScript <* eof) "<script>"

-- | Parse a single Sakura Script element
parseSakuraScript :: Text -> Either SSParseError SakuraScript
parseSakuraScript = parse (pSakuraScript <* eof) "<script>"

--------------------------------------------------------------------------------
-- Main parsers
--------------------------------------------------------------------------------

-- | Parse a complete script (sequence of elements)
pScript :: Parser Script
pScript = many pSakuraScript

-- | Parse a single script element
pSakuraScript :: Parser SakuraScript
pSakuraScript = choice [ pEnvVarElement, pCommand, pText ]

-- | Parse plain text (until we hit a command or env var)
pText :: Parser SakuraScript
pText = SSText . T.pack <$> some (satisfy isPlainChar)
  where
    isPlainChar c = c /= '\\' && c /= '%'

-- | Parse environment variable element
pEnvVarElement :: Parser SakuraScript
pEnvVarElement = SSEnvVar <$> pEnvVar

-- | Parse any command (starts with \)
pCommand :: Parser SakuraScript
pCommand
  = char '\\'
  *> choice
    [ SSSurface <$> try pSurfaceCmd
    , SSScope <$> try pScopeCmd
    , SSBalloon <$> try pBalloonCmd
    , SSWait <$> try pWaitCmd
    , SSChoice <$> try pChoiceCmd
    , SSFont <$> try pFontCmd
    , SSEvent <$> try pEventCmd
    , SSSound <$> try pSoundCmd
    , SSOpen <$> try pOpenCmd
    , SSMeta <$> try pMetaCmd
      -- Escape sequences must come last (fallback for \\, \%)
    , SSEscape <$> oneOf ("\\%" :: String)
    ]

--------------------------------------------------------------------------------
-- Scope commands
--------------------------------------------------------------------------------

-- | Parse scope commands: \0, \1, \h, \u, \p[n]
pScopeCmd :: Parser ScopeCmd
pScopeCmd
  = choice
    [ ScopeMain <$ (char '0' <|> char 'h')
    , ScopeKero <$ (char '1' <|> char 'u')
    , ScopeIndex <$> (char 'p' *> pBracketedInt)
    ]

--------------------------------------------------------------------------------
-- Surface commands
--------------------------------------------------------------------------------

-- | Parse surface commands
pSurfaceCmd :: Parser SurfaceCmd
pSurfaceCmd = choice [ pSurfaceChange, pSurfaceAnim, pSurfaceBang ]

-- | Parse surface change: \s[n] or \s[alias]
pSurfaceChange :: Parser SurfaceCmd
pSurfaceChange
  = char 's' *> pBracketed (try (SurfaceChange <$> pInt) <|> (SurfaceChangeAlias <$> pIdentifier))

-- | Parse animation command: \i[n] or \i[n,action]
pSurfaceAnim :: Parser SurfaceCmd
pSurfaceAnim = char 'i' *> pBracketed pAnimCmd
  where
    pAnimCmd = do
      n <- pInt
      action <- option AnimStart (pComma *> pAnimAction)
      pure $ SurfaceAnim n action

-- | Parse animation action
pAnimAction :: Parser AnimAction
pAnimAction
  = choice
    [ AnimStart <$ string "start"
    , AnimStop <$ string "stop"
    , AnimPause <$ string "pause"
    , AnimResume <$ string "resume"
    ]

-- | Parse surface bang commands: \![surface,...]
pSurfaceBang :: Parser SurfaceCmd
pSurfaceBang
  = string "!["
  *> string "surface"
  *> pComma
  *> choice
    [ SurfaceLockRepaint True <$ string "lock,repaint"
    , SurfaceLockRepaint False <$ string "unlock,repaint"
    , pSurfaceAlignment
    , pSurfaceAlpha
    ]
  <* char ']'

-- | Parse surface alignment
pSurfaceAlignment :: Parser SurfaceCmd
pSurfaceAlignment = string "set,align" *> pComma *> do
  target <- pAlignTarget
  _ <- pComma
  align <- pDesktopAlign
  pure $ SurfaceAlignment target align

pAlignTarget :: Parser AlignTarget
pAlignTarget = choice [ AlignDesktop <$ string "desktop", AlignOwner <$ string "owner" ]

pDesktopAlign :: Parser DesktopAlign
pDesktopAlign
  = choice
    [ AlignLeft <$ string "left"
    , AlignRight <$ string "right"
    , AlignTop <$ string "top"
    , AlignBottom <$ string "bottom"
    , AlignCenter <$ string "center"
    , AlignFree <$ string "free"
    ]

-- | Parse surface alpha
pSurfaceAlpha :: Parser SurfaceCmd
pSurfaceAlpha = string "set,alpha" *> pComma *> (SurfaceAlpha <$> pInt)

--------------------------------------------------------------------------------
-- Balloon commands
--------------------------------------------------------------------------------

-- | Parse balloon commands
pBalloonCmd :: Parser BalloonCmd
pBalloonCmd
  = choice
    [ pBalloonChange
    , pNewline
    , Clear <$ char 'c'
    , pClearChars
    , pCursorMove
    , pBalloonHideOrImage
    , pBalloonBang
    ]

-- | Parse balloon change: \b[n]
pBalloonChange :: Parser BalloonCmd
pBalloonChange = char 'b' *> pBracketed (BalloonChange <$> pInt)

-- | Parse newline variants: \n, \n[half], \n[percent,n]
pNewline :: Parser BalloonCmd
pNewline
  = char 'n'
  *> option
    Newline
    (pBracketed
     $ choice
       [ NewlineHalf <$ string "half", NewlinePercent <$> (string "percent" *> pComma *> pInt) ])

-- | Parse clear chars: \_c[n]
pClearChars :: Parser BalloonCmd
pClearChars = string "_c" *> pBracketed (ClearChars <$> pInt)

-- | Parse cursor move: \_l[x,y]
pCursorMove :: Parser BalloonCmd
pCursorMove = string "_l" *> pBracketed pCursorMoveArgs
  where
    pCursorMoveArgs = do
      x <- pCursorPos
      _ <- pComma
      y <- pCursorPos
      pure $ CursorMove x y

-- | Parse balloon hide or image: \_b or \_b[...]
pBalloonHideOrImage :: Parser BalloonCmd
pBalloonHideOrImage
  = string "_b" *> option BalloonHide (pBracketed (BalloonImage <$> pBalloonImageSpec))

-- | Parse balloon image spec
pBalloonImageSpec :: Parser BalloonImageSpec
pBalloonImageSpec = do
  file <- pQuotedString <|> pIdentifier
  opts <- many (pComma *> pImageOpt)
  pure $ foldr applyOpt defaultSpec { biFile = file } opts
  where
    defaultSpec = BalloonImageSpec "" Nothing Nothing False False False Nothing

    pImageOpt
      = choice
        [ ( "x",  ) <$> (string "x=" *> pInt)
        , ( "y",  ) <$> (string "y=" *> pInt)
        , ( "inline", 1 ) <$ string "inline"
        , ( "opaque", 1 ) <$ string "opaque"
        , ( "useSelfAlpha", 1 ) <$ string "useSelfAlpha"
        ]

    applyOpt :: ( String, Int ) -> BalloonImageSpec -> BalloonImageSpec
    applyOpt ( "x", v ) s = s { biX = Just v }
    applyOpt ( "y", v ) s = s { biY = Just v }
    applyOpt ( "inline", _ ) s = s { biInline = True }
    applyOpt ( "opaque", _ ) s = s { biOpaque = True }
    applyOpt ( "useSelfAlpha", _ ) s = s { biUseSelfAlpha = True }
    applyOpt _ s = s

-- | Parse balloon bang commands
pBalloonBang :: Parser BalloonCmd
pBalloonBang
  = string "!["
  *> string "balloon"
  *> pComma
  *> choice
    [ BalloonShow <$ string "show", BalloonHide <$ string "hide", pBalloonOffset, pBalloonTimeout ]
  <* char ']'

pBalloonOffset :: Parser BalloonCmd
pBalloonOffset = string "set,offset" *> pComma *> do
  x <- pInt
  _ <- pComma
  y <- pInt
  pure $ BalloonOffset x y

pBalloonTimeout :: Parser BalloonCmd
pBalloonTimeout = string "set,timeout" *> pComma *> (BalloonTimeout . Just <$> pInt)

--------------------------------------------------------------------------------
-- Wait commands
--------------------------------------------------------------------------------

-- | Parse wait commands
pWaitCmd :: Parser WaitCmd
pWaitCmd
  = choice
    [ pWaitSimple
    , pWaitMs
    , pWaitUntil
    , ClickWait <$ char 'x'
    , ClickWaitNoClear <$ string "_q"
    , TimeCriticalStart <$ char 't'
    , pWaitBang
    ]

-- | Parse simple wait: \wN (single digit)
pWaitSimple :: Parser WaitCmd
pWaitSimple = char 'w' *> (WaitSimple . read . pure <$> digitChar)

-- | Parse ms wait: \_w[n]
pWaitMs :: Parser WaitCmd
pWaitMs = string "_w" *> pBracketed (WaitMs <$> pInt)

-- | Parse until wait: \__w[n]
pWaitUntil :: Parser WaitCmd
pWaitUntil = string "__w" *> pBracketed (WaitUntil <$> pInt)

-- | Parse wait bang commands
pWaitBang :: Parser WaitCmd
pWaitBang
  = string "!["
  *> string "wait"
  *> pComma
  *> choice [ WaitAnimComplete <$> (string "animation" *> pComma *> pInt) ]
  <* char ']'

--------------------------------------------------------------------------------
-- Choice commands
--------------------------------------------------------------------------------

-- | Parse choice commands
pChoiceCmd :: Parser ChoiceCmd
pChoiceCmd = choice [ pChoiceBasic, pChoiceScript, pChoiceNoTimeout, pAnchor ]

-- | Parse basic choice: \q[text,action]
pChoiceBasic :: Parser ChoiceCmd
pChoiceBasic = char 'q' *> pBracketed pChoiceArgs
  where
    pChoiceArgs = do
      text <- pChoiceText
      _ <- pComma
      action <- pChoiceAction
      pure $ Choice text action

-- | Parse script choice: \__q[text,script]
pChoiceScript :: Parser ChoiceCmd
pChoiceScript = string "__q" *> pBracketed pChoiceScriptArgs
  where
    pChoiceScriptArgs = do
      text <- pChoiceText
      _ <- pComma
      script <- pChoiceText
      pure $ ChoiceScript text script

-- | Parse no-timeout choice: \_q[text,action]
pChoiceNoTimeout :: Parser ChoiceCmd
pChoiceNoTimeout = string "_q" *> pBracketed pChoiceArgs
  where
    pChoiceArgs = do
      text <- pChoiceText
      _ <- pComma
      action <- pChoiceAction
      pure $ ChoiceNoTimeout text action

-- | Parse anchor: \_a[id,text]
pAnchor :: Parser ChoiceCmd
pAnchor = string "_a" *> pBracketed pAnchorArgs
  where
    pAnchorArgs = do
      anchorId <- pIdentifier
      _ <- pComma
      text <- pChoiceText
      pure $ Anchor anchorId text

-- | Parse choice text (up to comma or bracket)
pChoiceText :: Parser Text
pChoiceText = T.pack <$> many (satisfy (\c -> c /= ',' && c /= ']'))

-- | Parse choice action
pChoiceAction :: Parser ChoiceAction
pChoiceAction
  = choice
    [ try $ ChoiceURL <$> (string "http" *> (T.pack . ("http" ++) <$> many (satisfy (/= ']'))))
    , ChoiceEvent <$> pIdentifier
    ]

--------------------------------------------------------------------------------
-- Font commands
--------------------------------------------------------------------------------

-- | Parse font commands
pFontCmd :: Parser FontCmd
pFontCmd = char 'f' *> pBracketed pFontArg

-- | Parse font argument
pFontArg :: Parser FontCmd
pFontArg
  = choice
    [ pFontAlign
    , pFontName
    , pFontHeight
    , pFontColor
    , pFontBold
    , pFontItalic
    , pFontStrike
    , pFontUnderline
    , pFontDefault
    ]

pFontAlign :: Parser FontCmd
pFontAlign
  = string "align"
  *> pComma
  *> choice
    [ FontAlign HAlignLeft <$ string "left"
    , FontAlign HAlignCenter <$ string "center"
    , FontAlign HAlignRight <$ string "right"
    , FontAlign HAlignDefault <$ string "default"
    ]

pFontName :: Parser FontCmd
pFontName = string "name" *> pComma *> (FontName <$> pQuotedStringOrId)

pFontHeight :: Parser FontCmd
pFontHeight = string "height" *> pComma *> (FontHeight <$> pFontSize)

pFontSize :: Parser FontSize
pFontSize
  = choice
    [ FontSizeDefault <$ string "default"
    , try pFontSizeRelative
    , try pFontSizePercent
    , FontSizeAbsolute <$> pInt
    ]
  where
    pFontSizeRelative = do
      sign <- (id <$ char '+') <|> (negate <$ char '-')
      n <- pInt
      pure $ FontSizeRelative (sign n)

    pFontSizePercent  = do
      n <- pInt
      _ <- char '%'
      pure $ FontSizePercent n

pFontColor :: Parser FontCmd
pFontColor = string "color" *> pComma *> (FontColor <$> pColor)

pFontBold :: Parser FontCmd
pFontBold = string "bold" *> pComma *> (FontBold <$> pFontToggle)

pFontItalic :: Parser FontCmd
pFontItalic = string "italic" *> pComma *> (FontItalic <$> pFontToggle)

pFontStrike :: Parser FontCmd
pFontStrike = string "strike" *> pComma *> (FontStrike <$> pFontToggle)

pFontUnderline :: Parser FontCmd
pFontUnderline = string "underline" *> pComma *> (FontUnderline <$> pFontToggle)

pFontDefault :: Parser FontCmd
pFontDefault = FontDefault <$ string "default"

pFontToggle :: Parser FontToggle
pFontToggle
  = choice
    [ ToggleOn <$ (string "true" <|> string "on")
    , ToggleOff <$ (string "false" <|> string "off")
    , ToggleDefault <$ string "default"
    ]

--------------------------------------------------------------------------------
-- Event commands
--------------------------------------------------------------------------------

-- | Parse event commands
pEventCmd :: Parser EventCmd
pEventCmd = choice [ EventExit <$ char 'e', pEventChain, pEventBang ]

-- | Parse event chain: \-
pEventChain :: Parser EventCmd
pEventChain = char '-' *> pure (EventScript "" "")

-- | Parse event bang commands
pEventBang :: Parser EventCmd
pEventBang
  = string "![" *> choice [ pRaise, pEmbed, pNotify, pTimerRaise, pVanish, pUpdate ] <* char ']'

pRaise :: Parser EventCmd
pRaise = string "raise" *> pComma *> do
  eventId <- pIdentifier
  refs <- many (pComma *> pIdentifier)
  pure $ EventRaise eventId refs

pEmbed :: Parser EventCmd
pEmbed = string "embed" *> pComma *> (EventEmbed <$> pQuotedStringOrId)

pNotify :: Parser EventCmd
pNotify = string "notify" *> pComma *> do
  eventId <- pIdentifier
  refs <- many (pComma *> pIdentifier)
  pure $ EventNotify eventId refs []

pTimerRaise :: Parser EventCmd
pTimerRaise = string "timerraise" *> pComma *> do
  eventId <- pIdentifier
  _ <- pComma
  interval <- pInt
  pure $ EventTimerRaise eventId interval []

pVanish :: Parser EventCmd
pVanish = EventVanish <$ string "vanish"

pUpdate :: Parser EventCmd
pUpdate
  = string "update"
  *> pComma
  *> choice
    [ EventUpdate UpdateCheck <$ string "check"
    , EventUpdate UpdateCheckAll <$ string "checkall"
    , EventUpdate UpdateGhost <$ string "ghost"
    , EventUpdate UpdateShell <$ string "shell"
    , EventUpdate UpdateBalloon <$ string "balloon"
    ]

--------------------------------------------------------------------------------
-- Sound commands
--------------------------------------------------------------------------------

-- | Parse sound commands
pSoundCmd :: Parser SoundCmd
pSoundCmd = choice [ pSoundPlay, SoundStop <$ string "_V", pSoundBang ]

-- | Parse sound play: \_v[file]
pSoundPlay :: Parser SoundCmd
pSoundPlay = string "_v" *> pBracketed (SoundPlay <$> pQuotedStringOrId)

-- | Parse sound bang commands
pSoundBang :: Parser SoundCmd
pSoundBang = string "![" *> string "sound" *> pComma *> pSoundAction' <* char ']'

pSoundAction' :: Parser SoundCmd
pSoundAction' = do
  action <- choice
    [ SoundActionPlay <$ string "play"
    , SoundActionLoop <$ string "loop"
    , SoundActionPause <$ string "pause"
    , SoundActionResume <$ string "resume"
    , SoundActionSoundStop <$ string "stop"
    , SoundActionWait <$ string "wait"
    ]
  _ <- pComma
  file <- pQuotedStringOrId
  refs <- many (pComma *> pIdentifier)
  pure $ SoundAction action file refs

--------------------------------------------------------------------------------
-- Open commands
--------------------------------------------------------------------------------

-- | Parse open commands
pOpenCmd :: Parser OpenCmd
pOpenCmd = choice [ pOpenURL, pOpenBang ]

-- | Parse URL open: \j[url]
pOpenURL :: Parser OpenCmd
pOpenURL = char 'j' *> pBracketed (OpenURL <$> pURLText)

-- | Parse open bang commands
pOpenBang :: Parser OpenCmd
pOpenBang
  = string "!["
  *> string "open"
  *> pComma
  *> choice
    [ pOpenBrowser
    , pOpenMailer
    , pOpenFile
    , pOpenEditor
    , pOpenInputBox
    , pOpenDialog
    , pOpenTeachBox
    , pOpenConfigMenu
    ]
  <* char ']'

pOpenBrowser :: Parser OpenCmd
pOpenBrowser = string "browser" *> pComma *> (OpenBrowser <$> pURLText)

pOpenMailer :: Parser OpenCmd
pOpenMailer = string "mailer" *> pComma *> (OpenMailer <$> pIdentifier)

pOpenFile :: Parser OpenCmd
pOpenFile = string "file" *> pComma *> (OpenFile <$> pQuotedStringOrId)

pOpenEditor :: Parser OpenCmd
pOpenEditor = string "editor" *> pComma *> (OpenEditor <$> pQuotedStringOrId)

pOpenInputBox :: Parser OpenCmd
pOpenInputBox = string "inputbox" *> pComma *> do
  eventId <- pIdentifier
  opts <- many (pComma *> pInputOpt)
  pure $ OpenInputBox eventId opts
  where
    pInputOpt
      = choice
        [ InputDefault <$> (string "default=" *> pQuotedStringOrId)
        , InputMaxLength <$> (string "maxlength=" *> pInt)
        , InputPassword <$ string "password"
        , InputMultiline <$ string "multiline"
        ]

pOpenDialog :: Parser OpenCmd
pOpenDialog = string "dialog" *> pComma *> do
  eventId <- pIdentifier
  _ <- pComma
  opt <- pDialogOpt
  pure $ OpenDialog eventId opt
  where
    pDialogOpt
      = choice
        [ DialogOK <$ string "ok"
        , DialogOKCancel <$ string "okcancel"
        , DialogYesNo <$ string "yesno"
        , DialogYesNoCancel <$ string "yesnocancel"
        ]

pOpenTeachBox :: Parser OpenCmd
pOpenTeachBox = OpenTeachBox <$ string "teachbox"

pOpenConfigMenu :: Parser OpenCmd
pOpenConfigMenu = OpenConfigMenu <$ string "configmenu"

--------------------------------------------------------------------------------
-- Meta commands
--------------------------------------------------------------------------------

-- | Parse meta commands
pMetaCmd :: Parser MetaCmd
pMetaCmd
  = string "!["
  *> choice [ pMetaSet, pMetaGet, pMetaReload, pMetaExecute, pPassiveMode, pLock, pUnlock ]
  <* char ']'

pMetaSet :: Parser MetaCmd
pMetaSet = string "set" *> pComma *> (MetaSet <$> pSetProperty)

pSetProperty :: Parser SetProperty
pSetProperty
  = choice
    [ SetAutoscroll <$> (string "autoscroll" *> pComma *> pBool)
    , SetVerbatim <$> (string "verbatim" *> pComma *> pBool)
    , SetBalloonTimeout <$> (string "balloontimeout" *> pComma *> pInt)
    , SetChoiceTimeout <$> (string "choicetimeout" *> pComma *> pInt)
    , SetWalkSpeed <$> (string "walkspeed" *> pComma *> pInt)
    , SetTalkInterval <$> (string "talkinterval" *> pComma *> pInt)
    , pGenericSet
    ]
  where
    pGenericSet = do
      name <- pIdentifier
      _ <- pComma
      value <- pIdentifier
      pure $ SetProperty' name value

pMetaGet :: Parser MetaCmd
pMetaGet = string "get" *> pComma *> (MetaGet <$> pGetProperty)

pGetProperty :: Parser GetProperty
pGetProperty
  = choice
    [ GetYear <$ string "year"
    , GetMonth <$ string "month"
    , GetDay <$ string "day"
    , GetHour <$ string "hour"
    , GetMinute <$ string "minute"
    , GetSecond <$ string "second"
    , GetProperty' <$> pIdentifier
    ]

pMetaReload :: Parser MetaCmd
pMetaReload = string "reload" *> pComma *> (MetaReload <$> pReloadTarget)

pReloadTarget :: Parser ReloadTarget
pReloadTarget
  = choice
    [ ReloadGhost <$ string "ghost"
    , ReloadShell <$ string "shell"
    , ReloadBalloon <$ string "balloon"
    , ReloadPlugin <$ string "plugin"
    , ReloadHeadline <$ string "headline"
    , ReloadAll <$ string "all"
    ]

pMetaExecute :: Parser MetaCmd
pMetaExecute = string "execute" *> pComma *> (MetaExecute <$> pExecuteCmd)

pExecuteCmd :: Parser ExecuteCmd
pExecuteCmd = choice [ pExecuteHttpGet, pExecuteHttpPost, pExecuteFile ]
  where
    pExecuteHttpGet  = string "http-get" *> pComma *> (ExecuteHTTPGet <$> pURLText)

    pExecuteHttpPost = string "http-post" *> pComma *> do
      url <- pURLText
      _ <- pComma
      body <- pQuotedStringOrId
      pure $ ExecuteHTTPPost url body

    pExecuteFile     = string "file" *> pComma *> (ExecuteFile <$> pQuotedStringOrId)

pPassiveMode :: Parser MetaCmd
pPassiveMode = string "passivemode" *> pComma *> (MetaPassiveMode <$> pBool)

pLock :: Parser MetaCmd
pLock = string "lock" *> pComma *> (MetaLock <$> pIdentifier)

pUnlock :: Parser MetaCmd
pUnlock = string "unlock" *> pComma *> (MetaUnlock <$> pIdentifier)

--------------------------------------------------------------------------------
-- Environment variables
--------------------------------------------------------------------------------

-- | Parse environment variable: %varname
pEnvVar :: Parser EnvVar
pEnvVar
  = char '%'
  *> choice
    -- Time
    [ EnvYear <$ string "year"
    , EnvMonth <$ string "month"
    , EnvDay <$ string "day"
    , EnvHour <$ string "hour"
    , EnvMinute <$ string "minute"
    , EnvSecond <$ string "second"
    , EnvWeekday <$ string "weekday"
      -- Ghost info
    , EnvSelfname <$ string "selfname"
    , EnvSelfname2 <$ string "selfname2"
    , EnvKeroname <$ string "keroname"
    , EnvGhostname <$ string "ghostname"
    , EnvShellname <$ string "shellname"
      -- User info
    , EnvUsername <$ string "username"
    , EnvOS <$ string "os"
      -- Screen
    , EnvScreenWidth <$ string "screenwidth"
    , EnvScreenHeight <$ string "screenheight"
      -- Surface
    , EnvSurface <$ string "surface"
    , EnvSurface0 <$ string "surface0"
    , EnvSurface1 <$ string "surface1"
      -- Custom fallback
    , EnvCustom <$> pIdentifier
    ]

--------------------------------------------------------------------------------
-- Utility parsers
--------------------------------------------------------------------------------

-- | Parse bracketed content: [content]
pBracketed :: Parser a -> Parser a
pBracketed p = char '[' *> p <* char ']'

-- | Parse bracketed integer
pBracketedInt :: Parser Int
pBracketedInt = pBracketed pInt

-- | Parse integer
pInt :: Parser Int
pInt = do
  sign <- option id (negate <$ char '-')
  digits <- some digitChar
  pure $ sign (read digits)

-- | Parse comma separator
pComma :: Parser ()
pComma = void $ char ','

-- | Parse boolean
pBool :: Parser Bool
pBool = choice [ True <$ (string "true" <|> string "1"), False <$ (string "false" <|> string "0") ]

-- | Parse identifier (alphanumeric + underscore)
pIdentifier :: Parser Text
pIdentifier = T.pack <$> some (satisfy isIdChar)
  where
    isIdChar c = c /= ',' && c /= ']' && c /= '[' && c /= '\\' && c /= '%' && c /= ' '

-- | Parse quoted string
pQuotedString :: Parser Text
pQuotedString = T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

-- | Parse quoted string or identifier
pQuotedStringOrId :: Parser Text
pQuotedStringOrId = pQuotedString <|> pIdentifier

-- | Parse URL text (up to comma or bracket)
pURLText :: Parser Text
pURLText = T.pack <$> many (satisfy (\c -> c /= ',' && c /= ']'))

-- | Parse color specification
pColor :: Parser Color
pColor
  = choice
    [ ColorDefault <$ string "default", try pColorHex, try pColorRGB, ColorName <$> pIdentifier ]

-- | Parse hex color: #RGB or #RRGGBB
pColorHex :: Parser Color
pColorHex = do
  _ <- char '#'
  hex <- some hexDigitChar
  pure $ ColorHex (T.pack ('#' : hex))

-- | Parse RGB color: rgb(r,g,b)
pColorRGB :: Parser Color
pColorRGB = do
  _ <- string "rgb("
  r <- pInt
  _ <- pComma
  g <- pInt
  _ <- pComma
  b <- pInt
  _ <- char ')'
  pure $ ColorRGB r g b

-- | Parse cursor position
pCursorPos :: Parser CursorPos
pCursorPos
  = choice
    [ PosUnchanged <$ string "@"
    , try pPosRelative
    , try pPosEm
    , try pPosPercent
    , PosAbsolute <$> pInt
    ]
  where
    pPosRelative = do
      sign <- (id <$ char '+') <|> (negate <$ char '-')
      n <- pInt
      pure $ PosRelative (sign n)

    pPosEm       = do
      n <- pDouble
      _ <- string "em"
      pure $ PosEm n

    pPosPercent  = do
      n <- pDouble
      _ <- char '%'
      pure $ PosPercent n

-- | Parse double
pDouble :: Parser Double
pDouble = do
  intPart <- some digitChar
  fracPart <- option "" ((:) <$> char '.' <*> some digitChar)
  pure $ read (intPart ++ fracPart)
