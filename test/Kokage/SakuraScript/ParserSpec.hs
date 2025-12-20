{-# LANGUAGE OverloadedStrings #-}

module Kokage.SakuraScript.ParserSpec ( spec ) where

import           Test.Hspec
import           Test.Hspec.Megaparsec

import           Kokage.SakuraScript.Parser
import           Types.SakuraScript

-- | Main spec
spec :: Spec
spec = do
  describe "Sakura Script Parser" $ do
    basicTextSpec
    escapeSpec
    scopeSpec
    surfaceSpec
    balloonSpec
    waitSpec
    choiceSpec
    fontSpec
    envVarSpec

--------------------------------------------------------------------------------
-- Basic Text Parsing
--------------------------------------------------------------------------------

basicTextSpec :: Spec
basicTextSpec = describe "basic text" $ do
  it "parses plain text" $
    parseScript "Hello world" `shouldParse` [SSText "Hello world"]

  it "parses empty string" $
    parseScript "" `shouldParse` []

  it "parses text with Japanese characters" $
    parseScript "こんにちは" `shouldParse` [SSText "こんにちは"]

  it "parses mixed text and commands" $
    parseScript "Hello\\0world" `shouldParse`
      [SSText "Hello", SSScope ScopeMain, SSText "world"]

--------------------------------------------------------------------------------
-- Escape Sequences
--------------------------------------------------------------------------------

escapeSpec :: Spec
escapeSpec = describe "escape sequences" $ do
  it "parses escaped backslash" $
    parseScript "\\\\" `shouldParse` [SSEscape '\\']

  it "parses escaped percent" $
    parseScript "\\%" `shouldParse` [SSEscape '%']

--------------------------------------------------------------------------------
-- Scope Commands
--------------------------------------------------------------------------------

scopeSpec :: Spec
scopeSpec = describe "scope commands" $ do
  it "parses \\0 as main scope" $
    parseScript "\\0" `shouldParse` [SSScope ScopeMain]

  it "parses \\h as main scope" $
    parseScript "\\h" `shouldParse` [SSScope ScopeMain]

  it "parses \\1 as kero scope" $
    parseScript "\\1" `shouldParse` [SSScope ScopeKero]

  it "parses \\u as kero scope" $
    parseScript "\\u" `shouldParse` [SSScope ScopeKero]

  it "parses \\p[n] as indexed scope" $
    parseScript "\\p[2]" `shouldParse` [SSScope (ScopeIndex 2)]

  it "parses \\p[0] as indexed scope 0" $
    parseScript "\\p[0]" `shouldParse` [SSScope (ScopeIndex 0)]

--------------------------------------------------------------------------------
-- Surface Commands
--------------------------------------------------------------------------------

surfaceSpec :: Spec
surfaceSpec = describe "surface commands" $ do
  it "parses \\s[n] as surface change" $
    parseScript "\\s[0]" `shouldParse` [SSSurface (SurfaceChange 0)]

  it "parses \\s[10] as surface change" $
    parseScript "\\s[10]" `shouldParse` [SSSurface (SurfaceChange 10)]

--------------------------------------------------------------------------------
-- Balloon Commands
--------------------------------------------------------------------------------

balloonSpec :: Spec
balloonSpec = describe "balloon commands" $ do
  it "parses \\n as newline" $
    parseScript "\\n" `shouldParse` [SSBalloon Newline]

  it "parses \\c as clear" $
    parseScript "\\c" `shouldParse` [SSBalloon Clear]

  it "parses \\b[n] as balloon change" $
    parseScript "\\b[1]" `shouldParse` [SSBalloon (BalloonChange 1)]

  it "parses \\_b as balloon hide" $
    parseScript "\\_b" `shouldParse` [SSBalloon BalloonHide]

  it "parses \\n[half] as half newline" $
    parseScript "\\n[half]" `shouldParse` [SSBalloon NewlineHalf]

--------------------------------------------------------------------------------
-- Wait Commands
--------------------------------------------------------------------------------

waitSpec :: Spec
waitSpec = describe "wait commands" $ do
  it "parses \\w[n] as simple wait" $
    parseScript "\\w9" `shouldParse` [SSWait (WaitSimple 9)]

  it "parses \\_w[n] as millisecond wait" $
    parseScript "\\_w[1000]" `shouldParse` [SSWait (WaitMs 1000)]

  it "parses \\x as click wait" $
    parseScript "\\x" `shouldParse` [SSWait ClickWait]

  it "parses \\t as time critical start" $
    parseScript "\\t" `shouldParse` [SSWait TimeCriticalStart]

--------------------------------------------------------------------------------
-- Choice Commands
--------------------------------------------------------------------------------

choiceSpec :: Spec
choiceSpec = describe "choice commands" $ do
  it "parses basic choice \\q[text,id]" $
    parseScript "\\q[Yes,OnYes]" `shouldParse`
      [SSChoice (Choice "Yes" (ChoiceEvent "OnYes"))]

  it "parses anchor \\_a[id,text]" $
    parseScript "\\_a[link1,Click here]" `shouldParse`
      [SSChoice (Anchor "link1" "Click here")]

--------------------------------------------------------------------------------
-- Font Commands
--------------------------------------------------------------------------------

fontSpec :: Spec
fontSpec = describe "font commands" $ do
  it "parses \\f[bold,true] as bold on" $
    parseScript "\\f[bold,true]" `shouldParse` [SSFont (FontBold ToggleOn)]

  it "parses \\f[bold,false] as bold off" $
    parseScript "\\f[bold,false]" `shouldParse` [SSFont (FontBold ToggleOff)]

  it "parses \\f[italic,true] as italic on" $
    parseScript "\\f[italic,true]" `shouldParse` [SSFont (FontItalic ToggleOn)]

  it "parses \\f[default] as font default" $
    parseScript "\\f[default]" `shouldParse` [SSFont FontDefault]

--------------------------------------------------------------------------------
-- Environment Variables
--------------------------------------------------------------------------------

envVarSpec :: Spec
envVarSpec = describe "environment variables" $ do
  it "parses %username" $
    parseScript "%username" `shouldParse` [SSEnvVar EnvUsername]

  it "parses %year" $
    parseScript "%year" `shouldParse` [SSEnvVar EnvYear]

  it "parses %selfname" $
    parseScript "%selfname" `shouldParse` [SSEnvVar EnvSelfname]

  it "parses %screenwidth" $
    parseScript "%screenwidth" `shouldParse` [SSEnvVar EnvScreenWidth]

  it "parses text with embedded env var" $
    parseScript "Hello %username!" `shouldParse`
      [SSText "Hello ", SSEnvVar EnvUsername, SSText "!"]
