# 2025-12-19-ghost-descript-types.md Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fill the fields of `Descript` in `@src/Types/Ghost.hs` according to the UKADOC spec.

**Architecture:** Define `Descript` as a Haskell record with fields corresponding to keys in `descript.txt`. Use `Text` for strings and `Int` for numbers. Wrap optional fields in `Maybe`.

**Tech Stack:** Haskell, Text

### Task 1: Create Test Spec for Ghost Types

**Files:**
- Create: `test/Types/GhostSpec.hs`

**Step 1: Write the failing test (compilation fail effectively)**

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Types.GhostSpec (spec) where

import Test.Hspec
import Types.Ghost
import Data.Text (Text)

spec :: Spec
spec = do
  describe "Descript" $ do
    it "has the required fields" $ do
      let descript = Descript
            { descriptCharset = Just "UTF-8"
            , descriptType = "ghost"
            , descriptName = "Test Ghost"
            , descriptSakuraName = "Sakura"
            , descriptKeroName = "Kero"
            , descriptCraftman = Just "Author"
            , descriptCraftmanW = Just "AuthorW"
            , descriptId = Nothing
            , descriptTitle = Nothing
            , descriptCraftmanUrl = Nothing
            , descriptHomeUrl = Nothing
            , descriptReadme = Nothing
            , descriptReadmeCharset = Nothing
            , descriptSakuraName2 = Nothing
            , descriptSakuraSerikoDefaultSurface = Nothing
            , descriptKeroSerikoDefaultSurface = Nothing
            , descriptSerikoAlignmentToDesktop = Nothing
            , descriptSakuraSerikoAlignmentToDesktop = Nothing
            , descriptKeroSerikoAlignmentToDesktop = Nothing
            , descriptSakuraDefaultX = Nothing
            , descriptKeroDefaultX = Nothing
            , descriptSakuraDefaultY = Nothing
            , descriptKeroDefaultY = Nothing
            , descriptSakuraDefaultLeft = Nothing
            , descriptKeroDefaultLeft = Nothing
            , descriptSakuraDefaultTop = Nothing
            , descriptKeroDefaultTop = Nothing
            , descriptSerikoDefaultSurfaceDirectoryName = Nothing
            , descriptSstpAllowUnspecifiedSend = Nothing
            , descriptSstpAllowCommunicate = Nothing
            , descriptSstpAlwaysTranslate = Nothing
            , descriptNameAllowOverride = Nothing
            , descriptShioriVersion = Nothing
            , descriptShioriCache = Nothing
            , descriptShioriEncoding = Nothing
            , descriptShioriForceEncoding = Nothing
            , descriptShioriEscapeUnknown = Nothing
            , descriptDontNeedOnMouseMove = Nothing
            , descriptDontNeedBind = Nothing
            , descriptDontNeedSerikoTalk = Nothing
            , descriptBalloonDontMove = Nothing
            , descriptBalloonSyncScale = Nothing
            , descriptIcon = Nothing
            , descriptIconMinimize = Nothing
            , descriptCursor = Nothing
            , descriptMouseCursorText = Nothing
            , descriptMouseCursorWait = Nothing
            , descriptMouseCursorHand = Nothing
            , descriptMouseCursorGrip = Nothing
            , descriptMouseCursorArrow = Nothing
            , descriptShiori = Nothing
            , descriptMakoto = Nothing
            , descriptMenuFontName = Nothing
            , descriptMenuFontHeight = Nothing
            , descriptShioriLogoFile = Nothing
            , descriptShioriLogoX = Nothing
            , descriptShioriLogoY = Nothing
            , descriptShioriLogoAlign = Nothing
            , descriptInstallAccept = Nothing
            , descriptBalloon = Nothing
            , descriptDefaultBalloonPath = Nothing
            , descriptRecommendedBalloon = Nothing
            , descriptRecommendedBalloonPath = Nothing
            }
      descriptName descript `shouldBe` "Test Ghost"
```

**Step 2: Run test to verify it fails**
Run: `stack test`
Expected: Fail because `Descript` doesn't have these fields yet.

### Task 2: Implement Descript Data Type

**Files:**
- Modify: `src/Types/Ghost.hs`

**Step 3: Modify Types/Ghost.hs**

Add `import Data.Text (Text)`
Update `Descript` definition with all fields from the spec.

```haskell
module Types.Ghost (
    module Types.Ghost
  ) where

import Data.Text (Text)

data Descript = Descript
  { descriptCharset :: Maybe Text
  , descriptType :: Text
  , descriptName :: Text
  , descriptSakuraName :: Text
  , descriptKeroName :: Text
  , descriptCraftman :: Maybe Text
  , descriptCraftmanW :: Maybe Text
  , descriptId :: Maybe Text
  , descriptTitle :: Maybe Text
  , descriptCraftmanUrl :: Maybe Text
  , descriptHomeUrl :: Maybe Text
  , descriptReadme :: Maybe Text
  , descriptReadmeCharset :: Maybe Text
  , descriptSakuraName2 :: Maybe Text
  , descriptSakuraSerikoDefaultSurface :: Maybe Int
  , descriptKeroSerikoDefaultSurface :: Maybe Int
  , descriptSerikoAlignmentToDesktop :: Maybe Text
  , descriptSakuraSerikoAlignmentToDesktop :: Maybe Text
  , descriptKeroSerikoAlignmentToDesktop :: Maybe Text
  , descriptSakuraDefaultX :: Maybe Int
  , descriptKeroDefaultX :: Maybe Int
  , descriptSakuraDefaultY :: Maybe Int
  , descriptKeroDefaultY :: Maybe Int
  , descriptSakuraDefaultLeft :: Maybe Int
  , descriptKeroDefaultLeft :: Maybe Int
  , descriptSakuraDefaultTop :: Maybe Int
  , descriptKeroDefaultTop :: Maybe Int
  , descriptSerikoDefaultSurfaceDirectoryName :: Maybe Text
  , descriptSstpAllowUnspecifiedSend :: Maybe Int
  , descriptSstpAllowCommunicate :: Maybe Int
  , descriptSstpAlwaysTranslate :: Maybe Int
  , descriptNameAllowOverride :: Maybe Int
  , descriptShioriVersion :: Maybe Text
  , descriptShioriCache :: Maybe Int
  , descriptShioriEncoding :: Maybe Text
  , descriptShioriForceEncoding :: Maybe Text
  , descriptShioriEscapeUnknown :: Maybe Int
  , descriptDontNeedOnMouseMove :: Maybe Int
  , descriptDontNeedBind :: Maybe Int
  , descriptDontNeedSerikoTalk :: Maybe Int
  , descriptBalloonDontMove :: Maybe Bool
  , descriptBalloonSyncScale :: Maybe Bool
  , descriptIcon :: Maybe Text
  , descriptIconMinimize :: Maybe Text
  , descriptCursor :: Maybe Text
  , descriptMouseCursorText :: Maybe Text
  , descriptMouseCursorWait :: Maybe Text
  , descriptMouseCursorHand :: Maybe Text
  , descriptMouseCursorGrip :: Maybe Text
  , descriptMouseCursorArrow :: Maybe Text
  , descriptShiori :: Maybe Text
  , descriptMakoto :: Maybe Text
  , descriptMenuFontName :: Maybe Text
  , descriptMenuFontHeight :: Maybe Int
  , descriptShioriLogoFile :: Maybe Text
  , descriptShioriLogoX :: Maybe Int
  , descriptShioriLogoY :: Maybe Int
  , descriptShioriLogoAlign :: Maybe Text
  , descriptInstallAccept :: Maybe Text
  , descriptBalloon :: Maybe Text
  , descriptDefaultBalloonPath :: Maybe Text
  , descriptRecommendedBalloon :: Maybe Text
  , descriptRecommendedBalloonPath :: Maybe Text
  }
  deriving (Show, Eq)

data Ghost = Ghost
 {
    ghostDescript :: Descript
 }
 deriving (Show)
```

**Step 4: Run tests**
Run: `stack test`
Expected: PASS

**Step 5: Commit**
`git add src/Types/Ghost.hs test/Types/GhostSpec.hs`
`git commit -m "feat: implement Descript data type based on UKADOC spec"`
