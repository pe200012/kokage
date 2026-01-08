{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Ghost descriptor types and parsers.
-- Parses ghost/master/descript.txt files.
module Types.Ghost.Descript ( GhostDescript(..), emptyGhostDescript, readGhostDescript ) where

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Text                  ( Text )
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE

import           Utils.Charset              ( convertToUtf8, detectCharset )
import           Utils.Text                 ( clean, readBoolOr, readIntOr, readMaybeInt )

data GhostDescript
  = Descript
  { -- | Character code for displaying text. Default: OS standard or SSP default.
    descriptCharset :: Text
    -- | The name of the ghost.
  , descriptName :: Text
    -- | File set type, for ghost it's "ghost".
  , descriptType :: Text
    -- | Author name (half-width alphanumeric).
  , descriptCraftman :: Text
    -- | Author name.
  , descriptCraftmanw :: Text
    -- | ID name (half-width alphanumeric).
  , descriptId :: Maybe Text
    -- | Display name in launch log.
  , descriptTitle :: Maybe Text
    -- | Author's URL.
  , descriptCraftmanUrl :: Maybe Text
    -- | Network update URL.
  , descriptHomeUrl :: Maybe Text
    -- | Readme file name. Default: readme.txt
  , descriptReadme :: Text
    -- | Character code for readme. Default: OS standard or baseware default.
  , descriptReadmeCharset :: Maybe Text
    -- | Name of the main ghost.
  , descriptSakuraName :: Text
    -- | Nickname of the main ghost.
  , descriptSakuraName2 :: Maybe Text
    -- | Name of the partner ghost.
  , descriptKeroName :: Text
    -- | Default surface number for main ghost. Default: 0
  , descriptSakuraSerikoDefaultSurface :: Int
    -- | Default surface number for partner ghost. Default: 10
  , descriptKeroSerikoDefaultSurface :: Int
    -- | Default surface number for balloon. Default: 0
  , descriptBalloonDefaultSurface :: Int
    -- | Default surface number for main balloon. Default: 0
  , descriptSakuraBalloonDefaultSurface :: Int
    -- | Default surface number for partner balloon. Default: 0
  , descriptKeroBalloonDefaultSurface :: Int
    -- | Default display position info for surfaces. Default: bottom
  , descriptSerikoAlignmentToDesktop :: Text
    -- | Default display position for main.
  , descriptSakuraSerikoAlignmentToDesktop :: Maybe Text
    -- | Default display position for partner.
  , descriptKeroSerikoAlignmentToDesktop :: Maybe Text
    -- | Image-based X coordinate for main. Default: image center X
  , descriptSakuraDefaultX :: Maybe Int
    -- | Image-based X coordinate for partner. Default: image center X
  , descriptKeroDefaultX :: Maybe Int
    -- | Image-based Y coordinate for main. Default: image bottom Y
  , descriptSakuraDefaultY :: Maybe Int
    -- | Image-based Y coordinate for partner. Default: image bottom Y
  , descriptKeroDefaultY :: Maybe Int
    -- | Display X coordinate for main.
  , descriptSakuraDefaultLeft :: Maybe Int
    -- | Display X coordinate for partner.
  , descriptKeroDefaultLeft :: Maybe Int
    -- | Display Y coordinate for main (when free).
  , descriptSakuraDefaultTop :: Maybe Int
    -- | Display Y coordinate for partner (when free).
  , descriptKeroDefaultTop :: Maybe Int
    -- | Default shell directory name. Default: master
  , descriptSerikoDefaultSurfaceDirectoryName :: Text
    -- | Allow SSTP without specifying ghost. Default: 1
  , descriptSstpAllowUnspecifiedSend :: Int
    -- | Allow COMMUNICATE. Default: 1
  , descriptSstpAllowCommunicate :: Int
    -- | Always translate SSTP + SHIORI OnTranslate. Default: 0
  , descriptSstpAlwaysTranslate :: Int
    -- | Allow shell to override sakura.name/kero.name. Default: 1
  , descriptNameAllowOverride :: Int
    -- | SHIORI protocol version.
  , descriptShioriVersion :: Maybe Text
    -- | SHIORI cache behavior. Default: 1
  , descriptShioriCache :: Int
    -- | Character code for SHIORI communication.
  , descriptShioriEncoding :: Maybe Text
    -- | Force character code for SHIORI.
  , descriptShioriForceEncoding :: Maybe Text
    -- | Escape unknown Unicode. Default: 0
  , descriptShioriEscapeUnknown :: Int
    -- | Disable OnMouseMove. Default: 0
  , descriptDontNeedOnMouseMove :: Int
    -- | Disable costume changing. Default: 0
  , descriptDontNeedBind :: Int
    -- | Disable SERIKO talk (lip sync). Default: 0
  , descriptDontNeedSerikoTalk :: Int
    -- | Disable balloon position adjustment. Default: false
  , descriptBalloonDontMove :: Bool
    -- | Force balloon scale sync with ghost. Default: false
  , descriptBalloonSyncScale :: Bool
    -- | Task tray icon.
  , descriptIcon :: Maybe Text
    -- | Minimized icon. Default: icon
  , descriptIconMinimize :: Maybe Text
    -- | Cursor for operable parts. Default: system finger
  , descriptCursor :: Maybe Text
    -- | Cursor for operable parts (alias for cursor).
  , descriptMouseCursor :: Maybe Text
    -- | Cursor for input boxes. Default: system text
  , descriptMouseCursorText :: Maybe Text
    -- | Cursor when processing. Default: system wait
  , descriptMouseCursorWait :: Maybe Text
    -- | Cursor for grabbable parts. Default: system hand
  , descriptMouseCursorHand :: Maybe Text
    -- | Cursor when grabbing. Default: system grip
  , descriptMouseCursorGrip :: Maybe Text
    -- | Cursor for non-operable parts. Default: system arrow
  , descriptMouseCursorArrow :: Maybe Text
    -- | SHIORI file name. Default: shiori.dll
  , descriptShiori :: Text
    -- | MAKOTO file name. Default: makoto.dll
  , descriptMakoto :: Text
    -- | Font for owner draw menu. Default: OS UI font
  , descriptMenuFontName :: Maybe Text
    -- | Font size for menu. Default: OS UI font height
  , descriptMenuFontHeight :: Maybe Int
    -- | Image file for AI graph background.
  , descriptShioriLogoFile :: Maybe Text
    -- | X coordinate for logo. Default: 0
  , descriptShioriLogoX :: Int
    -- | Y coordinate for logo. Default: 0
  , descriptShioriLogoY :: Int
    -- | Alignment for logo position. Default: lefttop
  , descriptShioriLogoAlign :: Text
    -- | Allow installation of named ghosts.
  , descriptInstallAccept :: Maybe Text
    -- | Default balloon name.
  , descriptBalloon :: Maybe Text
    -- | Default balloon path.
  , descriptDefaultBalloonPath :: Maybe Text
    -- | Recommended balloon name.
  , descriptRecommendedBalloon :: Maybe Text
    -- | Recommended balloon path.
  , descriptRecommendedBalloonPath :: Maybe Text
  }
  deriving ( Show, Eq )

emptyGhostDescript :: GhostDescript
emptyGhostDescript
  = Descript
  { descriptCharset = ""
  , descriptName = ""
  , descriptType = ""
  , descriptCraftman = ""
  , descriptCraftmanw = ""
  , descriptId = Nothing
  , descriptTitle = Nothing
  , descriptCraftmanUrl = Nothing
  , descriptHomeUrl = Nothing
  , descriptReadme = "readme.txt"
  , descriptReadmeCharset = Nothing
  , descriptSakuraName = ""
  , descriptSakuraName2 = Nothing
  , descriptKeroName = ""
  , descriptSakuraSerikoDefaultSurface = 0
  , descriptKeroSerikoDefaultSurface = 10
  , descriptBalloonDefaultSurface = 0
  , descriptSakuraBalloonDefaultSurface = 0
  , descriptKeroBalloonDefaultSurface = 0
  , descriptSerikoAlignmentToDesktop = "bottom"
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
  , descriptSerikoDefaultSurfaceDirectoryName = "master"
  , descriptSstpAllowUnspecifiedSend = 1
  , descriptSstpAllowCommunicate = 1
  , descriptSstpAlwaysTranslate = 0
  , descriptNameAllowOverride = 1
  , descriptShioriVersion = Nothing
  , descriptShioriCache = 1
  , descriptShioriEncoding = Nothing
  , descriptShioriForceEncoding = Nothing
  , descriptShioriEscapeUnknown = 0
  , descriptDontNeedOnMouseMove = 0
  , descriptDontNeedBind = 0
  , descriptDontNeedSerikoTalk = 0
  , descriptBalloonDontMove = False
  , descriptBalloonSyncScale = False
  , descriptIcon = Nothing
  , descriptIconMinimize = Nothing
  , descriptCursor = Nothing
  , descriptMouseCursor = Nothing
  , descriptMouseCursorText = Nothing
  , descriptMouseCursorWait = Nothing
  , descriptMouseCursorHand = Nothing
  , descriptMouseCursorGrip = Nothing
  , descriptMouseCursorArrow = Nothing
  , descriptShiori = "shiori.dll"
  , descriptMakoto = "makoto.dll"
  , descriptMenuFontName = Nothing
  , descriptMenuFontHeight = Nothing
  , descriptShioriLogoFile = Nothing
  , descriptShioriLogoX = 0
  , descriptShioriLogoY = 0
  , descriptShioriLogoAlign = "lefttop"
  , descriptInstallAccept = Nothing
  , descriptBalloon = Nothing
  , descriptDefaultBalloonPath = Nothing
  , descriptRecommendedBalloon = Nothing
  , descriptRecommendedBalloonPath = Nothing
  }

readGhostDescript :: FilePath -> IO GhostDescript
readGhostDescript path = do
  rawBytes <- BL.readFile path
  let rawLines        = BL8.lines rawBytes
      detectedCharset = detectCharset rawLines
      utf8Bytes       = convertToUtf8 detectedCharset rawBytes
      contents        = TE.decodeUtf8 (BL.toStrict utf8Bytes)
  return $ foldl' parseLine emptyGhostDescript (T.lines contents)
  where
    parseLine desc line = case T.breakOn "," line of
      ( rawKey, rest )
        | not (T.null rest) -> let
            key    = T.toLower (clean rawKey)
            rawVal = T.drop 1 rest  -- drop the comma
            val    = clean rawVal
          in 
            case key of
              "charset" -> desc { descriptCharset = val }
              "name" -> desc { descriptName = val }
              "type" -> desc { descriptType = val }
              "craftman" -> desc { descriptCraftman = val }
              "craftmanw" -> desc { descriptCraftmanw = val }

              "id" -> desc { descriptId = Just val }
              "title" -> desc { descriptTitle = Just val }
              "craftmanurl" -> desc { descriptCraftmanUrl = Just val }
              "homeurl" -> desc { descriptHomeUrl = Just val }
              "readme" -> desc { descriptReadme = val }
              "readme.charset" -> desc { descriptReadmeCharset = Just val }

              "sakura.name" -> desc { descriptSakuraName = val }
              "sakura.name2" -> desc { descriptSakuraName2 = Just val }
              "kero.name" -> desc { descriptKeroName = val }

              "sakura.seriko.defaultsurface" -> desc
                { descriptSakuraSerikoDefaultSurface = readIntOr 0 val }
              "kero.seriko.defaultsurface" -> desc
                { descriptKeroSerikoDefaultSurface = readIntOr 10 val }

              "seriko.alignmenttodesktop" -> desc { descriptSerikoAlignmentToDesktop = val }
              "sakura.seriko.alignmenttodesktop" -> desc
                { descriptSakuraSerikoAlignmentToDesktop = Just val }
              "kero.seriko.alignmenttodesktop" -> desc
                { descriptKeroSerikoAlignmentToDesktop = Just val }

              "sakura.defaultx" -> desc { descriptSakuraDefaultX = readMaybeInt val }
              "kero.defaultx" -> desc { descriptKeroDefaultX = readMaybeInt val }
              "sakura.defaulty" -> desc { descriptSakuraDefaultY = readMaybeInt val }
              "kero.defaulty" -> desc { descriptKeroDefaultY = readMaybeInt val }
              "sakura.defaultleft" -> desc { descriptSakuraDefaultLeft = readMaybeInt val }
              "kero.defaultleft" -> desc { descriptKeroDefaultLeft = readMaybeInt val }
              "sakura.defaulttop" -> desc { descriptSakuraDefaultTop = readMaybeInt val }
              "kero.defaulttop" -> desc { descriptKeroDefaultTop = readMaybeInt val }

              "seriko.defaultsurfacedirectoryname" -> desc
                { descriptSerikoDefaultSurfaceDirectoryName = val }
              "sstp.allowunspecifiedsend" -> desc
                { descriptSstpAllowUnspecifiedSend = readIntOr 1 val }
              "sstp.allowcommunicate" -> desc { descriptSstpAllowCommunicate = readIntOr 1 val }
              "sstp.alwaystranslate" -> desc { descriptSstpAlwaysTranslate = readIntOr 0 val }
              "name.allowoverride" -> desc { descriptNameAllowOverride = readIntOr 1 val }

              "shiori.version" -> desc { descriptShioriVersion = Just val }
              "shiori.cache" -> desc { descriptShioriCache = readIntOr 1 val }
              "shiori.encoding" -> desc { descriptShioriEncoding = Just val }
              "shiori.forceencoding" -> desc { descriptShioriForceEncoding = Just val }
              "shiori.escape_unknown" -> desc { descriptShioriEscapeUnknown = readIntOr 0 val }

              "don't need onmousemove" -> desc { descriptDontNeedOnMouseMove = readIntOr 0 val }
              "don't need bind" -> desc { descriptDontNeedBind = readIntOr 0 val }
              "don't need seriko talk" -> desc { descriptDontNeedSerikoTalk = readIntOr 0 val }

              "balloon.dontmove" -> desc { descriptBalloonDontMove = readBoolOr False val }
              "balloon.syncscale" -> desc { descriptBalloonSyncScale = readBoolOr False val }
              "balloon.defaultsurface" -> desc { descriptBalloonDefaultSurface = readIntOr 0 val }
              "sakura.balloon.defaultsurface" -> desc
                { descriptSakuraBalloonDefaultSurface = readIntOr 0 val }
              "kero.balloon.defaultsurface" -> desc
                { descriptKeroBalloonDefaultSurface = readIntOr 0 val }

              -- we should keep raw values for icon and dll paths
              "icon" -> desc { descriptIcon = Just rawVal }
              "icon.minimize" -> desc { descriptIconMinimize = Just val }
              "cursor" -> desc { descriptCursor = Just rawVal }
              "mousecursor" -> desc { descriptMouseCursor = Just val }
              "mousecursor.text" -> desc { descriptMouseCursorText = Just val }
              "mousecursor.wait" -> desc { descriptMouseCursorWait = Just val }
              "mousecursor.hand" -> desc { descriptMouseCursorHand = Just val }
              "mousecursor.grip" -> desc { descriptMouseCursorGrip = Just val }
              "mousecursor.arrow" -> desc { descriptMouseCursorArrow = Just val }

              "shiori" -> desc { descriptShiori = val }
              "makoto" -> desc { descriptMakoto = val }
              "menu.font.name" -> desc { descriptMenuFontName = Just val }
              "menu.font.height" -> desc { descriptMenuFontHeight = readMaybeInt val }
              "shiori.logo.file" -> desc { descriptShioriLogoFile = Just val }
              "shiori.logo.x" -> desc { descriptShioriLogoX = readIntOr 0 val }
              "shiori.logo.y" -> desc { descriptShioriLogoY = readIntOr 0 val }
              "shiori.logo.align" -> desc { descriptShioriLogoAlign = val }
              "install.accept" -> desc { descriptInstallAccept = Just val }

              "balloon" -> desc { descriptBalloon = Just val }
              "default.balloon.path" -> desc { descriptDefaultBalloonPath = Just val }
              "recommended.balloon" -> desc { descriptRecommendedBalloon = Just val }
              "recommended.balloon.path" -> desc { descriptRecommendedBalloonPath = Just val }

              _ -> desc
      _ -> desc
