{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Ghost, Shell, and Surface type definitions and parsers
module Types.Ghost
  ( -- * Charset utilities
    isValidUtf8
  , hasUtf8Bom
  , guessCharset
  , detectCharset
  , detectCharsetFromBytes
  , convertToUtf8
    -- * Text utilities
  , clean
  , cleanStr
  , readIntOr
  , readMaybeInt
  , readBoolOr
  , readMaybeBool
    -- * Ghost types
  , GhostDescript(..)
  , emptyGhostDescript
  , readGhostDescript
  , Ghost(..)
  , loadGhost
    -- * Shell types
  , ShellDescript(..)
  , emptyShellDescript
  , readShellDescript
  , Shell(..)
  , loadShell
  , getCharSettings
  , updateCharSettings
    -- * Character settings
  , CharacterSettings(..)
  , emptyCharacterSettings
  , BindGroup(..)
  , emptyBindGroup
  , BindOptionType(..)
  , BindOption(..)
  , MenuItem(..)
  , MenuItemEx(..)
    -- * Surface types
  , Surface(..)
  , SurfaceDefinition(..)
  , emptySurfaceDefinition
  , Surfaces(..)
  , emptySurfaces
  , SurfacesDescript(..)
  , emptySurfacesDescript
  , readSurfaces
    -- * Surface components
  , Element(..)
  , Animation(..)
  , AnimationPattern(..)
  , AnimationInterval(..)
  , AnimationOption(..)
  , DrawMethod(..)
  , CollisionRegion(..)
  , CollisionShape(..)
  , SortOrder(..)
    -- * Surface aliases and cursors
  , SurfaceAlias(..)
  , CursorDef(..)
  , ScopeCursors(..)
  , emptyScopeCursors
  , TooltipDef(..)
    -- * Parsing utilities
  , BraceBlock(..)
  , tokenizeBraces
  , parseSurfaceIds
  , parseDrawMethod
  , parseAnimationInterval
  ) where

import           Codec.Text.IConv           ( EncodingName, convert )

import           Control.Applicative        ( (<|>) )
import           Control.Exception          ( SomeException, try )
import           Control.Monad              ( filterM, forM )

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Char                  ( isSpace, toLower )
import           Data.Map.Strict            ( Map )
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 ( catMaybes, fromMaybe, mapMaybe )
import           Data.Text                  ( Text )
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Word                  ( Word8 )

import           System.Directory           ( doesDirectoryExist, doesFileExist, listDirectory )
import           System.FilePath            ( (</>) )

import           Text.Read                  ( readMaybe )

-- | Check if bytes appear to be valid UTF-8.
-- Returns True if the bytes are valid UTF-8, False otherwise.
isValidUtf8 :: BL.ByteString -> Bool
isValidUtf8 bs = go (BL.unpack bs)
  where
    go :: [ Word8 ] -> Bool
    go []         = True
    go (b : rest)
      -- ASCII (0x00-0x7F)



        | b <= 0x7F = go rest
        -- 2-byte sequence (0xC0-0xDF followed by 0x80-0xBF)
        | b >= 0xC2 && b <= 0xDF = check1Continuation rest
        -- 3-byte sequence (0xE0-0xEF followed by 2 continuation bytes)
        | b >= 0xE0 && b <= 0xEF = check2Continuations b rest
        -- 4-byte sequence (0xF0-0xF4 followed by 3 continuation bytes)
        | b >= 0xF0 && b <= 0xF4 = check3Continuations b rest
        -- Invalid leading byte
        | otherwise = False

    check1Continuation :: [ Word8 ] -> Bool
    check1Continuation (c : rest)
      | c >= 0x80 && c <= 0xBF = go rest
    check1Continuation _          = False

    check2Continuations :: Word8 -> [ Word8 ] -> Bool
    check2Continuations lead (c1 : c2 : rest)
      | c1 >= 0x80 && c1 <= 0xBF && c2 >= 0x80 && c2 <= 0xBF
        =
        -- Check for overlong encodings and surrogates
        case lead of
          0xE0 -> c1 >= 0xA0 && go rest  -- Overlong check
          0xED -> c1 <= 0x9F && go rest  -- Surrogate check
          _    -> go rest
    check2Continuations _ _ = False

    check3Continuations :: Word8 -> [ Word8 ] -> Bool
    check3Continuations lead (c1 : c2 : c3 : rest)
      | c1 >= 0x80 && c1 <= 0xBF && c2 >= 0x80 && c2 <= 0xBF && c3 >= 0x80 && c3 <= 0xBF
        = case lead of
          0xF0 -> c1 >= 0x90 && go rest  -- Overlong check
          0xF4 -> c1 <= 0x8F && go rest  -- Max codepoint check
          _    -> go rest
    check3Continuations _ _ = False

-- | Check if bytes have UTF-8 BOM (Byte Order Mark).
hasUtf8Bom :: BL.ByteString -> Bool
hasUtf8Bom bs = BL.take 3 bs == BL.pack [ 0xEF, 0xBB, 0xBF ]

-- | Guess charset by analyzing byte patterns.
-- Strategy:
-- 1. Check for UTF-8 BOM
-- 2. Check for explicit charset declaration
-- 3. Try to validate as UTF-8
-- 4. Default to CP932 (Shift_JIS) for Japanese ghost files
guessCharset :: BL.ByteString -> EncodingName
guessCharset bytes
  -- UTF-8 BOM present



    | hasUtf8Bom bytes = "UTF-8"
    -- Check for explicit charset declaration
    | Just charset <- findCharsetDeclaration (BL8.lines bytes) = charset
    -- Check if it's valid UTF-8 with non-ASCII characters
    | hasNonAscii bytes && isValidUtf8 bytes = "UTF-8"
    -- Default to CP932 (Shift_JIS) - most common for older Japanese files
    | otherwise = "CP932"
  where
    hasNonAscii :: BL.ByteString -> Bool
    hasNonAscii = BL.any (> 0x7F)

-- | Find charset declaration in file lines.
findCharsetDeclaration :: [ BL.ByteString ] -> Maybe EncodingName
findCharsetDeclaration lns = case filter (BL8.isPrefixOf (BL8.pack "charset,")) lns of
  (l : _) -> let
      charset = map toLower . cleanStr . BL8.unpack . BL8.drop 8 $ l
    in 
      Just $ normalizeCharset charset
  []      -> Nothing

-- | Normalize charset name to iconv encoding name.
normalizeCharset :: String -> EncodingName
normalizeCharset charset = case charset of
  "shift_jis" -> "CP932"
  "shiftjis"  -> "CP932"
  "sjis"      -> "CP932"
  "utf-8"     -> "UTF-8"
  "utf8"      -> "UTF-8"
  "euc-jp"    -> "EUC-JP"
  "eucjp"     -> "EUC-JP"
  "ascii"     -> "UTF-8"  -- ASCII is a subset of UTF-8
  _           -> "CP932"  -- Default to CP932 for unknown charsets

-- | Detect charset from raw descript file lines.
-- Looks for "charset,<encoding>" line and normalizes to iconv encoding name.
-- If no charset declaration found, guesses based on byte patterns.
detectCharset :: [ BL.ByteString ] -> EncodingName
detectCharset = fromMaybe "CP932" . findCharsetDeclaration  -- Default to Shift_JIS for Japanese files without declaration

-- | Detect charset from raw bytes with guessing.
detectCharsetFromBytes :: BL.ByteString -> EncodingName
detectCharsetFromBytes = guessCharset

-- | Convert bytes to UTF-8, skipping conversion if already UTF-8.
convertToUtf8 :: EncodingName -> BL.ByteString -> BL.ByteString
convertToUtf8 srcEnc bytes
  | srcEnc == "UTF-8" = bytes
  | otherwise = convert srcEnc "UTF-8" bytes

-- | Trim leading and trailing whitespace (String version for charset detection).
cleanStr :: String -> String
cleanStr = dropWhile isSpace . dropWhileEnd isSpace
  where
    dropWhileEnd p = foldr (\x acc -> if p x && null acc
                              then []
                              else x : acc) []

-- | Trim leading and trailing whitespace (Text version).
clean :: Text -> Text
clean = T.strip

-- | Read an Int with a default value if parsing fails.
readIntOr :: Int -> Text -> Int
readIntOr def val = fromMaybe def (readMaybe (T.unpack val))

-- | Read a Maybe Int from Text.
readMaybeInt :: Text -> Maybe Int
readMaybeInt = readMaybe . T.unpack

-- | Read a Bool with a default value if parsing fails.
readBoolOr :: Bool -> Text -> Bool
readBoolOr def val = case T.toLower val of
  "true"  -> True
  "1"     -> True
  "false" -> False
  "0"     -> False
  _       -> def

-- | Read a Maybe Bool from Text.
readMaybeBool :: Text -> Maybe Bool
readMaybeBool val = case T.toLower val of
  "true"  -> Just True
  "1"     -> Just True
  "false" -> Just False
  "0"     -> Just False
  _       -> Nothing

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

-- | Bind group definition for costume/clothing
data BindGroup
  = BindGroup { bgCategory  :: Text        -- ^ Category name (e.g., "服", "リボン")
              , bgPartName  :: Text        -- ^ Part name (e.g., "エプロンドレス")
              , bgThumbnail :: Maybe Text  -- ^ Optional thumbnail filename
              , bgDefault   :: Bool        -- ^ Show by default
              , bgAddIds    :: [ Int ]     -- ^ IDs to enable simultaneously
              }
  deriving ( Show, Eq )

-- | Default bind group
emptyBindGroup :: BindGroup
emptyBindGroup
  = BindGroup
  { bgCategory = "", bgPartName = "", bgThumbnail = Nothing, bgDefault = False, bgAddIds = [] }

-- | Bind option for a category
data BindOptionType = BindMustSelect | BindMultiple
  deriving ( Show, Eq )

data BindOption = BindOption { boCategory :: Text, boOptions :: [ BindOptionType ] }
  deriving ( Show, Eq )

-- | Menu item (either animation ID or separator)
data MenuItem
  = MenuItemBind Int           -- ^ Animation ID for bind
  | MenuItemSeparator          -- ^ Separator line "-"
  deriving ( Show, Eq )

-- | Extended menu item with hierarchy
data MenuItemEx
  = MenuItemEx { mieMenuName :: Text        -- ^ Menu hierarchy name
               , mieItem     :: MenuItem    -- ^ The menu item
               }
  deriving ( Show, Eq )

-- | Per-character settings (sakura=0, kero=1, char2, char3, ...)
data CharacterSettings
  = CharacterSettings
  { csName :: Maybe Text    -- ^ Character name override
  , csName2 :: Maybe Text    -- ^ Nickname (sakura only)
  , csSerikoAlignmentToDesktop :: Maybe Text   -- ^ Surface alignment (top/bottom/free)
  , csDefaultX :: Maybe Int     -- ^ Image base X coordinate
  , csDefaultY :: Maybe Int     -- ^ Image base Y coordinate
  , csDefaultLeft :: Maybe Int     -- ^ Display X coordinate
  , csDefaultTop :: Maybe Int     -- ^ Display Y coordinate
  , csBalloonOffsetX :: Maybe Int     -- ^ Balloon X offset
  , csBalloonOffsetY :: Maybe Int     -- ^ Balloon Y offset
  , csBalloonOffsetXL :: Maybe Int     -- ^ Balloon X offset (left side)
  , csBalloonOffsetXR :: Maybe Int     -- ^ Balloon X offset (right side)
  , csBalloonOffsetYL :: Maybe Int     -- ^ Balloon Y offset (left side)
  , csBalloonOffsetYR :: Maybe Int     -- ^ Balloon Y offset (right side)
  , csBalloonAlignment :: Maybe Text    -- ^ Balloon alignment (none/left/right)
  , csBalloonDontMove :: Maybe Bool    -- ^ Restrict balloon movement
  , csBalloonSyncScale :: Maybe Bool    -- ^ Sync balloon scale with ghost
  , csBindGroups :: Map Int BindGroup   -- ^ Costume bindings by animation ID
  , csBindOptions :: [ BindOption ]      -- ^ Category options
  , csMenuItems :: [ MenuItem ]        -- ^ Menu items (by order)
  , csMenuItemsEx :: [ MenuItemEx ]      -- ^ Extended menu items with hierarchy
  , csMenu :: Maybe Text          -- ^ Menu display (auto/hidden)
  }
  deriving ( Show, Eq )

-- | Empty character settings
emptyCharacterSettings :: CharacterSettings
emptyCharacterSettings
  = CharacterSettings
  { csName = Nothing
  , csName2 = Nothing
  , csSerikoAlignmentToDesktop = Nothing
  , csDefaultX = Nothing
  , csDefaultY = Nothing
  , csDefaultLeft = Nothing
  , csDefaultTop = Nothing
  , csBalloonOffsetX = Nothing
  , csBalloonOffsetY = Nothing
  , csBalloonOffsetXL = Nothing
  , csBalloonOffsetXR = Nothing
  , csBalloonOffsetYL = Nothing
  , csBalloonOffsetYR = Nothing
  , csBalloonAlignment = Nothing
  , csBalloonDontMove = Nothing
  , csBalloonSyncScale = Nothing
  , csBindGroups = Map.empty
  , csBindOptions = []
  , csMenuItems = []
  , csMenuItemsEx = []
  , csMenu = Nothing
  }

data ShellDescript
  = ShellDescript
  { -- | Character encoding. Default: OS standard or SSP default.
    shellDescriptCharset :: Text
    -- | Shell name. Required.
  , shellDescriptName :: Text
    -- | Shell ID (alphanumeric). Optional.
  , shellDescriptId :: Maybe Text
    -- | File set type, for shell it's "shell". Required.
  , shellDescriptType :: Text
    -- | Author name (alphanumeric).
  , shellDescriptCraftman :: Maybe Text
    -- | Author name.
  , shellDescriptCraftmanw :: Maybe Text
    -- | Author URL.
  , shellDescriptCraftmanUrl :: Maybe Text
    -- | Network update URL.
  , shellDescriptHomeUrl :: Maybe Text
    -- | Readme filename. Default: readme.txt
  , shellDescriptReadme :: Text
    -- | Readme charset.
  , shellDescriptReadmeCharset :: Maybe Text
    -- | Hide shell from menu if "hidden".
  , shellDescriptMenu :: Maybe Text
    -- | Z-order for scopes. Comma-separated scope IDs.
  , shellDescriptSerikoZorder :: Maybe Text
    -- | Sticky window scopes. Comma-separated scope IDs.
  , shellDescriptSerikoStickyWindow :: Maybe Text
    -- | Recommended DPI. Default: 96.
  , shellDescriptSerikoDpi :: Int
    -- | Surface alignment to desktop (top/bottom/free). Global default.
  , shellDescriptSerikoAlignmentToDesktop :: Maybe Text
    -- | Per-character settings. Key: 0=sakura, 1=kero, 2+=char*
  , shellDescriptCharacters :: Map Int CharacterSettings
    -- | Menu font name.
  , shellDescriptMenuFontName :: Maybe Text
    -- | Menu font height.
  , shellDescriptMenuFontHeight :: Maybe Int
    -- | Menu background bitmap filename.
  , shellDescriptMenuBackgroundBitmapFilename :: Maybe Text
    -- | Menu foreground bitmap filename.
  , shellDescriptMenuForegroundBitmapFilename :: Maybe Text
    -- | Menu sidebar bitmap filename.
  , shellDescriptMenuSidebarBitmapFilename :: Maybe Text
    -- | Menu background font color (R).
  , shellDescriptMenuBackgroundFontColorR :: Maybe Int
    -- | Menu background font color (G).
  , shellDescriptMenuBackgroundFontColorG :: Maybe Int
    -- | Menu background font color (B).
  , shellDescriptMenuBackgroundFontColorB :: Maybe Int
    -- | Menu foreground font color (R).
  , shellDescriptMenuForegroundFontColorR :: Maybe Int
    -- | Menu foreground font color (G).
  , shellDescriptMenuForegroundFontColorG :: Maybe Int
    -- | Menu foreground font color (B).
  , shellDescriptMenuForegroundFontColorB :: Maybe Int
    -- | Menu separator color (R).
  , shellDescriptMenuSeparatorColorR :: Maybe Int
    -- | Menu separator color (G).
  , shellDescriptMenuSeparatorColorG :: Maybe Int
    -- | Menu separator color (B).
  , shellDescriptMenuSeparatorColorB :: Maybe Int
    -- | Menu frame color (R). Default: 0.
  , shellDescriptMenuFrameColorR :: Int
    -- | Menu frame color (G). Default: 0.
  , shellDescriptMenuFrameColorG :: Int
    -- | Menu frame color (B). Default: 0.
  , shellDescriptMenuFrameColorB :: Int
    -- | Menu disabled font color (R).
  , shellDescriptMenuDisableFontColorR :: Maybe Int
    -- | Menu disabled font color (G).
  , shellDescriptMenuDisableFontColorG :: Maybe Int
    -- | Menu disabled font color (B).
  , shellDescriptMenuDisableFontColorB :: Maybe Int
    -- | Menu background alignment. Default: lefttop.
  , shellDescriptMenuBackgroundAlignment :: Text
    -- | Menu foreground alignment. Default: lefttop.
  , shellDescriptMenuForegroundAlignment :: Text
    -- | Menu sidebar alignment. Default: bottom.
  , shellDescriptMenuSidebarAlignment :: Text
    -- | Paint transparent region black. 0 or 1.
  , shellDescriptSerikoPaintTransparentRegionBlack :: Maybe Int
    -- | Use image's own alpha channel. Default: 0.
  , shellDescriptSerikoUseSelfAlpha :: Int
  }
  deriving ( Show, Eq )

emptyShellDescript :: ShellDescript
emptyShellDescript
  = ShellDescript
  { shellDescriptCharset = ""
  , shellDescriptName = ""
  , shellDescriptId = Nothing
  , shellDescriptType = ""
  , shellDescriptCraftman = Nothing
  , shellDescriptCraftmanw = Nothing
  , shellDescriptCraftmanUrl = Nothing
  , shellDescriptHomeUrl = Nothing
  , shellDescriptReadme = "readme.txt"
  , shellDescriptReadmeCharset = Nothing
  , shellDescriptMenu = Nothing
  , shellDescriptSerikoZorder = Nothing
  , shellDescriptSerikoStickyWindow = Nothing
  , shellDescriptSerikoDpi = 96
  , shellDescriptSerikoAlignmentToDesktop = Nothing
  , shellDescriptCharacters = Map.empty
  , shellDescriptMenuFontName = Nothing
  , shellDescriptMenuFontHeight = Nothing
  , shellDescriptMenuBackgroundBitmapFilename = Nothing
  , shellDescriptMenuForegroundBitmapFilename = Nothing
  , shellDescriptMenuSidebarBitmapFilename = Nothing
  , shellDescriptMenuBackgroundFontColorR = Nothing
  , shellDescriptMenuBackgroundFontColorG = Nothing
  , shellDescriptMenuBackgroundFontColorB = Nothing
  , shellDescriptMenuForegroundFontColorR = Nothing
  , shellDescriptMenuForegroundFontColorG = Nothing
  , shellDescriptMenuForegroundFontColorB = Nothing
  , shellDescriptMenuSeparatorColorR = Nothing
  , shellDescriptMenuSeparatorColorG = Nothing
  , shellDescriptMenuSeparatorColorB = Nothing
  , shellDescriptMenuFrameColorR = 0
  , shellDescriptMenuFrameColorG = 0
  , shellDescriptMenuFrameColorB = 0
  , shellDescriptMenuDisableFontColorR = Nothing
  , shellDescriptMenuDisableFontColorG = Nothing
  , shellDescriptMenuDisableFontColorB = Nothing
  , shellDescriptMenuBackgroundAlignment = "lefttop"
  , shellDescriptMenuForegroundAlignment = "lefttop"
  , shellDescriptMenuSidebarAlignment = "bottom"
  , shellDescriptSerikoPaintTransparentRegionBlack = Nothing
  , shellDescriptSerikoUseSelfAlpha = 0
  }

-- | Get or create character settings for a scope index
getCharSettings :: Int -> ShellDescript -> CharacterSettings
getCharSettings idx desc
  = Map.findWithDefault emptyCharacterSettings idx (shellDescriptCharacters desc)

-- | Update character settings for a scope index
updateCharSettings
  :: Int -> (CharacterSettings -> CharacterSettings) -> ShellDescript -> ShellDescript
updateCharSettings idx f desc
  = let
      chars   = shellDescriptCharacters desc
      current = Map.findWithDefault emptyCharacterSettings idx chars
      updated = f current
    in 
      desc { shellDescriptCharacters = Map.insert idx updated chars }

readShellDescript :: FilePath -> IO ShellDescript
readShellDescript path = do
  rawBytes <- BL.readFile path
  let detectedCharset = detectCharsetFromBytes rawBytes
      utf8Bytes       = convertToUtf8 detectedCharset rawBytes
      contents        = TE.decodeUtf8 (BL.toStrict utf8Bytes)
  return $ foldl' parseLine emptyShellDescript (T.lines contents)
  where
    -- Parse "char<N>." prefix, returns (scope index, rest of key)
    parseCharPrefix :: Text -> Maybe ( Int, Text )
    parseCharPrefix key
      | Just rest <- T.stripPrefix "sakura." key = Just ( 0, rest )
      | Just rest <- T.stripPrefix "kero." key = Just ( 1, rest )
      | Just rest <- T.stripPrefix "char" key = case T.breakOn "." rest of
        ( numPart, dotRest )
          | not (T.null dotRest) -> case readMaybe (T.unpack numPart) of
            Just n  -> Just ( n, T.drop 1 dotRest )  -- drop the dot
            Nothing -> Nothing
        _ -> Nothing
      | otherwise = Nothing

    -- Parse indexed key like "bindgroup0.name" -> (0, "name")
    parseIndexedKey :: Text -> Text -> Maybe ( Int, Text )
    parseIndexedKey prefix key
      | Just rest <- T.stripPrefix prefix key = case T.breakOn "." rest of
        ( numPart, dotRest )
          | not (T.null dotRest) -> case readMaybe (T.unpack numPart) of
            Just n  -> Just ( n, T.drop 1 dotRest )
            Nothing -> Nothing
        ( numPart, "" )     -- No dot, just the index (e.g., "menuitem0")
          -> case readMaybe (T.unpack numPart) of
            Just n  -> Just ( n, "" )
            Nothing -> Nothing
        _ -> Nothing
      | otherwise = Nothing

    -- Update a bind group in character settings
    updateBindGroup :: Int -> (BindGroup -> BindGroup) -> CharacterSettings -> CharacterSettings
    updateBindGroup bgAnimId f cs
      = let
          groups  = csBindGroups cs
          current = Map.findWithDefault emptyBindGroup bgAnimId groups
          updated = f current
        in 
          cs { csBindGroups = Map.insert bgAnimId updated groups }

    -- Parse bind option types from "+" separated string
    parseBindOptions :: Text -> [ BindOptionType ]
    parseBindOptions val
      = let
          parts = T.splitOn "+" val
        in 
          concatMap parseOne parts
      where
        parseOne t = case T.toLower (T.strip t) of
          "mustselect" -> [ BindMustSelect ]
          "multiple" -> [ BindMultiple ]
          _ -> []

    -- Parse menu item value
    parseMenuItem :: Text -> MenuItem
    parseMenuItem val
      | val == "-" = MenuItemSeparator
      | otherwise = maybe MenuItemSeparator MenuItemBind (readMaybe (T.unpack val))  -- fallback

    parseLine desc line = case T.breakOn "," line of
      ( rawKey, rest )
        | not (T.null rest) -> let
            key    = T.toLower (clean rawKey)
            rawVal = T.drop 1 rest  -- drop the comma
            val    = clean rawVal
          in 
            case key of
              -- Global settings
              "charset" -> desc { shellDescriptCharset = val }
              "name" -> desc { shellDescriptName = val }
              "id" -> desc { shellDescriptId = Just val }
              "type" -> desc { shellDescriptType = val }
              "craftman" -> desc { shellDescriptCraftman = Just val }
              "craftmanw" -> desc { shellDescriptCraftmanw = Just val }
              "craftmanurl" -> desc { shellDescriptCraftmanUrl = Just val }
              "homeurl" -> desc { shellDescriptHomeUrl = Just val }
              "readme" -> desc { shellDescriptReadme = val }
              "readme.charset" -> desc { shellDescriptReadmeCharset = Just val }
              "menu" -> desc { shellDescriptMenu = Just val }

              "seriko.zorder" -> desc { shellDescriptSerikoZorder = Just val }
              "seriko.sticky-window" -> desc { shellDescriptSerikoStickyWindow = Just val }
              "seriko.dpi" -> desc { shellDescriptSerikoDpi = readIntOr 96 val }
              "seriko.alignmenttodesktop" -> desc
                { shellDescriptSerikoAlignmentToDesktop = Just val }

              "menu.font.name" -> desc { shellDescriptMenuFontName = Just val }
              "menu.font.height" -> desc { shellDescriptMenuFontHeight = readMaybeInt val }
              "menu.background.bitmap.filename" -> desc
                { shellDescriptMenuBackgroundBitmapFilename = Just val }
              "menu.foreground.bitmap.filename" -> desc
                { shellDescriptMenuForegroundBitmapFilename = Just val }
              "menu.sidebar.bitmap.filename" -> desc
                { shellDescriptMenuSidebarBitmapFilename = Just val }
              "menu.background.font.color.r" -> desc
                { shellDescriptMenuBackgroundFontColorR = readMaybeInt val }
              "menu.background.font.color.g" -> desc
                { shellDescriptMenuBackgroundFontColorG = readMaybeInt val }
              "menu.background.font.color.b" -> desc
                { shellDescriptMenuBackgroundFontColorB = readMaybeInt val }
              "menu.foreground.font.color.r" -> desc
                { shellDescriptMenuForegroundFontColorR = readMaybeInt val }
              "menu.foreground.font.color.g" -> desc
                { shellDescriptMenuForegroundFontColorG = readMaybeInt val }
              "menu.foreground.font.color.b" -> desc
                { shellDescriptMenuForegroundFontColorB = readMaybeInt val }
              "menu.separator.color.r" -> desc
                { shellDescriptMenuSeparatorColorR = readMaybeInt val }
              "menu.separator.color.g" -> desc
                { shellDescriptMenuSeparatorColorG = readMaybeInt val }
              "menu.separator.color.b" -> desc
                { shellDescriptMenuSeparatorColorB = readMaybeInt val }
              "menu.frame.color.r" -> desc { shellDescriptMenuFrameColorR = readIntOr 0 val }
              "menu.frame.color.g" -> desc { shellDescriptMenuFrameColorG = readIntOr 0 val }
              "menu.frame.color.b" -> desc { shellDescriptMenuFrameColorB = readIntOr 0 val }
              "menu.disable.font.color.r" -> desc
                { shellDescriptMenuDisableFontColorR = readMaybeInt val }
              "menu.disable.font.color.g" -> desc
                { shellDescriptMenuDisableFontColorG = readMaybeInt val }
              "menu.disable.font.color.b" -> desc
                { shellDescriptMenuDisableFontColorB = readMaybeInt val }
              "menu.background.alignment" -> desc { shellDescriptMenuBackgroundAlignment = val }
              "menu.foreground.alignment" -> desc { shellDescriptMenuForegroundAlignment = val }
              "menu.sidebar.alignment" -> desc { shellDescriptMenuSidebarAlignment = val }

              "seriko.paint_transparent_region_black" -> desc
                { shellDescriptSerikoPaintTransparentRegionBlack = readMaybeInt val }
              "seriko.use_self_alpha" -> desc { shellDescriptSerikoUseSelfAlpha = readIntOr 0 val }

              -- Per-character settings
              _ -> case parseCharPrefix key of
                Just ( scopeIdx, restKey ) -> parseCharacterKey scopeIdx restKey val rawVal desc
                Nothing -> desc
      _ -> desc

    -- Parse character-specific keys
    parseCharacterKey :: Int -> Text -> Text -> Text -> ShellDescript -> ShellDescript
    parseCharacterKey idx restKey val rawVal desc = case restKey of
      "name" -> updateCharSettings idx (\cs -> cs { csName = Just val }) desc
      "name2" -> updateCharSettings idx (\cs -> cs { csName2 = Just val }) desc
      "seriko.alignmenttodesktop"
        -> updateCharSettings idx (\cs -> cs { csSerikoAlignmentToDesktop = Just val }) desc
      "defaultx" -> updateCharSettings idx (\cs -> cs { csDefaultX = readMaybeInt val }) desc
      "defaulty" -> updateCharSettings idx (\cs -> cs { csDefaultY = readMaybeInt val }) desc
      "defaultleft" -> updateCharSettings idx (\cs -> cs { csDefaultLeft = readMaybeInt val }) desc
      "defaulttop" -> updateCharSettings idx (\cs -> cs { csDefaultTop = readMaybeInt val }) desc
      "balloon.offsetx"
        -> updateCharSettings idx (\cs -> cs { csBalloonOffsetX = readMaybeInt val }) desc
      "balloon.offsety"
        -> updateCharSettings idx (\cs -> cs { csBalloonOffsetY = readMaybeInt val }) desc
      "balloon.offsetxl"
        -> updateCharSettings idx (\cs -> cs { csBalloonOffsetXL = readMaybeInt val }) desc
      "balloon.offsetxr"
        -> updateCharSettings idx (\cs -> cs { csBalloonOffsetXR = readMaybeInt val }) desc
      "balloon.offsetyl"
        -> updateCharSettings idx (\cs -> cs { csBalloonOffsetYL = readMaybeInt val }) desc
      "balloon.offsetyr"
        -> updateCharSettings idx (\cs -> cs { csBalloonOffsetYR = readMaybeInt val }) desc
      "balloon.alignment"
        -> updateCharSettings idx (\cs -> cs { csBalloonAlignment = Just val }) desc
      "balloon.dontmove"
        -> updateCharSettings idx (\cs -> cs { csBalloonDontMove = readMaybeBool val }) desc
      "balloon.syncscale"
        -> updateCharSettings idx (\cs -> cs { csBalloonSyncScale = readMaybeBool val }) desc
      "menu" -> updateCharSettings idx (\cs -> cs { csMenu = Just val }) desc

      -- Bind group settings: bindgroup<N>.name, bindgroup<N>.default, bindgroup<N>.addid
      _
        | Just ( bgAnimId, subKey ) <- parseIndexedKey "bindgroup" restKey -> case subKey of
          "name"    ->
            -- Parse "category,partname,thumbnail" or "category,partname"
            case T.splitOn "," rawVal of
              (cat : pname : thumb : _) ->
                updateCharSettings idx (updateBindGroup bgAnimId (\bg -> bg
                  { bgCategory  = clean cat
                  , bgPartName  = clean pname
                  , bgThumbnail = Just (clean thumb)
                  })) desc
              (cat : pname : _) ->
                updateCharSettings idx (updateBindGroup bgAnimId (\bg -> bg
                  { bgCategory  = clean cat
                  , bgPartName  = clean pname
                  , bgThumbnail = Nothing
                  })) desc
              (cat : _) ->
                updateCharSettings idx (updateBindGroup bgAnimId (\bg -> bg
                  { bgCategory  = clean cat
                  , bgPartName  = ""
                  , bgThumbnail = Nothing
                  })) desc
              [] -> desc
          "default" -> let
              isDefault = val == "1" || T.toLower val == "true"
            in 
              updateCharSettings idx (updateBindGroup bgAnimId (\bg -> bg
                                                                { bgDefault = isDefault })) desc
          "addid"   -> let
              ids = mapMaybe (readMaybe . T.unpack . T.strip) (T.splitOn "," val)
            in 
              updateCharSettings idx (updateBindGroup bgAnimId (\bg -> bg { bgAddIds = ids })) desc
          _         -> desc

      -- Bind option settings: bindoption<N>.group
      _
        | Just ( _, subKey ) <- parseIndexedKey "bindoption" restKey -> case subKey of
          "group" ->
            -- Parse "category,options" where options is "+" separated
            case T.splitOn "," rawVal of
              (cat : optPart : _) -> let
                  opts = parseBindOptions optPart
                  opt  = BindOption (clean cat) opts
                in 
                  updateCharSettings idx (\cs -> cs
                                          { csBindOptions = csBindOptions cs ++ [ opt ] }) desc
              _ -> desc
          _       -> desc

      -- Menu item settings: menuitem<N>
      _
        | Just ( _, "" ) <- parseIndexedKey "menuitem" restKey -> let
            item = parseMenuItem val
          in 
            updateCharSettings idx (\cs -> cs { csMenuItems = csMenuItems cs ++ [ item ] }) desc

      -- Extended menu item settings: menuitemex<N>
      _
        | Just ( _, "" ) <- parseIndexedKey "menuitemex" restKey ->
          -- Parse "menuname,id"
          case T.splitOn "," rawVal of
            (menuName : idPart : _) -> let
                item   = parseMenuItem (clean idPart)
                itemEx = MenuItemEx (clean menuName) item
              in 
                updateCharSettings idx (\cs -> cs
                                        { csMenuItemsEx = csMenuItemsEx cs ++ [ itemEx ] }) desc
            _ -> desc

      _ -> desc

-- | Drawing method for element/animation pattern composition
data DrawMethod
  = DrawBase                      -- ^ Replace base surface entirely
  | DrawOverlay                   -- ^ Simple overlay on base
  | DrawOverlayfast               -- ^ Overlay based on base opacity
  | DrawOverlaymultiply           -- ^ Multiply blend based on base opacity
  | DrawReplace                   -- ^ Replace including transparent regions
  | DrawInterpolate               -- ^ Overlay based on base transparency (inverse of overlayfast)
  | DrawAsis                      -- ^ Overlay ignoring transparency
  | DrawMove                      -- ^ Move base surface position
  | DrawBind                      -- ^ Costume/clothing overlay (same as overlay)
  | DrawAdd                       -- ^ Costume part addition (same as overlay)
  | DrawReduce                    -- ^ Transparency multiplication (cut-out effect)
  | DrawInsert [ Int ]            -- ^ Insert another bind animation group
  | DrawStart [ Int ]             -- ^ Start specified animation(s)
  | DrawStop [ Int ]              -- ^ Stop specified animation(s)
  | DrawAlternativeStart [ Int ]  -- ^ Start one random animation from list
  | DrawAlternativeStop [ Int ]   -- ^ Stop one random animation from list
  | DrawParallelStart [ Int ]     -- ^ Start all animations in list
  | DrawParallelStop [ Int ]      -- ^ Stop all animations in list
  deriving ( Show, Eq )

-- | Animation interval/trigger types
data AnimationInterval
  = IntervalNever              -- ^ Never auto-execute (triggered by start/alternativestart)
  | IntervalSometimes          -- ^ 1/2 probability per second
  | IntervalRarely             -- ^ 1/4 probability per second
  | IntervalRandom Int         -- ^ 1/N probability per second
  | IntervalPeriodic Int       -- ^ Execute every N seconds
  | IntervalAlways             -- ^ Loop continuously
  | IntervalRunonce            -- ^ Execute once on surface change
  | IntervalYenE               -- ^ Execute on \e tag
  | IntervalTalk Int           -- ^ Execute every N characters of text
  | IntervalBind               -- ^ Costume/clothing definition
  | IntervalCombined [ AnimationInterval ]  -- ^ SSP: Combined intervals (e.g., bind+runonce)
  deriving ( Show, Eq )

-- | Animation option flags
data AnimationOption
  = OptionExclusive (Maybe [ Int ])  -- ^ Exclusive execution; optional list for limited exclusivity
  | OptionBackground                 -- ^ Render behind base surface
  | OptionSharedIndex                -- ^ Continue animation across surface changes
  deriving ( Show, Eq )

-- | Collision shape types for collisionex
data CollisionShape
  = CollisionRect Int Int Int Int              -- ^ Rectangle: x1, y1, x2, y2
  | CollisionEllipse Int Int Int Int           -- ^ Ellipse bounded by rect: x1, y1, x2, y2
  | CollisionCircle Int Int Int                -- ^ Circle: centerX, centerY, radius
  | CollisionPolygon [ ( Int, Int ) ]          -- ^ Polygon: list of (x, y) vertices
  | CollisionRegionFile Text Int Int Int Bool  -- ^ Region from image file: filename, R, G, B, invert
  deriving ( Show, Eq )

-- | Clickable collision region
data CollisionRegion
  = CollisionRegion { crIndex :: Int, crName :: Text, crShape :: CollisionShape }
  deriving ( Show, Eq )

-- | Element overlay/base instruction
data Element
  = Element
  { elemIndex :: Int, elemMethod :: DrawMethod, elemFile :: Text, elemX :: Int, elemY :: Int }
  deriving ( Show, Eq )

-- | Animation pattern step
data AnimationPattern
  = AnimationPattern
  { apIndex     :: Int
  , apMethod    :: DrawMethod
  , apSurfaceId :: Int           -- ^ -1 = stop this animation, -2 = stop all animations
  , apWait      :: Int           -- ^ Milliseconds (or min-max range for SSP)
  , apWaitMax   :: Maybe Int     -- ^ Optional max wait for random range (SSP)
  , apX         :: Int
  , apY         :: Int
  }
  deriving ( Show, Eq )

-- | Animation sequence
data Animation
  = Animation { animId         :: Int
              , animInterval   :: AnimationInterval
              , animOptions    :: [ AnimationOption ]
              , animPatterns   :: [ AnimationPattern ]
              , animCollisions :: [ CollisionRegion ]  -- ^ Animation-specific collisions
              }
  deriving ( Show, Eq )

-- | Surface definition from surfaces.txt
data SurfaceDefinition
  = SurfaceDefinition
  { sdId :: Int
  , sdElements :: [ Element ]
  , sdAnimations :: [ Animation ]
  , sdCollisions :: [ CollisionRegion ]
    -- Balloon offsets for this surface
  , sdSakuraBalloonOffsetX :: Maybe Int
  , sdSakuraBalloonOffsetY :: Maybe Int
  , sdKeroBalloonOffsetX :: Maybe Int
  , sdKeroBalloonOffsetY :: Maybe Int
  , sdBalloonOffsetX :: Maybe Int  -- ^ Generic (applies to any scope)
  , sdBalloonOffsetY :: Maybe Int
    -- Center/position points
  , sdPointCenterX :: Maybe Int  -- ^ Surface center X
  , sdPointCenterY :: Maybe Int  -- ^ Surface center Y
  , sdPointKinokoCenterX :: Maybe Int  -- ^ Mushroom growth point X
  , sdPointKinokoCenterY :: Maybe Int  -- ^ Mushroom growth point Y
  , sdPointBaseposX :: Maybe Int  -- ^ Window positioning base X
  , sdPointBaseposY :: Maybe Int  -- ^ Window positioning base Y
  }
  deriving ( Show, Eq )

-- | Empty surface definition with defaults
emptySurfaceDefinition :: Int -> SurfaceDefinition
emptySurfaceDefinition sid
  = SurfaceDefinition
  { sdId = sid
  , sdElements = []
  , sdAnimations = []
  , sdCollisions = []
  , sdSakuraBalloonOffsetX = Nothing
  , sdSakuraBalloonOffsetY = Nothing
  , sdKeroBalloonOffsetX = Nothing
  , sdKeroBalloonOffsetY = Nothing
  , sdBalloonOffsetX = Nothing
  , sdBalloonOffsetY = Nothing
  , sdPointCenterX = Nothing
  , sdPointCenterY = Nothing
  , sdPointKinokoCenterX = Nothing
  , sdPointKinokoCenterY = Nothing
  , sdPointBaseposX = Nothing
  , sdPointBaseposY = Nothing
  }

-- | A loaded surface image ready for display (runtime)
data Surface
  = Surface
  { surfaceId :: Int, surfaceImagePath :: FilePath, surfaceWidth :: Int, surfaceHeight :: Int }
  deriving ( Show, Eq )

-- | Sort order for collisions and animations
data SortOrder = SortNone | SortAscend | SortDescend
  deriving ( Show, Eq )

-- | Descript brace settings in surfaces.txt
data SurfacesDescript
  = SurfacesDescript
  { surfDescVersion       :: Int        -- ^ SERIKO version: 0 = old format, 1 = new format
  , surfDescMaxWidth      :: Maybe Int  -- ^ Maximum surface width (auto-detected in SSP)
  , surfDescCollisionSort :: SortOrder  -- ^ Collision evaluation order
  , surfDescAnimationSort :: SortOrder  -- ^ Animation layer order
  }
  deriving ( Show, Eq )

-- | Default surfaces descript settings
emptySurfacesDescript :: SurfacesDescript
emptySurfacesDescript
  = SurfacesDescript { surfDescVersion       = 1
                     , surfDescMaxWidth      = Nothing
                     , surfDescCollisionSort = SortNone
                     , surfDescAnimationSort = SortDescend
                     }

-- | Surface alias definition
data SurfaceAlias
  = SurfaceAlias { saName       :: Text      -- ^ Alias name (e.g., "素", "照れ")
                 , saSurfaceIds :: [ Int ]   -- ^ Surface IDs (random selection if multiple)
                 }
  deriving ( Show, Eq )

-- | Cursor definition for a collision region
data CursorDef
  = CursorDef { cdCollisionId :: Text       -- ^ Collision region name
              , cdCursorFile  :: Text       -- ^ Cursor file or system:* name
              }
  deriving ( Show, Eq )

-- | Cursor definitions for a scope (sakura/kero/char*)
data ScopeCursors
  = ScopeCursors { scMouseUp        :: [ CursorDef ]
                 , scMouseDown      :: [ CursorDef ]
                 , scMouseRightDown :: [ CursorDef ]
                 , scMouseWheel     :: [ CursorDef ]
                 , scMouseHover     :: [ CursorDef ]
                 }
  deriving ( Show, Eq )

-- | Empty cursor definitions
emptyScopeCursors :: ScopeCursors
emptyScopeCursors = ScopeCursors [] [] [] [] []

-- | Tooltip definition
data TooltipDef
  = TooltipDef { tdCollisionId :: Text  -- ^ Collision region name
               , tdText        :: Text  -- ^ Tooltip text content
               }
  deriving ( Show, Eq )

-- | Complete surfaces.txt data
data Surfaces
  = Surfaces
  { surfacesCharset       :: Text                           -- ^ Character encoding
  , surfacesDescript      :: SurfacesDescript               -- ^ descript brace settings
  , surfaceDefinitions    :: [ SurfaceDefinition ]          -- ^ All surface definitions
  , surfaceSakuraAlias    :: [ SurfaceAlias ]               -- ^ sakura.surface.alias
  , surfaceKeroAlias      :: [ SurfaceAlias ]               -- ^ kero.surface.alias
  , surfaceCharAliases    :: [ ( Int, [ SurfaceAlias ] ) ]  -- ^ char*.surface.alias (scope index, aliases)
  , surfaceSakuraCursor   :: ScopeCursors                   -- ^ sakura.cursor
  , surfaceKeroCursor     :: ScopeCursors                   -- ^ kero.cursor
  , surfaceCharCursors    :: [ ( Int, ScopeCursors ) ]      -- ^ char*.cursor
  , surfaceSakuraTooltips :: [ TooltipDef ]               -- ^ sakura.tooltips
  , surfaceKeroTooltips   :: [ TooltipDef ]               -- ^ kero.tooltips
  , surfaceCharTooltips   :: [ ( Int, [ TooltipDef ] ) ]  -- ^ char*.tooltips
  }
  deriving ( Show, Eq )

-- | Empty surfaces with defaults
emptySurfaces :: Surfaces
emptySurfaces
  = Surfaces
  { surfacesCharset       = ""
  , surfacesDescript      = emptySurfacesDescript
  , surfaceDefinitions    = []
  , surfaceSakuraAlias    = []
  , surfaceKeroAlias      = []
  , surfaceCharAliases    = []
  , surfaceSakuraCursor   = emptyScopeCursors
  , surfaceKeroCursor     = emptyScopeCursors
  , surfaceCharCursors    = []
  , surfaceSakuraTooltips = []
  , surfaceKeroTooltips   = []
  , surfaceCharTooltips   = []
  }

-- | A brace block from surfaces.txt
data BraceBlock
  = BraceBlock
  { bbName  :: Text      -- ^ Brace name (e.g., "surface0", "descript", "sakura.surface.alias")
  , bbLines :: [ Text ]  -- ^ Lines inside the brace
  }
  deriving ( Show, Eq )

-- | Tokenize surfaces.txt into brace blocks
-- Format: name\n{\nlines...\n}
tokenizeBraces :: Text -> [ BraceBlock ]
tokenizeBraces contents = go Nothing [] (filter (not . isCommentOrEmpty) (T.lines contents))
  where
    isCommentOrEmpty line
      = let
          stripped = T.strip line
        in 
          T.null stripped || "//" `T.isPrefixOf` stripped

    go :: Maybe Text -> [ Text ] -> [ Text ] -> [ BraceBlock ]
    go Nothing _ [] = []
    go (Just name) acc [] = [ BraceBlock name (reverse acc) ]  -- Unclosed brace (shouldn't happen)
    go Nothing _ (line : rest)
      | "{" `T.isSuffixOf` T.strip line
        =
        -- Name might be on same line as {
        let
            name = T.strip (T.dropEnd 1 (T.strip line))
          in 
            go (Just name) [] rest
      | otherwise
        =
        -- Line before { is the brace name
        go (Just (T.strip line)) [] rest
    go (Just name) acc (line : rest)
      | T.strip line == "}" = BraceBlock name (reverse acc) : go Nothing [] rest
      | T.strip line == "{"
        =
        -- Opening brace on separate line, continue
        go (Just name) acc rest
      | otherwise = go (Just name) (line : acc) rest

-- | Parse draw method from text
parseDrawMethod :: Text -> DrawMethod
parseDrawMethod txt
  = let
      ( method, rest ) = T.breakOn "," (T.toLower (T.strip txt))
      ids
        = if T.null rest
          then []
          else mapMaybe (readMaybe . T.unpack . T.strip) (T.splitOn "," (T.drop 1 rest))
    in 
      case method of
        "base" -> DrawBase
        "overlay" -> DrawOverlay
        "overlayfast" -> DrawOverlayfast
        "overlaymultiply" -> DrawOverlaymultiply
        "replace" -> DrawReplace
        "interpolate" -> DrawInterpolate
        "asis" -> DrawAsis
        "move" -> DrawMove
        "bind" -> DrawBind
        "add" -> DrawAdd
        "reduce" -> DrawReduce
        "insert" -> DrawInsert ids
        "start" -> DrawStart ids
        "stop" -> DrawStop ids
        "alternativestart" -> DrawAlternativeStart ids
        "alternativestop" -> DrawAlternativeStop ids
        "parallelstart" -> DrawParallelStart ids
        "parallelstop" -> DrawParallelStop ids
        _ -> DrawOverlay  -- Default fallback

-- | Parse animation interval from text
-- Handles: never, sometimes, rarely, random,N, periodic,N, always, runonce, yen-e, talk,N, bind
-- Also handles combined intervals: bind+runonce, bind+random,5
parseAnimationInterval :: Text -> AnimationInterval
parseAnimationInterval txt
  = let
      stripped = T.toLower (T.strip txt)
    in 
      if "+" `T.isInfixOf` stripped
        then IntervalCombined (map parseAnimationInterval (T.splitOn "+" stripped))
        else parseSingleInterval stripped
  where
    parseSingleInterval :: Text -> AnimationInterval
    parseSingleInterval t = case T.breakOn "," t of
      ( "never", _ ) -> IntervalNever
      ( "sometimes", _ ) -> IntervalSometimes
      ( "rarely", _ ) -> IntervalRarely
      ( "always", _ ) -> IntervalAlways
      ( "runonce", _ ) -> IntervalRunonce
      ( "yen-e", _ ) -> IntervalYenE
      ( "bind", _ ) -> IntervalBind
      ( "random", rest ) -> IntervalRandom (readIntOr 2 (T.drop 1 rest))
      ( "periodic", rest ) -> IntervalPeriodic (readIntOr 1 (T.drop 1 rest))
      ( "talk", rest ) -> IntervalTalk (readIntOr 1 (T.drop 1 rest))
      _ -> IntervalNever  -- Unknown, treat as never

-- | Parse sort order
parseSortOrder :: Text -> SortOrder
parseSortOrder txt = case T.toLower (T.strip txt) of
  "ascend"  -> SortAscend
  "descend" -> SortDescend
  _         -> SortNone

-- | Parse animation option
parseAnimationOption :: Text -> Maybe AnimationOption
parseAnimationOption txt
  = let
      ( opt, rest ) = T.breakOn "," (T.toLower (T.strip txt))
      ids
        = if T.null rest
          then Nothing
          else Just (mapMaybe (readMaybe . T.unpack . T.strip) (T.splitOn "," (T.drop 1 rest)))
    in 
      case opt of
        "exclusive" -> Just (OptionExclusive ids)
        "background" -> Just OptionBackground
        "shared-index" -> Just OptionSharedIndex
        _ -> Nothing

-- | Parse surface IDs from surface brace name
-- Handles: surface0, surface1,3,5, surface1-10, surface1-10,!5,!7-8
-- Also handles: surface.append0, surface.append1-10
parseSurfaceIds :: Text -> ( Bool, [ Int ] )  -- ^ (isAppend, surfaceIds)
parseSurfaceIds txt
  = let
      stripped = T.strip txt
      ( isAppend, numPart )
        = if "surface.append" `T.isPrefixOf` stripped
          then ( True, T.drop 14 stripped )  -- "surface.append" is 14 chars
          else if "surface" `T.isPrefixOf` stripped
            then ( False, T.drop 7 stripped )  -- "surface" is 7 chars
            else ( False, stripped )
    in 
      ( isAppend, parseIdSpec numPart )
  where
    parseIdSpec :: Text -> [ Int ]
    parseIdSpec spec
      = let
          parts = T.splitOn "," spec
          ( includes, excludes ) = foldr categorize ( [], [] ) parts
          baseIds = concatMap parseRange includes
          excludeIds = concatMap parseRange excludes
        in 
          filter (`notElem` excludeIds) baseIds

    categorize :: Text -> ( [ Text ], [ Text ] ) -> ( [ Text ], [ Text ] )
    categorize part ( inc, exc )
      | "!" `T.isPrefixOf` part = ( inc, T.drop 1 part : exc )
      | otherwise = ( part : inc, exc )

    parseRange :: Text -> [ Int ]
    parseRange part = case T.breakOn "-" part of
      ( start, rest )
        | not (T.null rest) && not (T.null start) -> let
            end = T.drop 1 rest
          in 
            case ( readMaybe (T.unpack start), readMaybe (T.unpack end) ) of
              ( Just s, Just e ) -> [ s .. e ]
              _ -> maybeToList (readMaybe (T.unpack part))
      _ -> maybeToList (readMaybe (T.unpack part))

    maybeToList :: Maybe a -> [ a ]
    maybeToList Nothing  = []
    maybeToList (Just x) = [ x ]

-- | Parse collision region from collision or collisionex line
-- Old format: collision<N>,x1,y1,x2,y2,name
-- New format: collisionex<N>,name,rect,x1,y1,x2,y2
--             collisionex<N>,name,ellipse,x1,y1,x2,y2
--             collisionex<N>,name,circle,cx,cy,r
--             collisionex<N>,name,polygon,x1,y1,x2,y2,...
--             collisionex<N>,name,region,filename,R,G,B[,flag]
parseCollision :: Text -> Text -> Maybe CollisionRegion
parseCollision key val
  = let
      parts = T.splitOn "," val
    in 
      if "collisionex" `T.isPrefixOf` key
        then parseCollisionEx parts
        else parseCollisionOld key parts
  where
    parseCollisionOld :: Text -> [ Text ] -> Maybe CollisionRegion
    parseCollisionOld k ps = do
      idx <- parseIndexFromKey "collision" k
      case ps of
        [ x1, y1, x2, y2, name ] -> do
          x1' <- readMaybe (T.unpack x1)
          y1' <- readMaybe (T.unpack y1)
          x2' <- readMaybe (T.unpack x2)
          y2' <- readMaybe (T.unpack y2)
          Just $ CollisionRegion idx (T.strip name) (CollisionRect x1' y1' x2' y2')
        _ -> Nothing

    parseCollisionEx :: [ Text ] -> Maybe CollisionRegion
    parseCollisionEx ps = do
      idx <- parseIndexFromKey "collisionex" key
      case ps of
        (name : shapeType : rest) -> do
          shape <- parseShape (T.toLower (T.strip shapeType)) rest
          Just $ CollisionRegion idx (T.strip name) shape
        _ -> Nothing

    parseShape :: Text -> [ Text ] -> Maybe CollisionShape
    parseShape "rect" [ x1, y1, x2, y2 ] = do
      x1' <- readMaybe (T.unpack x1)
      y1' <- readMaybe (T.unpack y1)
      x2' <- readMaybe (T.unpack x2)
      y2' <- readMaybe (T.unpack y2)
      Just $ CollisionRect x1' y1' x2' y2'
    parseShape "ellipse" [ x1, y1, x2, y2 ] = do
      x1' <- readMaybe (T.unpack x1)
      y1' <- readMaybe (T.unpack y1)
      x2' <- readMaybe (T.unpack x2)
      y2' <- readMaybe (T.unpack y2)
      Just $ CollisionEllipse x1' y1' x2' y2'
    parseShape "circle" [ cx, cy, r ] = do
      cx' <- readMaybe (T.unpack cx)
      cy' <- readMaybe (T.unpack cy)
      r' <- readMaybe (T.unpack r)
      Just $ CollisionCircle cx' cy' r'
    parseShape "polygon" coords = do
      let pairs = pairUp coords
      if null pairs
        then Nothing
        else Just $ CollisionPolygon pairs
    parseShape "region" (file : r : g : b : rest) = do
      r' <- readMaybe (T.unpack r)
      g' <- readMaybe (T.unpack g)
      b' <- readMaybe (T.unpack b)
      let invert = case rest of
            (flag : _) -> flag == "1" || T.toLower flag == "true"
            _          -> False
      Just $ CollisionRegionFile file r' g' b' invert
    parseShape _ _ = Nothing

    pairUp :: [ Text ] -> [ ( Int, Int ) ]
    pairUp (x : y : rest) = case ( readMaybe (T.unpack x), readMaybe (T.unpack y) ) of
      ( Just x', Just y' ) -> ( x', y' ) : pairUp rest
      _ -> pairUp rest
    pairUp _ = []

-- | Parse index from key like "collision0", "element5", "animation10"
parseIndexFromKey :: Text -> Text -> Maybe Int
parseIndexFromKey prefix key = readMaybe (T.unpack (T.drop (T.length prefix) key))

-- | Parse element line: element<N>,method,filename,x,y
parseElement :: Text -> Text -> Maybe Element
parseElement key val = do
  idx <- parseIndexFromKey "element" key
  case T.splitOn "," val of
    [ method, file, x, y ] -> do
      x' <- readMaybe (T.unpack x)
      y' <- readMaybe (T.unpack y)
      Just $ Element idx (parseDrawMethod method) file x' y'
    -- Also handle format without method (assumes overlay): element0,filename,x,y
    [ file, x, y ] -> do
      x' <- readMaybe (T.unpack x)
      y' <- readMaybe (T.unpack y)
      Just $ Element idx DrawOverlay file x' y'
    _ -> Nothing

-- | Parse animation pattern line (SERIKO version 1)
-- animation<N>.pattern<M>,method,surfaceId,wait,x,y
-- animation<N>.pattern<M>,method,surfaceId,wait-waitMax,x,y  (SSP random wait)
-- Old format (version 0): surfaceId,wait,method,x,y
parseAnimationPattern :: Int -> Text -> Maybe AnimationPattern
parseAnimationPattern idx val = case T.splitOn "," val of
  [ p1, p2, p3, p4, p5 ] ->
    -- Determine format by checking if first part is a number (old format) or method name (new format)
    case readMaybe (T.unpack p1) :: Maybe Int of
      Just surfId ->
        -- Old format: surfaceId,wait,method,x,y
        do
          x' <- readMaybe (T.unpack p4)
          y' <- readMaybe (T.unpack p5)
          let ( wait, waitMax ) = parseWait p2
          Just $ AnimationPattern idx (parseDrawMethod p3) surfId wait waitMax x' y'
      Nothing     ->
        -- New format: method,surfaceId,wait,x,y
        do
          surfId <- readMaybe (T.unpack p2)
          x' <- readMaybe (T.unpack p4)
          y' <- readMaybe (T.unpack p5)
          let ( wait, waitMax ) = parseWait p3
          Just $ AnimationPattern idx (parseDrawMethod p1) surfId wait waitMax x' y'
  _ -> Nothing
  where
    parseWait :: Text -> ( Int, Maybe Int )
    parseWait w = case T.breakOn "-" w of
      ( minW, rest )
        | not (T.null rest) -> ( readIntOr 0 minW, readMaybeInt (T.drop 1 rest) )
      _ -> ( readIntOr 0 w, Nothing )

-- | State for accumulating animation data during parsing
data AnimationAcc
  = AnimationAcc { aaInterval   :: AnimationInterval
                 , aaOptions    :: [ AnimationOption ]
                 , aaPatterns   :: [ AnimationPattern ]
                 , aaCollisions :: [ CollisionRegion ]
                 }

emptyAnimationAcc :: AnimationAcc
emptyAnimationAcc = AnimationAcc IntervalNever [] [] []

-- | Convert AnimationAcc to Animation
accToAnimation :: Int -> AnimationAcc -> Animation
accToAnimation aid acc
  = Animation { animId         = aid
              , animInterval   = aaInterval acc
              , animOptions    = aaOptions acc
              , animPatterns   = aaPatterns acc
              , animCollisions = aaCollisions acc
              }

-- | Parse surface brace content into SurfaceDefinition
parseSurfaceBrace :: Int -> [ Text ] -> SurfaceDefinition
parseSurfaceBrace sid = foldl' parseLine (emptySurfaceDefinition sid)
  where
    parseLine :: SurfaceDefinition -> Text -> SurfaceDefinition
    parseLine sd line = case T.breakOn "," line of
      ( rawKey, rest )
        | not (T.null rest) -> let
            key = T.toLower (T.strip rawKey)
            val = T.drop 1 rest  -- drop comma
          in 
            parseKey sd key val
      _ -> sd

    parseKey :: SurfaceDefinition -> Text -> Text -> SurfaceDefinition
    parseKey sd key val
      -- Elements



        | "element" `T.isPrefixOf` key = case parseElement key val of
          Just el -> sd { sdElements = sdElements sd ++ [ el ] }
          Nothing -> sd

        -- Collisions (surface-level)
        | "collisionex" `T.isPrefixOf` key = case parseCollision key val of
          Just col -> sd { sdCollisions = sdCollisions sd ++ [ col ] }
          Nothing  -> sd
        | "collision" `T.isPrefixOf` key && not ("." `T.isInfixOf` key)
          = case parseCollision key val of
            Just col -> sd { sdCollisions = sdCollisions sd ++ [ col ] }
            Nothing  -> sd

        -- Animation keys: animation<N>.interval, animation<N>.pattern<M>, etc.
        | "animation" `T.isPrefixOf` key = parseAnimationKey sd key val

        -- Balloon offsets
        | key == "sakura.balloon.offsetx" = sd { sdSakuraBalloonOffsetX = readMaybeInt val }
        | key == "sakura.balloon.offsety" = sd { sdSakuraBalloonOffsetY = readMaybeInt val }
        | key == "kero.balloon.offsetx" = sd { sdKeroBalloonOffsetX = readMaybeInt val }
        | key == "kero.balloon.offsety" = sd { sdKeroBalloonOffsetY = readMaybeInt val }
        | key == "balloon.offsetx" = sd { sdBalloonOffsetX = readMaybeInt val }
        | key == "balloon.offsety" = sd { sdBalloonOffsetY = readMaybeInt val }

        -- Center/position points
        | key == "point.centerx" = sd { sdPointCenterX = readMaybeInt val }
        | key == "point.centery" = sd { sdPointCenterY = readMaybeInt val }
        | key == "point.kinoko.centerx" = sd { sdPointKinokoCenterX = readMaybeInt val }
        | key == "point.kinoko.centery" = sd { sdPointKinokoCenterY = readMaybeInt val }
        | key == "point.basepos.x" = sd { sdPointBaseposX = readMaybeInt val }
        | key == "point.basepos.y" = sd { sdPointBaseposY = readMaybeInt val }

        | otherwise = sd

    -- Parse animation<N>.* keys
    parseAnimationKey :: SurfaceDefinition -> Text -> Text -> SurfaceDefinition
    parseAnimationKey sd key val = case T.stripPrefix "animation" key of
      Just rest -> case T.breakOn "." rest of
        ( numPart, dotRest )
          | not (T.null dotRest) -> case readMaybe (T.unpack numPart) of
            Just aid -> let
                subKey = T.drop 1 dotRest  -- drop the dot
              in 
                updateAnimation sd aid subKey val
            Nothing  -> sd
        _ -> sd
      Nothing   -> sd

    -- Update or create animation
    updateAnimation :: SurfaceDefinition -> Int -> Text -> Text -> SurfaceDefinition
    updateAnimation sd aid subKey val
      = let
          anims    = sdAnimations sd
          existing = filter (\a -> animId a == aid) anims
          others   = filter (\a -> animId a /= aid) anims
          acc      = case existing of
            (a : _)
              -> AnimationAcc (animInterval a) (animOptions a) (animPatterns a) (animCollisions a)
            []      -> emptyAnimationAcc
          acc'     = updateAnimationAcc acc subKey val
          anim     = accToAnimation aid acc'
        in 
          sd { sdAnimations = others ++ [ anim ] }

    updateAnimationAcc :: AnimationAcc -> Text -> Text -> AnimationAcc
    updateAnimationAcc acc subKey val
      | subKey == "interval" = acc { aaInterval = parseAnimationInterval val }
      | subKey == "option" = case parseAnimationOption val of
        Just opt -> acc { aaOptions = aaOptions acc ++ [ opt ] }
        Nothing  -> acc
      | "pattern" `T.isPrefixOf` subKey = case parseIndexFromKey "pattern" subKey of
        Just patIdx -> case parseAnimationPattern patIdx val of
          Just pat -> acc { aaPatterns = aaPatterns acc ++ [ pat ] }
          Nothing  -> acc
        Nothing     -> acc
      | "collision" `T.isPrefixOf` subKey
        = let
            fullKey = "collision" <> T.drop 9 subKey  -- rebuild collision key
          in 
            case parseCollision fullKey val of
              Just col -> acc { aaCollisions = aaCollisions acc ++ [ col ] }
              Nothing  -> acc
      | otherwise = acc

-- | Parse descript brace content
parseDescriptBrace :: [ Text ] -> SurfacesDescript
parseDescriptBrace = foldl' parseLine emptySurfacesDescript
  where
    parseLine :: SurfacesDescript -> Text -> SurfacesDescript
    parseLine sd line = case T.breakOn "," line of
      ( rawKey, rest )
        | not (T.null rest) -> let
            key = T.toLower (T.strip rawKey)
            val = T.strip (T.drop 1 rest)
          in 
            case key of
              "version" -> sd { surfDescVersion = readIntOr 1 val }
              "maxwidth" -> sd { surfDescMaxWidth = readMaybeInt val }
              "collision-sort" -> sd { surfDescCollisionSort = parseSortOrder val }
              "animation-sort" -> sd { surfDescAnimationSort = parseSortOrder val }
              _ -> sd
      _ -> sd

-- | Parse surface alias brace content
-- Format: name,[id1,id2,...]
parseAliasBrace :: [ Text ] -> [ SurfaceAlias ]
parseAliasBrace = mapMaybe parseLine
  where
    parseLine :: Text -> Maybe SurfaceAlias
    parseLine line = case T.breakOn "," line of
      ( name, rest )
        | not (T.null rest) -> let
            idsPart = T.drop 1 rest  -- drop comma
            -- Parse [id1,id2,...] format
            cleaned = T.filter (\c -> c /= '[' && c /= ']') idsPart
            ids     = mapMaybe (readMaybe . T.unpack . T.strip) (T.splitOn "," cleaned)
          in 
            if null ids
              then Nothing
              else Just $ SurfaceAlias (T.strip name) ids
      _ -> Nothing

-- | Parse cursor brace content
-- Format: mouseup<N>,collisionName,cursorFile
--         mousedown<N>,collisionName,cursorFile
parseCursorBrace :: [ Text ] -> ScopeCursors
parseCursorBrace = foldl' parseLine emptyScopeCursors
  where
    parseLine :: ScopeCursors -> Text -> ScopeCursors
    parseLine sc line = case T.breakOn "," line of
      ( rawKey, rest )
        | not (T.null rest) -> let
            key   = T.toLower (T.strip rawKey)
            parts = T.splitOn "," (T.drop 1 rest)
          in 
            case parts of
              [ collId, cursorFile ] -> let
                  def = CursorDef (T.strip collId) (T.strip cursorFile)
                in 
                  categorize key def sc
              _ -> sc
      _ -> sc

    categorize :: Text -> CursorDef -> ScopeCursors -> ScopeCursors
    categorize key def sc
      | "mouseup" `T.isPrefixOf` key = sc { scMouseUp = scMouseUp sc ++ [ def ] }
      | "mousedown" `T.isPrefixOf` key = sc { scMouseDown = scMouseDown sc ++ [ def ] }
      | "mouserightdown" `T.isPrefixOf` key
        = sc { scMouseRightDown = scMouseRightDown sc ++ [ def ] }
      | "mousewheel" `T.isPrefixOf` key = sc { scMouseWheel = scMouseWheel sc ++ [ def ] }
      | "mousehover" `T.isPrefixOf` key = sc { scMouseHover = scMouseHover sc ++ [ def ] }
      | otherwise = sc

-- | Parse tooltip brace content
-- Format: collisionName,tooltipText
parseTooltipBrace :: [ Text ] -> [ TooltipDef ]
parseTooltipBrace = mapMaybe parseLine
  where
    parseLine :: Text -> Maybe TooltipDef
    parseLine line = case T.breakOn "," line of
      ( collId, rest )
        | not (T.null rest) -> Just $ TooltipDef (T.strip collId) (T.drop 1 rest)
      _ -> Nothing

-- | Parse scope index from scope prefix
-- sakura -> 0, kero -> 1, char<N> -> N
parseScopeIndex :: Text -> Maybe Int
parseScopeIndex txt
  | "sakura" `T.isPrefixOf` txt = Just 0
  | "kero" `T.isPrefixOf` txt = Just 1
  | "char" `T.isPrefixOf` txt
    = let
        rest    = T.drop 4 txt  -- drop "char"
        numPart = T.takeWhile (/= '.') rest
      in 
        readMaybe (T.unpack numPart)
  | otherwise = Nothing

-- | Read and parse surfaces.txt
readSurfaces :: FilePath -> IO Surfaces
readSurfaces path = do
  rawBytes <- BL.readFile path
  let detectedCharset = detectCharsetFromBytes rawBytes
      utf8Bytes       = convertToUtf8 detectedCharset rawBytes
      contents        = TE.decodeUtf8 (BL.toStrict utf8Bytes)
      charsetLine     = case filter (T.isPrefixOf "charset,") (T.lines contents) of
        (l : _) -> T.strip (T.drop 8 l)
        []      -> ""
      braces          = tokenizeBraces contents
  return $ foldl' processBrace (emptySurfaces { surfacesCharset = charsetLine }) braces
  where
    processBrace :: Surfaces -> BraceBlock -> Surfaces
    processBrace surf (BraceBlock name lns)
      -- descript brace



        | name == "descript" = surf { surfacesDescript = parseDescriptBrace lns }

        -- surface definitions
        | "surface" `T.isPrefixOf` name
          && not ("alias" `T.isInfixOf` name)
          && not ("cursor" `T.isInfixOf` name)
          && not ("tooltips" `T.isInfixOf` name)
          = let
              ( isAppend, ids ) = parseSurfaceIds name
              newDefs           = map (`parseSurfaceBrace` lns) ids
            in 
              if isAppend
                then surf
                  { surfaceDefinitions = mergeSurfaceDefinitions (surfaceDefinitions surf) newDefs
                  }
                else surf { surfaceDefinitions = surfaceDefinitions surf ++ newDefs }

        -- surface aliases
        | name == "sakura.surface.alias" = surf { surfaceSakuraAlias = parseAliasBrace lns }
        | name == "kero.surface.alias" = surf { surfaceKeroAlias = parseAliasBrace lns }
        | ".surface.alias" `T.isSuffixOf` name = case parseScopeIndex name of
          Just idx
            | idx >= 2 -> surf
              { surfaceCharAliases = surfaceCharAliases surf ++ [ ( idx, parseAliasBrace lns ) ] }
          _        -> surf

        -- cursors
        | name == "sakura.cursor" = surf { surfaceSakuraCursor = parseCursorBrace lns }
        | name == "kero.cursor" = surf { surfaceKeroCursor = parseCursorBrace lns }
        | ".cursor" `T.isSuffixOf` name = case parseScopeIndex name of
          Just idx
            | idx >= 2 -> surf
              { surfaceCharCursors = surfaceCharCursors surf ++ [ ( idx, parseCursorBrace lns ) ] }
          _        -> surf

        -- tooltips
        | name == "sakura.tooltips" = surf { surfaceSakuraTooltips = parseTooltipBrace lns }
        | name == "kero.tooltips" = surf { surfaceKeroTooltips = parseTooltipBrace lns }
        | ".tooltips" `T.isSuffixOf` name = case parseScopeIndex name of
          Just idx
            | idx >= 2 -> surf { surfaceCharTooltips
                                   = surfaceCharTooltips surf ++ [ ( idx, parseTooltipBrace lns ) ]
                               }
          _        -> surf

        | otherwise = surf

    -- Merge appended surface definitions with existing ones
    mergeSurfaceDefinitions
      :: [ SurfaceDefinition ] -> [ SurfaceDefinition ] -> [ SurfaceDefinition ]
    mergeSurfaceDefinitions existing appends
      = let
          existingMap = Map.fromList [ ( sdId sd, sd ) | sd <- existing ]
          merged      = foldl' mergeOne existingMap appends
        in 
          Map.elems merged

    mergeOne :: Map Int SurfaceDefinition -> SurfaceDefinition -> Map Int SurfaceDefinition
    mergeOne m append
      = let
          sid = sdId append
        in 
          case Map.lookup sid m of
            Just existing -> Map.insert sid (mergeSurfaceDef existing append) m
            Nothing       -> Map.insert sid append m

    mergeSurfaceDef :: SurfaceDefinition -> SurfaceDefinition -> SurfaceDefinition
    mergeSurfaceDef base append
      = base
      { sdElements = sdElements base ++ sdElements append
      , sdAnimations = sdAnimations base ++ sdAnimations append
      , sdCollisions = sdCollisions base ++ sdCollisions append
        -- Override balloon offsets if appended
      , sdSakuraBalloonOffsetX = sdSakuraBalloonOffsetX append <|> sdSakuraBalloonOffsetX base
      , sdSakuraBalloonOffsetY = sdSakuraBalloonOffsetY append <|> sdSakuraBalloonOffsetY base
      , sdKeroBalloonOffsetX = sdKeroBalloonOffsetX append <|> sdKeroBalloonOffsetX base
      , sdKeroBalloonOffsetY = sdKeroBalloonOffsetY append <|> sdKeroBalloonOffsetY base
      , sdBalloonOffsetX = sdBalloonOffsetX append <|> sdBalloonOffsetX base
      , sdBalloonOffsetY = sdBalloonOffsetY append <|> sdBalloonOffsetY base
      , sdPointCenterX = sdPointCenterX append <|> sdPointCenterX base
      , sdPointCenterY = sdPointCenterY append <|> sdPointCenterY base
      , sdPointKinokoCenterX = sdPointKinokoCenterX append <|> sdPointKinokoCenterX base
      , sdPointKinokoCenterY = sdPointKinokoCenterY append <|> sdPointKinokoCenterY base
      , sdPointBaseposX = sdPointBaseposX append <|> sdPointBaseposX base
      , sdPointBaseposY = sdPointBaseposY append <|> sdPointBaseposY base
      }

data Shell
  = Shell { shellDescript :: ShellDescript
          , shellSurfaces :: Surfaces
          , shellPath     :: FilePath      -- ^ Path to the shell directory
          }
  deriving ( Show, Eq )

data Ghost
  = Ghost { ghostDescript :: GhostDescript
          , ghostShells   :: [ Shell ]
          , ghostPath     :: FilePath      -- ^ Path to the ghost root directory
          }
  deriving ( Show, Eq )

-- | Load a shell from a shell directory
-- Shell directory should contain descript.txt and surfaces.txt
loadShell :: FilePath -> IO (Maybe Shell)
loadShell shellDir = do
  let descriptPath = shellDir </> "descript.txt"
      surfacesPath = shellDir </> "surfaces.txt"

  descriptExists <- doesFileExist descriptPath
  surfacesExists <- doesFileExist surfacesPath

  if not descriptExists
    then return Nothing
    else do
      -- Load shell descript (required)
      descriptResult <- try (readShellDescript descriptPath)
        :: IO (Either SomeException ShellDescript)
      case descriptResult of
        Left _         -> return Nothing
        Right descript -> do
          -- Load surfaces (optional, use empty if not present)
          surfaces <- if surfacesExists
            then do
              surfResult <- try (readSurfaces surfacesPath) :: IO (Either SomeException Surfaces)
              case surfResult of
                Left _  -> return emptySurfaces
                Right s -> return s
            else return emptySurfaces

          return
            $ Just
              Shell { shellDescript = descript, shellSurfaces = surfaces, shellPath = shellDir }

-- | Load a ghost from a ghost root directory
-- Ghost directory structure:
--   (myghost)/
--   ├── ghost/master/descript.txt   -- Ghost descript (required)
--   ├── shell/master/               -- Default shell (required)
--   │   ├── descript.txt
--   │   └── surfaces.txt
--   └── shell/(additional)/         -- Additional shells (optional)
loadGhost :: FilePath -> IO (Maybe Ghost)
loadGhost ghostDir = do
  let ghostDescriptPath = ghostDir </> "ghost" </> "master" </> "descript.txt"
      shellBaseDir      = ghostDir </> "shell"

  -- Check if ghost descript exists
  ghostDescriptExists <- doesFileExist ghostDescriptPath
  shellDirExists <- doesDirectoryExist shellBaseDir

  if not ghostDescriptExists || not shellDirExists
    then return Nothing
    else do
      -- Load ghost descript
      descriptResult <- try (readGhostDescript ghostDescriptPath)
        :: IO (Either SomeException GhostDescript)
      case descriptResult of
        Left _         -> return Nothing
        Right descript -> do
          -- Find all shell directories
          shellDirs <- listDirectory shellBaseDir
          shellPaths <- filterM doesDirectoryExist (map (shellBaseDir </>) shellDirs)

          -- Load all shells
          shells <- forM shellPaths loadShell
          let loadedShells = catMaybes shells

          -- Must have at least the master shell
          if null loadedShells
            then return Nothing
            else return
              $ Just
                Ghost
                { ghostDescript = descript, ghostShells = loadedShells, ghostPath = ghostDir }