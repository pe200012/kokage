{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Shell descriptor types and parsers.
-- Parses shell/*/descript.txt files.
module Types.Ghost.Shell
  ( -- * Shell descriptor
    ShellDescript(..)
  , emptyShellDescript
  , readShellDescript
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
  ) where

import qualified Data.ByteString.Lazy       as BL
import           Data.List                  ( foldl' )
import           Data.Map.Strict            ( Map )
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 ( mapMaybe )
import           Data.Text                  ( Text )
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE

import           Text.Read                  ( readMaybe )

import           Utils.Charset              ( convertToUtf8, detectCharsetFromBytes )
import           Utils.Text                 ( clean, readIntOr, readMaybeInt, readMaybeBool )

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
  , csSurfaceLife :: Maybe Int    -- ^ Surface life in milliseconds (for OnSurfaceRestore)
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
  , csSurfaceLife = Nothing
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
      "surface_life" -> updateCharSettings idx (\cs -> cs { csSurfaceLife = readMaybeInt val }) desc

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
