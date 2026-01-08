{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Surface types, definitions, and parsers for surfaces.txt
module Types.Ghost.Surface
  ( -- * Surface types
    Surface(..)
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

import           Control.Applicative        ( (<|>) )

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
import           Utils.Text                 ( readIntOr, readMaybeInt )

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
        | "surface.append" `T.isPrefixOf` stripped = ( True, T.drop 14 stripped )
        | "surface" `T.isPrefixOf` stripped = ( False, T.drop 7 stripped )
        | otherwise = ( False, stripped )
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
parseAnimationPattern :: Int -> Text -> Maybe AnimationPattern
parseAnimationPattern idx val = case T.splitOn "," val of
  [ p1, p2, p3, p4, p5 ] ->
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
parseTooltipBrace :: [ Text ] -> [ TooltipDef ]
parseTooltipBrace = mapMaybe parseLine
  where
    parseLine :: Text -> Maybe TooltipDef
    parseLine line = case T.breakOn "," line of
      ( collId, rest )
        | not (T.null rest) -> Just $ TooltipDef (T.strip collId) (T.drop 1 rest)
      _ -> Nothing

-- | Parse scope index from scope prefix
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
            ( _, ids ) = parseSurfaceIds name
            newDefs           = map (`parseSurfaceBrace` lns) ids
          in
            surf
              { surfaceDefinitions = mergeSurfaceDefinitions (surfaceDefinitions surf) newDefs
              }

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
