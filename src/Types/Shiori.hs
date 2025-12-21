{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | SHIORI/3.0 protocol types and parsing
-- Reference: https://ssp.shillest.net/ukadoc/manual/spec_shiori3.html
--            https://ssp.shillest.net/ukadoc/manual/list_shiori_event.html
--            https://ssp.shillest.net/ukadoc/manual/list_shiori_resource.html
module Types.Shiori
  ( -- * Protocol Types
    ShioriVersion(..)
  , ShioriMethod(..)
  , ShioriStatusCode(..)
  , ShioriRequest(..)
  , ShioriResponse(..)
    -- * Sender Type (SSP extension)
  , SenderType(..)
  , parseSenderType
  , parseSenderTypes
  , serializeSenderType
  , serializeSenderTypes
    -- * Ghost Status (SSP extension)
  , GhostStatus(..)
  , parseGhostStatus
  , parseGhostStatuses
  , serializeGhostStatus
  , serializeGhostStatuses
    -- * Security Level
  , SecurityLevel(..)
  , parseSecurityLevel
  , serializeSecurityLevel
    -- * Security Origin (SSP extension)
  , SecurityOrigin(..)
  , parseSecurityOrigin
  , serializeSecurityOrigin
    -- * Error Level (SSP extension)
  , ErrorLevel(..)
  , parseErrorLevel
  , parseErrorLevels
  , serializeErrorLevel
  , serializeErrorLevels
    -- * Balloon Offset (SSP extension)
  , BalloonOffset(..)
  , parseBalloonOffset
  , serializeBalloonOffset
    -- * Events
  , ShioriEvent(..)
  , ShioriEventCategory(..)
  , eventCategory
  , eventToId
  , eventFromId
    -- * Resources
  , ShioriResource(..)
  , resourceToId
  , resourceFromId
    -- * Common Reference Types
  , MouseButton(..)
  , WheelDirection(..)
  , InstallResult(..)
  , UpdateResult(..)
    -- * Parsing
  , parseShioriRequest
  , parseShioriResponse
  , parseShioriResponseBS
    -- * Serialization
  , serializeShioriRequest
  , serializeShioriResponse
  , serializeShioriRequestBS
    -- * Request Builders
  , mkRequest
  , mkNotify
  , emptyResponse
  ) where

import           Data.Text                  ( Text )
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS8
import qualified Data.Map.Strict            as Map
import           Data.Map.Strict            ( Map )
import           Data.Maybe                 ( fromMaybe, mapMaybe )
import qualified TextBuilder                as TB

--------------------------------------------------------------------------------
-- Protocol Version
--------------------------------------------------------------------------------

-- | SHIORI protocol version
data ShioriVersion
  = Shiori20    -- ^ SHIORI/2.0
  | Shiori26    -- ^ SHIORI/2.6
  | Shiori30    -- ^ SHIORI/3.0 (current standard)
  deriving ( Show, Eq, Ord )

-- | Parse version from text
parseShioriVersion :: Text -> Maybe ShioriVersion
parseShioriVersion t = case T.strip t of
  "SHIORI/2.0" -> Just Shiori20
  "SHIORI/2.6" -> Just Shiori26
  "SHIORI/3.0" -> Just Shiori30
  _            -> Nothing

-- | Serialize version to text
serializeShioriVersion :: ShioriVersion -> Text
serializeShioriVersion Shiori20 = "SHIORI/2.0"
serializeShioriVersion Shiori26 = "SHIORI/2.6"
serializeShioriVersion Shiori30 = "SHIORI/3.0"

--------------------------------------------------------------------------------
-- Protocol Method
--------------------------------------------------------------------------------

-- | SHIORI request method
data ShioriMethod
  = MethodGet     -- ^ GET - Request data from ghost, expects response
  | MethodNotify  -- ^ NOTIFY - Notify ghost of event, no response expected
  deriving ( Show, Eq )

-- | Parse method from text
parseShioriMethod :: Text -> Maybe ShioriMethod
parseShioriMethod t = case T.strip t of
  "GET"    -> Just MethodGet
  "NOTIFY" -> Just MethodNotify
  _        -> Nothing

-- | Serialize method to text
serializeShioriMethod :: ShioriMethod -> Text
serializeShioriMethod MethodGet    = "GET"
serializeShioriMethod MethodNotify = "NOTIFY"

--------------------------------------------------------------------------------
-- Status Codes
--------------------------------------------------------------------------------

-- | SHIORI response status code
data ShioriStatusCode
  = Status200  -- ^ OK - Request processed successfully
  | Status204  -- ^ No Content - No response content (valid for some events)
  | Status310  -- ^ Communicate - Response to communication
  | Status311  -- ^ Not Enough - Ghost wants more input
  | Status312  -- ^ Advice - Ghost provides advice
  | Status400  -- ^ Bad Request - Malformed request
  | Status500  -- ^ Internal Server Error - Ghost internal error
  | StatusOther Int Text  -- ^ Unknown status code
  deriving ( Show, Eq )

-- | Get numeric status code
statusCodeNumber :: ShioriStatusCode -> Int
statusCodeNumber Status200        = 200
statusCodeNumber Status204        = 204
statusCodeNumber Status310        = 310
statusCodeNumber Status311        = 311
statusCodeNumber Status312        = 312
statusCodeNumber Status400        = 400
statusCodeNumber Status500        = 500
statusCodeNumber (StatusOther n _) = n

-- | Get status message
statusCodeMessage :: ShioriStatusCode -> Text
statusCodeMessage Status200        = "OK"
statusCodeMessage Status204        = "No Content"
statusCodeMessage Status310        = "Communicate"
statusCodeMessage Status311        = "Not Enough"
statusCodeMessage Status312        = "Advice"
statusCodeMessage Status400        = "Bad Request"
statusCodeMessage Status500        = "Internal Server Error"
statusCodeMessage (StatusOther _ m) = m

-- | Parse status code from number
parseShioriStatusCode :: Int -> Maybe Text -> ShioriStatusCode
parseShioriStatusCode 200 _ = Status200
parseShioriStatusCode 204 _ = Status204
parseShioriStatusCode 310 _ = Status310
parseShioriStatusCode 311 _ = Status311
parseShioriStatusCode 312 _ = Status312
parseShioriStatusCode 400 _ = Status400
parseShioriStatusCode 500 _ = Status500
parseShioriStatusCode n msg = StatusOther n (fromMaybe "" msg)

--------------------------------------------------------------------------------
-- Request/Response Types
--------------------------------------------------------------------------------

-- | SHIORI request from baseware to ghost
data ShioriRequest
  = ShioriRequest
  { srqMethod         :: ShioriMethod           -- ^ Request method (GET/NOTIFY)
  , srqVersion        :: ShioriVersion          -- ^ Protocol version
  , srqCharset        :: Text                   -- ^ Character encoding (usually UTF-8)
  , srqSender         :: Text                   -- ^ Sender name (baseware name)
  , srqSenderType     :: [SenderType]           -- ^ Sender type attributes [SSP 2.5.05+]
  , srqSecurityLevel  :: Maybe SecurityLevel    -- ^ Security level (local/external)
  , srqSecurityOrigin :: Maybe SecurityOrigin   -- ^ Security origin URL [SSP extension]
  , srqStatus         :: [GhostStatus]          -- ^ Ghost execution status [SSP extension]
  , srqId             :: Text                   -- ^ Event ID or resource ID
  , srqBaseId         :: Maybe Text             -- ^ Base ID for special events [SSP extension]
  , srqReferences     :: Map Int Text           -- ^ Reference0, Reference1, etc.
  , srqPassThru       :: Map Text Text          -- ^ X-SSTP-PassThru-* headers [SSP 2.5.05+]
  , srqHeaders        :: Map Text Text          -- ^ Additional headers
  }
  deriving ( Show, Eq )

-- | SHIORI response from ghost to baseware
data ShioriResponse
  = ShioriResponse
  { srsVersion          :: ShioriVersion          -- ^ Protocol version
  , srsStatus           :: ShioriStatusCode       -- ^ Status code
  , srsCharset          :: Text                   -- ^ Character encoding
  , srsSender           :: Text                   -- ^ Sender name (ghost name)
  , srsValue            :: Maybe Text             -- ^ Response value (SakuraScript for events)
  , srsValueNotify      :: Maybe Text             -- ^ Immediate script execution [SSP 2.5.35+]
  , srsSecurityLevel    :: Maybe SecurityLevel    -- ^ Script security level [SSP extension]
  , srsMarker           :: Maybe Text             -- ^ Balloon marker text [SSP extension]
  , srsErrorLevel       :: [ErrorLevel]           -- ^ Error levels [SSP extension]
  , srsErrorDescription :: [Text]                 -- ^ Error descriptions [SSP extension]
  , srsBalloonOffset    :: Maybe BalloonOffset    -- ^ Balloon offset (X,Y) [SSP extension]
  , srsAge              :: Maybe Int              -- ^ Communicate generation count [SSP extension]
  , srsMarkerSend       :: Maybe Text             -- ^ Marker to send to communicate partner [SSP extension]
  , srsReferences       :: Map Int Text           -- ^ Reference0, Reference1, etc. (for communicate)
  , srsPassThru         :: Map Text Text          -- ^ X-SSTP-PassThru-* headers [SSP 2.5.03+]
  , srsHeaders          :: Map Text Text          -- ^ Additional headers
  }
  deriving ( Show, Eq )

--------------------------------------------------------------------------------
-- Mouse Types
--------------------------------------------------------------------------------

-- | Mouse button type
data MouseButton
  = LeftButton
  | RightButton
  | MiddleButton
  | ExButton1
  | ExButton2
  deriving ( Show, Eq )

-- | Parse mouse button from reference value
parseMouseButton :: Text -> MouseButton
parseMouseButton t = case T.strip t of
  "0" -> LeftButton
  "1" -> RightButton
  "2" -> MiddleButton
  "3" -> ExButton1
  "4" -> ExButton2
  _   -> LeftButton

-- | Mouse wheel direction
data WheelDirection
  = WheelUp
  | WheelDown
  deriving ( Show, Eq )

--------------------------------------------------------------------------------
-- Install/Update Result Types
--------------------------------------------------------------------------------

-- | Installation result
data InstallResult
  = InstallSuccess
  | InstallFailure
  | InstallRefuse
  deriving ( Show, Eq )

-- | Network update result
data UpdateResult
  = UpdateSuccess
  | UpdateFailure
  | UpdateNone       -- ^ No updates available
  deriving ( Show, Eq )

--------------------------------------------------------------------------------
-- Sender Type (SSP 2.5.05+ extension)
--------------------------------------------------------------------------------

-- | Event sender type attributes.
-- Multiple types can be combined (comma-separated in protocol).
data SenderType
  = SenderInternal      -- ^ Event from application internal
  | SenderExternal      -- ^ Event from application external
  | SenderSakuraApi     -- ^ Event from Sakura API
  | SenderEmbed         -- ^ Event from \![embed] tag
  | SenderRaise         -- ^ Event from \![raise] tag
  | SenderProperty      -- ^ Event from property system reference
  | SenderPlugin        -- ^ Event from plugin
  | SenderSSTP          -- ^ Event from SSTP
  | SenderCommunicate   -- ^ Event from communicate specification
  deriving ( Show, Eq, Ord )

-- | Parse a single sender type from text
parseSenderType :: Text -> Maybe SenderType
parseSenderType t = case T.strip t of
  "internal"    -> Just SenderInternal
  "external"    -> Just SenderExternal
  "sakuraapi"   -> Just SenderSakuraApi
  "embed"       -> Just SenderEmbed
  "raise"       -> Just SenderRaise
  "property"    -> Just SenderProperty
  "plugin"      -> Just SenderPlugin
  "sstp"        -> Just SenderSSTP
  "communicate" -> Just SenderCommunicate
  _             -> Nothing

-- | Parse comma-separated sender types
parseSenderTypes :: Text -> [SenderType]
parseSenderTypes t = mapMaybe parseSenderType (T.splitOn "," t)

-- | Serialize sender type to text
serializeSenderType :: SenderType -> Text
serializeSenderType SenderInternal    = "internal"
serializeSenderType SenderExternal    = "external"
serializeSenderType SenderSakuraApi   = "sakuraapi"
serializeSenderType SenderEmbed       = "embed"
serializeSenderType SenderRaise       = "raise"
serializeSenderType SenderProperty    = "property"
serializeSenderType SenderPlugin      = "plugin"
serializeSenderType SenderSSTP        = "sstp"
serializeSenderType SenderCommunicate = "communicate"

-- | Serialize sender types to comma-separated text
serializeSenderTypes :: [SenderType] -> Text
serializeSenderTypes = T.intercalate "," . map serializeSenderType

--------------------------------------------------------------------------------
-- Ghost Status (SSP extension)
--------------------------------------------------------------------------------

-- | Ghost execution status flags.
-- Multiple statuses can be active simultaneously.
data GhostStatus
  = StatusTalking                      -- ^ Ghost is talking
  | StatusChoosing                     -- ^ Choice selection in progress
  | StatusMinimizing                   -- ^ Window is minimized
  | StatusInduction                    -- ^ In \![enter,inductionmode]
  | StatusPassive                      -- ^ In \![enter,passivemode]
  | StatusTimeCritical                 -- ^ In time critical section (\t)
  | StatusNoUserBreak                  -- ^ In \![enter,nouserbreak]
  | StatusOnline                       -- ^ Network communication in progress
  | StatusOpening [Text]               -- ^ Input boxes open (types: communicate, input, teach, dialog)
  | StatusBalloon [(Int, Int)]         -- ^ Balloons visible: [(charId, balloonId)]
  deriving ( Show, Eq )

-- | Parse a single status from text
parseGhostStatus :: Text -> Maybe GhostStatus
parseGhostStatus t = case T.strip t of
  "talking"       -> Just StatusTalking
  "choosing"      -> Just StatusChoosing
  "minimizing"    -> Just StatusMinimizing
  "induction"     -> Just StatusInduction
  "passive"       -> Just StatusPassive
  "timecritical"  -> Just StatusTimeCritical
  "nouserbreak"   -> Just StatusNoUserBreak
  "online"        -> Just StatusOnline
  other
    | "opening(" `T.isPrefixOf` other && ")" `T.isSuffixOf` other ->
        let content = T.dropEnd 1 $ T.drop 8 other  -- drop "opening(" and ")"
            types = T.splitOn "/" content
        in Just $ StatusOpening types
    | "balloon(" `T.isPrefixOf` other && ")" `T.isSuffixOf` other ->
        let content = T.dropEnd 1 $ T.drop 8 other  -- drop "balloon(" and ")"
            pairs = T.splitOn "/" content
            parsePair p = case T.splitOn "=" p of
              [cid, bid] -> case (reads (T.unpack cid), reads (T.unpack bid)) of
                ([(c, "")], [(b, "")]) -> Just (c, b)
                _ -> Nothing
              _ -> Nothing
        in Just $ StatusBalloon (mapMaybe parsePair pairs)
    | otherwise -> Nothing

-- | Parse comma-separated status values
parseGhostStatuses :: Text -> [GhostStatus]
parseGhostStatuses t = mapMaybe parseGhostStatus (T.splitOn "," t)

-- | Serialize ghost status to text
serializeGhostStatus :: GhostStatus -> Text
serializeGhostStatus StatusTalking      = "talking"
serializeGhostStatus StatusChoosing     = "choosing"
serializeGhostStatus StatusMinimizing   = "minimizing"
serializeGhostStatus StatusInduction    = "induction"
serializeGhostStatus StatusPassive      = "passive"
serializeGhostStatus StatusTimeCritical = "timecritical"
serializeGhostStatus StatusNoUserBreak  = "nouserbreak"
serializeGhostStatus StatusOnline       = "online"
serializeGhostStatus (StatusOpening types) = "opening(" <> T.intercalate "/" types <> ")"
serializeGhostStatus (StatusBalloon pairs) = 
  "balloon(" <> T.intercalate "/" (map (\(c, b) -> T.pack (show c) <> "=" <> T.pack (show b)) pairs) <> ")"

-- | Serialize ghost statuses to comma-separated text
serializeGhostStatuses :: [GhostStatus] -> Text
serializeGhostStatuses = T.intercalate "," . map serializeGhostStatus

--------------------------------------------------------------------------------
-- Security Level
--------------------------------------------------------------------------------

-- | Security level for script execution
data SecurityLevel
  = SecurityLocal     -- ^ From local machine (safe)
  | SecurityExternal  -- ^ From external source (requires caution)
  deriving ( Show, Eq )

-- | Parse security level from text
parseSecurityLevel :: Text -> Maybe SecurityLevel
parseSecurityLevel t = case T.strip t of
  "local"    -> Just SecurityLocal
  "external" -> Just SecurityExternal
  _          -> Nothing

-- | Serialize security level to text
serializeSecurityLevel :: SecurityLevel -> Text
serializeSecurityLevel SecurityLocal    = "local"
serializeSecurityLevel SecurityExternal = "external"

--------------------------------------------------------------------------------
-- Security Origin (SSP extension)
--------------------------------------------------------------------------------

-- | Security origin indicating the source server.
-- Format: null | <scheme>://<hostname> | <scheme>://<hostname>:<port>
data SecurityOrigin
  = OriginNull                                   -- ^ No origin information
  | Origin { originScheme :: Text                -- ^ Protocol (http, https, sstp)
           , originHost   :: Text                -- ^ Hostname or IP
           , originPort   :: Maybe Int           -- ^ Optional port
           }
  deriving ( Show, Eq )

-- | Parse security origin from text
parseSecurityOrigin :: Text -> SecurityOrigin
parseSecurityOrigin t = case T.strip t of
  "null" -> OriginNull
  other  -> case T.breakOn "://" other of
    (scheme, rest)
      | not (T.null rest) ->
          let hostPort = T.drop 3 rest  -- drop "://"
          in case T.breakOn ":" hostPort of
            (host, portPart)
              | T.null portPart -> Origin scheme host Nothing
              | otherwise -> case reads (T.unpack $ T.drop 1 portPart) of
                  [(p, "")] -> Origin scheme host (Just p)
                  _         -> Origin scheme hostPort Nothing
    _ -> OriginNull

-- | Serialize security origin to text
serializeSecurityOrigin :: SecurityOrigin -> Text
serializeSecurityOrigin OriginNull = "null"
serializeSecurityOrigin (Origin scheme host mPort) = 
  scheme <> "://" <> host <> maybe "" (\p -> ":" <> T.pack (show p)) mPort

--------------------------------------------------------------------------------
-- Error Level (SSP extension)
--------------------------------------------------------------------------------

-- | Error level for SHIORI internal errors
data ErrorLevel
  = ErrorInfo      -- ^ Information, not an error
  | ErrorNotice    -- ^ Notice, should be fixed but not critical
  | ErrorWarning   -- ^ Warning, has issues but processing continues
  | ErrorError     -- ^ Normal error, processing stopped
  | ErrorCritical  -- ^ Critical error, immediate action needed
  deriving ( Show, Eq, Ord )

-- | Parse error level from text
parseErrorLevel :: Text -> Maybe ErrorLevel
parseErrorLevel t = case T.strip t of
  "info"     -> Just ErrorInfo
  "notice"   -> Just ErrorNotice
  "warning"  -> Just ErrorWarning
  "error"    -> Just ErrorError
  "critical" -> Just ErrorCritical
  _          -> Nothing

-- | Parse multiple error levels (separated by byte value 1)
parseErrorLevels :: Text -> [ErrorLevel]
parseErrorLevels t = mapMaybe parseErrorLevel (T.splitOn "\x01" t)

-- | Serialize error level to text
serializeErrorLevel :: ErrorLevel -> Text
serializeErrorLevel ErrorInfo     = "info"
serializeErrorLevel ErrorNotice   = "notice"
serializeErrorLevel ErrorWarning  = "warning"
serializeErrorLevel ErrorError    = "error"
serializeErrorLevel ErrorCritical = "critical"

-- | Serialize multiple error levels (separated by byte value 1)
serializeErrorLevels :: [ErrorLevel] -> Text
serializeErrorLevels = T.intercalate "\x01" . map serializeErrorLevel

--------------------------------------------------------------------------------
-- Balloon Offset (SSP extension)
--------------------------------------------------------------------------------

-- | Balloon offset value (X, Y coordinates)
data BalloonOffset = BalloonOffset
  { boX :: !Int
  , boY :: !Int
  }
  deriving ( Show, Eq )

-- | Parse balloon offset from text (format: "X,Y")
parseBalloonOffset :: Text -> Maybe BalloonOffset
parseBalloonOffset t = case T.splitOn "," (T.strip t) of
  [xStr, yStr] -> case (reads (T.unpack xStr), reads (T.unpack yStr)) of
    ([(x, "")], [(y, "")]) -> Just $ BalloonOffset x y
    _ -> Nothing
  _ -> Nothing

-- | Serialize balloon offset to text
serializeBalloonOffset :: BalloonOffset -> Text
serializeBalloonOffset (BalloonOffset x y) = T.pack (show x) <> "," <> T.pack (show y)

--------------------------------------------------------------------------------
-- Event Categories
--------------------------------------------------------------------------------

-- | Category of SHIORI events
data ShioriEventCategory
  = CatBoot           -- ^ Boot/Close/Switch events
  | CatInput          -- ^ Input box events
  | CatDialog         -- ^ Dialog events
  | CatTime           -- ^ Time-related events
  | CatVanish         -- ^ Vanish events
  | CatChoice         -- ^ Choice/Anchor events
  | CatSurface        -- ^ Surface events
  | CatMouse          -- ^ Mouse events
  | CatBalloon        -- ^ Balloon events
  | CatInstall        -- ^ Install events
  | CatFileDrop       -- ^ File drop events
  | CatUpdate         -- ^ Network update events
  | CatMail           -- ^ Mail/BIFF events
  | CatHeadline       -- ^ Headline/RSS events
  | CatCalendar       -- ^ Calendar events
  | CatSSTP           -- ^ SSTP events
  | CatOS             -- ^ OS state events
  | CatNotify         -- ^ NOTIFY events from baseware
  | CatPlugin         -- ^ Plugin events
  | CatOther          -- ^ Uncategorized events
  deriving ( Show, Eq )

--------------------------------------------------------------------------------
-- SHIORI Events
--------------------------------------------------------------------------------

-- | All SHIORI events
-- Events are organized by category for easier maintenance
data ShioriEvent
  -- Boot/Close/Switch Events
  = OnFirstBoot               -- ^ First time ghost is booted
  | OnBoot                    -- ^ Ghost boots up
  | OnClose                   -- ^ Ghost is closing (can show bye message)
  | OnCloseAll                -- ^ All ghosts closing (baseware shutdown)
  | OnGhostChanged            -- ^ Changed from another ghost
  | OnGhostChanging           -- ^ About to change to another ghost
  | OnGhostCalled             -- ^ Called by another ghost
  | OnGhostCalling            -- ^ Calling another ghost
  | OnGhostCallComplete       -- ^ Ghost call completed
  | OnOtherGhostBooted        -- ^ Another ghost booted
  | OnOtherGhostClosed        -- ^ Another ghost closed
  | OnOtherGhostChanged       -- ^ Another ghost changed
  | OnShellChanged            -- ^ Shell was changed
  | OnShellChanging           -- ^ Shell is about to change
  | OnDressupChanged          -- ^ Dressup changed
  | OnBalloonChange           -- ^ Balloon changed
  
  -- Second Change / Time Events
  | OnSecondChange            -- ^ Every second
  | OnMinuteChange            -- ^ Every minute
  | OnHourTimeSignal          -- ^ On the hour
  
  -- Input Events
  | OnTeachStart              -- ^ User started teaching mode
  | OnTeach                   -- ^ User is teaching
  | OnCommunicate             -- ^ Communication from user or other ghost
  | OnUserInput               -- ^ Generic user input
  | OnUserInputCancel         -- ^ User cancelled input
  
  -- Dialog Events
  | OnSystemDialog            -- ^ System dialog shown
  | OnConfigurationDialogHelp -- ^ Help requested in config dialog
  
  -- Vanish Events
  | OnVanishSelecting         -- ^ Vanish being selected
  | OnVanishSelected          -- ^ Vanish selected (confirm)
  | OnVanishCancel            -- ^ Vanish cancelled
  | OnVanishButtonHold        -- ^ Vanish button held
  | OnVanished                -- ^ Ghost vanished
  | OnVanishBroadcast         -- ^ Notify other ghosts of vanish
  
  -- Choice/Anchor Events
  | OnChoiceSelect            -- ^ User selected a choice
  | OnChoiceSelectEx          -- ^ Extended choice select with more info
  | OnChoiceEnter             -- ^ Mouse entered choice area
  | OnChoiceTimeout           -- ^ Choice timed out
  | OnChoiceHover             -- ^ Mouse hovering over choice
  | OnAnchorSelect            -- ^ User clicked anchor
  | OnAnchorSelectEx          -- ^ Extended anchor select
  
  -- Surface Events
  | OnSurfaceChange           -- ^ Surface changed
  | OnSurfaceRestore          -- ^ Surface restored to default
  
  -- Mouse Events (main)
  | OnMouseClick              -- ^ Mouse clicked
  | OnMouseClickEx            -- ^ Extended mouse click
  | OnMouseDoubleClick        -- ^ Mouse double-clicked
  | OnMouseDoubleClickEx      -- ^ Extended double-click
  | OnMouseMultiClick         -- ^ Mouse multi-clicked (3+)
  | OnMouseMultiClickEx       -- ^ Extended multi-click
  | OnMouseUp                 -- ^ Mouse button released
  | OnMouseUpEx               -- ^ Extended mouse up
  | OnMouseDown               -- ^ Mouse button pressed
  | OnMouseDownEx             -- ^ Extended mouse down
  | OnMouseMove               -- ^ Mouse moved
  | OnMouseWheel              -- ^ Mouse wheel scrolled
  | OnMouseEnter              -- ^ Mouse entered character area
  | OnMouseEnterEx            -- ^ Extended mouse enter
  | OnMouseLeave              -- ^ Mouse left character area
  | OnMouseLeaveEx            -- ^ Extended mouse leave
  | OnMouseDragStart          -- ^ Started dragging
  | OnMouseDrag               -- ^ Dragging
  | OnMouseDragEnd            -- ^ Finished dragging
  | OnMouseGesture            -- ^ Mouse gesture performed
  
  -- Balloon Events
  | OnBalloonBreak            -- ^ Balloon text interrupted
  | OnBalloonClose            -- ^ Balloon closed
  | OnBalloonTimeout          -- ^ Balloon timed out
  | OnBalloonDoubleClick      -- ^ Balloon double-clicked
  
  -- Install Events
  | OnInstallBegin            -- ^ Installation starting
  | OnInstallComplete         -- ^ Installation completed
  | OnInstallCompleteEx       -- ^ Extended install complete
  | OnInstallFailure          -- ^ Installation failed
  | OnInstallRefuse           -- ^ Installation refused by ghost
  
  -- File Drop Events
  | OnFileDropping            -- ^ File being dragged over
  | OnFileDropped             -- ^ File dropped
  | OnFileDrop2               -- ^ Alternative file drop
  | OnDirectoryDrop           -- ^ Directory dropped
  | OnDirectoryDropping       -- ^ Directory being dragged over
  
  -- Network Update Events
  | OnUpdateBegin             -- ^ Update check starting
  | OnUpdateReady             -- ^ Update ready to download
  | OnUpdateComplete          -- ^ Update completed
  | OnUpdateFailure           -- ^ Update failed
  | OnUpdate                  -- ^ Update in progress
  | OnUpdateOtherBegin        -- ^ Other ghost update starting
  | OnUpdateOtherComplete     -- ^ Other ghost update completed
  | OnUpdateOtherFailure      -- ^ Other ghost update failed
  
  -- Mail/BIFF Events
  | OnBIFFBegin               -- ^ Mail check starting
  | OnBIFFComplete            -- ^ Mail check completed
  | OnBIFFFailure             -- ^ Mail check failed
  | OnBIFF                    -- ^ Mail notification
  
  -- Headline/RSS Events
  | OnHeadlinesenseBegin      -- ^ Headline check starting
  | OnHeadlinesenseComplete   -- ^ Headline check completed
  | OnHeadlinesenseFailure    -- ^ Headline check failed
  | OnHeadlinesense           -- ^ Headline notification
  | OnRSSBegin                -- ^ RSS fetch starting
  | OnRSSComplete             -- ^ RSS fetch completed
  | OnRSSFailure              -- ^ RSS fetch failed
  
  -- Calendar Events
  | OnSchedule5MinutesToGo    -- ^ 5 minutes before scheduled event
  | OnScheduleRead            -- ^ Schedule read
  | OnScheduleInput           -- ^ Schedule input started
  
  -- SSTP Events
  | OnSSTPBreak               -- ^ SSTP connection broken
  | OnSSTPBlacklisting        -- ^ SSTP blacklisting
  
  -- OS/System Events
  | OnScreenSaverStart        -- ^ Screen saver starting
  | OnScreenSaverEnd          -- ^ Screen saver ending
  | OnBatteryNotify           -- ^ Battery status notification
  | OnBatteryCritical         -- ^ Battery critical
  | OnBatteryLow              -- ^ Battery low
  | OnPowerSuspend            -- ^ System suspending
  | OnPowerResume             -- ^ System resuming
  | OnDisplayChange           -- ^ Display settings changed
  | OnNetworkChange           -- ^ Network status changed
  | OnVolumeChange            -- ^ Volume changed
  | OnMuteChange              -- ^ Mute status changed
  | OnWindowStateRestore      -- ^ Window state restored
  | OnWindowStateMinimize     -- ^ Window minimized
  | OnDesktopChange           -- ^ Desktop/workspace changed
  | OnFullscreenAppEnter      -- ^ Fullscreen app started
  | OnFullscreenAppLeave      -- ^ Fullscreen app ended
  | OnEmbryoExist             -- ^ Embryo (nascent ghost) exists
  | OnKeyDown                 -- ^ Key pressed
  | OnKeyUp                   -- ^ Key released
  | OnKeyPress                -- ^ Key typed
  | OnClipboardChange         -- ^ Clipboard changed
  
  -- Plugin Events
  | OnPluginInstall           -- ^ Plugin installed
  | OnPluginUninstall         -- ^ Plugin uninstalled
  | OnPluginRequest           -- ^ Plugin request
  
  -- Third-party/External Events
  | OnSNTPCompare             -- ^ SNTP time comparison
  | OnTranslate               -- ^ Translation request
  | OnNotifyInformation       -- ^ Information notification
  | OnNotifyOSInfo            -- ^ OS information notification
  | OnNotifyUserInfo          -- ^ User information notification
  | OnNotifyDressupInfo       -- ^ Dressup information notification
  | OnNotifyShellInfo         -- ^ Shell information notification
  | OnNotifyBalloonInfo       -- ^ Balloon information notification
  
  -- NOTIFY events (from baseware to ghost)
  | NotifyBasewareVersion     -- ^ Baseware version info
  | NotifyHwnd                -- ^ Window handle
  | NotifyUniqueId            -- ^ Unique ID
  | NotifyCapability          -- ^ Baseware capabilities
  | NotifyOwnerGhostName      -- ^ Owner ghost name
  
  -- Generic/Unknown event
  | OnEvent Text              -- ^ Unknown event with ID
  deriving ( Show, Eq )

-- | Get the category of an event
eventCategory :: ShioriEvent -> ShioriEventCategory
eventCategory e = case e of
  OnFirstBoot           -> CatBoot
  OnBoot                -> CatBoot
  OnClose               -> CatBoot
  OnCloseAll            -> CatBoot
  OnGhostChanged        -> CatBoot
  OnGhostChanging       -> CatBoot
  OnGhostCalled         -> CatBoot
  OnGhostCalling        -> CatBoot
  OnGhostCallComplete   -> CatBoot
  OnOtherGhostBooted    -> CatBoot
  OnOtherGhostClosed    -> CatBoot
  OnOtherGhostChanged   -> CatBoot
  OnShellChanged        -> CatBoot
  OnShellChanging       -> CatBoot
  OnDressupChanged      -> CatBoot
  OnBalloonChange       -> CatBoot
  
  OnSecondChange        -> CatTime
  OnMinuteChange        -> CatTime
  OnHourTimeSignal      -> CatTime
  
  OnTeachStart          -> CatInput
  OnTeach               -> CatInput
  OnCommunicate         -> CatInput
  OnUserInput           -> CatInput
  OnUserInputCancel     -> CatInput
  
  OnSystemDialog              -> CatDialog
  OnConfigurationDialogHelp   -> CatDialog
  
  OnVanishSelecting     -> CatVanish
  OnVanishSelected      -> CatVanish
  OnVanishCancel        -> CatVanish
  OnVanishButtonHold    -> CatVanish
  OnVanished            -> CatVanish
  OnVanishBroadcast     -> CatVanish
  
  OnChoiceSelect        -> CatChoice
  OnChoiceSelectEx      -> CatChoice
  OnChoiceEnter         -> CatChoice
  OnChoiceTimeout       -> CatChoice
  OnChoiceHover         -> CatChoice
  OnAnchorSelect        -> CatChoice
  OnAnchorSelectEx      -> CatChoice
  
  OnSurfaceChange       -> CatSurface
  OnSurfaceRestore      -> CatSurface
  
  OnMouseClick          -> CatMouse
  OnMouseClickEx        -> CatMouse
  OnMouseDoubleClick    -> CatMouse
  OnMouseDoubleClickEx  -> CatMouse
  OnMouseMultiClick     -> CatMouse
  OnMouseMultiClickEx   -> CatMouse
  OnMouseUp             -> CatMouse
  OnMouseUpEx           -> CatMouse
  OnMouseDown           -> CatMouse
  OnMouseDownEx         -> CatMouse
  OnMouseMove           -> CatMouse
  OnMouseWheel          -> CatMouse
  OnMouseEnter          -> CatMouse
  OnMouseEnterEx        -> CatMouse
  OnMouseLeave          -> CatMouse
  OnMouseLeaveEx        -> CatMouse
  OnMouseDragStart      -> CatMouse
  OnMouseDrag           -> CatMouse
  OnMouseDragEnd        -> CatMouse
  OnMouseGesture        -> CatMouse
  
  OnBalloonBreak        -> CatBalloon
  OnBalloonClose        -> CatBalloon
  OnBalloonTimeout      -> CatBalloon
  OnBalloonDoubleClick  -> CatBalloon
  
  OnInstallBegin        -> CatInstall
  OnInstallComplete     -> CatInstall
  OnInstallCompleteEx   -> CatInstall
  OnInstallFailure      -> CatInstall
  OnInstallRefuse       -> CatInstall
  
  OnFileDropping        -> CatFileDrop
  OnFileDropped         -> CatFileDrop
  OnFileDrop2           -> CatFileDrop
  OnDirectoryDrop       -> CatFileDrop
  OnDirectoryDropping   -> CatFileDrop
  
  OnUpdateBegin         -> CatUpdate
  OnUpdateReady         -> CatUpdate
  OnUpdateComplete      -> CatUpdate
  OnUpdateFailure       -> CatUpdate
  OnUpdate              -> CatUpdate
  OnUpdateOtherBegin    -> CatUpdate
  OnUpdateOtherComplete -> CatUpdate
  OnUpdateOtherFailure  -> CatUpdate
  
  OnBIFFBegin           -> CatMail
  OnBIFFComplete        -> CatMail
  OnBIFFFailure         -> CatMail
  OnBIFF                -> CatMail
  
  OnHeadlinesenseBegin    -> CatHeadline
  OnHeadlinesenseComplete -> CatHeadline
  OnHeadlinesenseFailure  -> CatHeadline
  OnHeadlinesense         -> CatHeadline
  OnRSSBegin              -> CatHeadline
  OnRSSComplete           -> CatHeadline
  OnRSSFailure            -> CatHeadline
  
  OnSchedule5MinutesToGo  -> CatCalendar
  OnScheduleRead          -> CatCalendar
  OnScheduleInput         -> CatCalendar
  
  OnSSTPBreak             -> CatSSTP
  OnSSTPBlacklisting      -> CatSSTP
  
  OnScreenSaverStart      -> CatOS
  OnScreenSaverEnd        -> CatOS
  OnBatteryNotify         -> CatOS
  OnBatteryCritical       -> CatOS
  OnBatteryLow            -> CatOS
  OnPowerSuspend          -> CatOS
  OnPowerResume           -> CatOS
  OnDisplayChange         -> CatOS
  OnNetworkChange         -> CatOS
  OnVolumeChange          -> CatOS
  OnMuteChange            -> CatOS
  OnWindowStateRestore    -> CatOS
  OnWindowStateMinimize   -> CatOS
  OnDesktopChange         -> CatOS
  OnFullscreenAppEnter    -> CatOS
  OnFullscreenAppLeave    -> CatOS
  OnEmbryoExist           -> CatOS
  OnKeyDown               -> CatOS
  OnKeyUp                 -> CatOS
  OnKeyPress              -> CatOS
  OnClipboardChange       -> CatOS
  
  OnPluginInstall         -> CatPlugin
  OnPluginUninstall       -> CatPlugin
  OnPluginRequest         -> CatPlugin
  
  OnSNTPCompare           -> CatOther
  OnTranslate             -> CatOther
  OnNotifyInformation     -> CatNotify
  OnNotifyOSInfo          -> CatNotify
  OnNotifyUserInfo        -> CatNotify
  OnNotifyDressupInfo     -> CatNotify
  OnNotifyShellInfo       -> CatNotify
  OnNotifyBalloonInfo     -> CatNotify
  
  NotifyBasewareVersion   -> CatNotify
  NotifyHwnd              -> CatNotify
  NotifyUniqueId          -> CatNotify
  NotifyCapability        -> CatNotify
  NotifyOwnerGhostName    -> CatNotify
  
  OnEvent _               -> CatOther

-- | Convert event to its ID string
eventToId :: ShioriEvent -> Text
eventToId e = case e of
  OnFirstBoot             -> "OnFirstBoot"
  OnBoot                  -> "OnBoot"
  OnClose                 -> "OnClose"
  OnCloseAll              -> "OnCloseAll"
  OnGhostChanged          -> "OnGhostChanged"
  OnGhostChanging         -> "OnGhostChanging"
  OnGhostCalled           -> "OnGhostCalled"
  OnGhostCalling          -> "OnGhostCalling"
  OnGhostCallComplete     -> "OnGhostCallComplete"
  OnOtherGhostBooted      -> "OnOtherGhostBooted"
  OnOtherGhostClosed      -> "OnOtherGhostClosed"
  OnOtherGhostChanged     -> "OnOtherGhostChanged"
  OnShellChanged          -> "OnShellChanged"
  OnShellChanging         -> "OnShellChanging"
  OnDressupChanged        -> "OnDressupChanged"
  OnBalloonChange         -> "OnBalloonChange"
  
  OnSecondChange          -> "OnSecondChange"
  OnMinuteChange          -> "OnMinuteChange"
  OnHourTimeSignal        -> "OnHourTimeSignal"
  
  OnTeachStart            -> "OnTeachStart"
  OnTeach                 -> "OnTeach"
  OnCommunicate           -> "OnCommunicate"
  OnUserInput             -> "OnUserInput"
  OnUserInputCancel       -> "OnUserInputCancel"
  
  OnSystemDialog            -> "OnSystemDialog"
  OnConfigurationDialogHelp -> "OnConfigurationDialogHelp"
  
  OnVanishSelecting       -> "OnVanishSelecting"
  OnVanishSelected        -> "OnVanishSelected"
  OnVanishCancel          -> "OnVanishCancel"
  OnVanishButtonHold      -> "OnVanishButtonHold"
  OnVanished              -> "OnVanished"
  OnVanishBroadcast       -> "OnVanishBroadcast"
  
  OnChoiceSelect          -> "OnChoiceSelect"
  OnChoiceSelectEx        -> "OnChoiceSelectEx"
  OnChoiceEnter           -> "OnChoiceEnter"
  OnChoiceTimeout         -> "OnChoiceTimeout"
  OnChoiceHover           -> "OnChoiceHover"
  OnAnchorSelect          -> "OnAnchorSelect"
  OnAnchorSelectEx        -> "OnAnchorSelectEx"
  
  OnSurfaceChange         -> "OnSurfaceChange"
  OnSurfaceRestore        -> "OnSurfaceRestore"
  
  OnMouseClick            -> "OnMouseClick"
  OnMouseClickEx          -> "OnMouseClickEx"
  OnMouseDoubleClick      -> "OnMouseDoubleClick"
  OnMouseDoubleClickEx    -> "OnMouseDoubleClickEx"
  OnMouseMultiClick       -> "OnMouseMultiClick"
  OnMouseMultiClickEx     -> "OnMouseMultiClickEx"
  OnMouseUp               -> "OnMouseUp"
  OnMouseUpEx             -> "OnMouseUpEx"
  OnMouseDown             -> "OnMouseDown"
  OnMouseDownEx           -> "OnMouseDownEx"
  OnMouseMove             -> "OnMouseMove"
  OnMouseWheel            -> "OnMouseWheel"
  OnMouseEnter            -> "OnMouseEnter"
  OnMouseEnterEx          -> "OnMouseEnterEx"
  OnMouseLeave            -> "OnMouseLeave"
  OnMouseLeaveEx          -> "OnMouseLeaveEx"
  OnMouseDragStart        -> "OnMouseDragStart"
  OnMouseDrag             -> "OnMouseDrag"
  OnMouseDragEnd          -> "OnMouseDragEnd"
  OnMouseGesture          -> "OnMouseGesture"
  
  OnBalloonBreak          -> "OnBalloonBreak"
  OnBalloonClose          -> "OnBalloonClose"
  OnBalloonTimeout        -> "OnBalloonTimeout"
  OnBalloonDoubleClick    -> "OnBalloonDoubleClick"
  
  OnInstallBegin          -> "OnInstallBegin"
  OnInstallComplete       -> "OnInstallComplete"
  OnInstallCompleteEx     -> "OnInstallCompleteEx"
  OnInstallFailure        -> "OnInstallFailure"
  OnInstallRefuse         -> "OnInstallRefuse"
  
  OnFileDropping          -> "OnFileDropping"
  OnFileDropped           -> "OnFileDropped"
  OnFileDrop2             -> "OnFileDrop2"
  OnDirectoryDrop         -> "OnDirectoryDrop"
  OnDirectoryDropping     -> "OnDirectoryDropping"
  
  OnUpdateBegin           -> "OnUpdateBegin"
  OnUpdateReady           -> "OnUpdateReady"
  OnUpdateComplete        -> "OnUpdateComplete"
  OnUpdateFailure         -> "OnUpdateFailure"
  OnUpdate                -> "OnUpdate"
  OnUpdateOtherBegin      -> "OnUpdateOtherBegin"
  OnUpdateOtherComplete   -> "OnUpdateOtherComplete"
  OnUpdateOtherFailure    -> "OnUpdateOtherFailure"
  
  OnBIFFBegin             -> "OnBIFFBegin"
  OnBIFFComplete          -> "OnBIFFComplete"
  OnBIFFFailure           -> "OnBIFFFailure"
  OnBIFF                  -> "OnBIFF"
  
  OnHeadlinesenseBegin    -> "OnHeadlinesenseBegin"
  OnHeadlinesenseComplete -> "OnHeadlinesenseComplete"
  OnHeadlinesenseFailure  -> "OnHeadlinesenseFailure"
  OnHeadlinesense         -> "OnHeadlinesense"
  OnRSSBegin              -> "OnRSSBegin"
  OnRSSComplete           -> "OnRSSComplete"
  OnRSSFailure            -> "OnRSSFailure"
  
  OnSchedule5MinutesToGo  -> "OnSchedule5MinutesToGo"
  OnScheduleRead          -> "OnScheduleRead"
  OnScheduleInput         -> "OnScheduleInput"
  
  OnSSTPBreak             -> "OnSSTPBreak"
  OnSSTPBlacklisting      -> "OnSSTPBlacklisting"
  
  OnScreenSaverStart      -> "OnScreenSaverStart"
  OnScreenSaverEnd        -> "OnScreenSaverEnd"
  OnBatteryNotify         -> "OnBatteryNotify"
  OnBatteryCritical       -> "OnBatteryCritical"
  OnBatteryLow            -> "OnBatteryLow"
  OnPowerSuspend          -> "OnPowerSuspend"
  OnPowerResume           -> "OnPowerResume"
  OnDisplayChange         -> "OnDisplayChange"
  OnNetworkChange         -> "OnNetworkChange"
  OnVolumeChange          -> "OnVolumeChange"
  OnMuteChange            -> "OnMuteChange"
  OnWindowStateRestore    -> "OnWindowStateRestore"
  OnWindowStateMinimize   -> "OnWindowStateMinimize"
  OnDesktopChange         -> "OnDesktopChange"
  OnFullscreenAppEnter    -> "OnFullscreenAppEnter"
  OnFullscreenAppLeave    -> "OnFullscreenAppLeave"
  OnEmbryoExist           -> "OnEmbryoExist"
  OnKeyDown               -> "OnKeyDown"
  OnKeyUp                 -> "OnKeyUp"
  OnKeyPress              -> "OnKeyPress"
  OnClipboardChange       -> "OnClipboardChange"
  
  OnPluginInstall         -> "OnPluginInstall"
  OnPluginUninstall       -> "OnPluginUninstall"
  OnPluginRequest         -> "OnPluginRequest"
  
  OnSNTPCompare           -> "OnSNTPCompare"
  OnTranslate             -> "OnTranslate"
  OnNotifyInformation     -> "OnNotifyInformation"
  OnNotifyOSInfo          -> "OnNotifyOSInfo"
  OnNotifyUserInfo        -> "OnNotifyUserInfo"
  OnNotifyDressupInfo     -> "OnNotifyDressupInfo"
  OnNotifyShellInfo       -> "OnNotifyShellInfo"
  OnNotifyBalloonInfo     -> "OnNotifyBalloonInfo"
  
  NotifyBasewareVersion   -> "basewareversion"
  NotifyHwnd              -> "hwnd"
  NotifyUniqueId          -> "uniqueid"
  NotifyCapability        -> "capability"
  NotifyOwnerGhostName    -> "ownerghostname"
  
  OnEvent eid             -> eid

-- | Parse event from ID string
eventFromId :: Text -> ShioriEvent
eventFromId t = case t of
  "OnFirstBoot"             -> OnFirstBoot
  "OnBoot"                  -> OnBoot
  "OnClose"                 -> OnClose
  "OnCloseAll"              -> OnCloseAll
  "OnGhostChanged"          -> OnGhostChanged
  "OnGhostChanging"         -> OnGhostChanging
  "OnGhostCalled"           -> OnGhostCalled
  "OnGhostCalling"          -> OnGhostCalling
  "OnGhostCallComplete"     -> OnGhostCallComplete
  "OnOtherGhostBooted"      -> OnOtherGhostBooted
  "OnOtherGhostClosed"      -> OnOtherGhostClosed
  "OnOtherGhostChanged"     -> OnOtherGhostChanged
  "OnShellChanged"          -> OnShellChanged
  "OnShellChanging"         -> OnShellChanging
  "OnDressupChanged"        -> OnDressupChanged
  "OnBalloonChange"         -> OnBalloonChange
  
  "OnSecondChange"          -> OnSecondChange
  "OnMinuteChange"          -> OnMinuteChange
  "OnHourTimeSignal"        -> OnHourTimeSignal
  
  "OnTeachStart"            -> OnTeachStart
  "OnTeach"                 -> OnTeach
  "OnCommunicate"           -> OnCommunicate
  "OnUserInput"             -> OnUserInput
  "OnUserInputCancel"       -> OnUserInputCancel
  
  "OnSystemDialog"            -> OnSystemDialog
  "OnConfigurationDialogHelp" -> OnConfigurationDialogHelp
  
  "OnVanishSelecting"       -> OnVanishSelecting
  "OnVanishSelected"        -> OnVanishSelected
  "OnVanishCancel"          -> OnVanishCancel
  "OnVanishButtonHold"      -> OnVanishButtonHold
  "OnVanished"              -> OnVanished
  "OnVanishBroadcast"       -> OnVanishBroadcast
  
  "OnChoiceSelect"          -> OnChoiceSelect
  "OnChoiceSelectEx"        -> OnChoiceSelectEx
  "OnChoiceEnter"           -> OnChoiceEnter
  "OnChoiceTimeout"         -> OnChoiceTimeout
  "OnChoiceHover"           -> OnChoiceHover
  "OnAnchorSelect"          -> OnAnchorSelect
  "OnAnchorSelectEx"        -> OnAnchorSelectEx
  
  "OnSurfaceChange"         -> OnSurfaceChange
  "OnSurfaceRestore"        -> OnSurfaceRestore
  
  "OnMouseClick"            -> OnMouseClick
  "OnMouseClickEx"          -> OnMouseClickEx
  "OnMouseDoubleClick"      -> OnMouseDoubleClick
  "OnMouseDoubleClickEx"    -> OnMouseDoubleClickEx
  "OnMouseMultiClick"       -> OnMouseMultiClick
  "OnMouseMultiClickEx"     -> OnMouseMultiClickEx
  "OnMouseUp"               -> OnMouseUp
  "OnMouseUpEx"             -> OnMouseUpEx
  "OnMouseDown"             -> OnMouseDown
  "OnMouseDownEx"           -> OnMouseDownEx
  "OnMouseMove"             -> OnMouseMove
  "OnMouseWheel"            -> OnMouseWheel
  "OnMouseEnter"            -> OnMouseEnter
  "OnMouseEnterEx"          -> OnMouseEnterEx
  "OnMouseLeave"            -> OnMouseLeave
  "OnMouseLeaveEx"          -> OnMouseLeaveEx
  "OnMouseDragStart"        -> OnMouseDragStart
  "OnMouseDrag"             -> OnMouseDrag
  "OnMouseDragEnd"          -> OnMouseDragEnd
  "OnMouseGesture"          -> OnMouseGesture
  
  "OnBalloonBreak"          -> OnBalloonBreak
  "OnBalloonClose"          -> OnBalloonClose
  "OnBalloonTimeout"        -> OnBalloonTimeout
  "OnBalloonDoubleClick"    -> OnBalloonDoubleClick
  
  "OnInstallBegin"          -> OnInstallBegin
  "OnInstallComplete"       -> OnInstallComplete
  "OnInstallCompleteEx"     -> OnInstallCompleteEx
  "OnInstallFailure"        -> OnInstallFailure
  "OnInstallRefuse"         -> OnInstallRefuse
  
  "OnFileDropping"          -> OnFileDropping
  "OnFileDropped"           -> OnFileDropped
  "OnFileDrop2"             -> OnFileDrop2
  "OnDirectoryDrop"         -> OnDirectoryDrop
  "OnDirectoryDropping"     -> OnDirectoryDropping
  
  "OnUpdateBegin"           -> OnUpdateBegin
  "OnUpdateReady"           -> OnUpdateReady
  "OnUpdateComplete"        -> OnUpdateComplete
  "OnUpdateFailure"         -> OnUpdateFailure
  "OnUpdate"                -> OnUpdate
  "OnUpdateOtherBegin"      -> OnUpdateOtherBegin
  "OnUpdateOtherComplete"   -> OnUpdateOtherComplete
  "OnUpdateOtherFailure"    -> OnUpdateOtherFailure
  
  "OnBIFFBegin"             -> OnBIFFBegin
  "OnBIFFComplete"          -> OnBIFFComplete
  "OnBIFFFailure"           -> OnBIFFFailure
  "OnBIFF"                  -> OnBIFF
  
  "OnHeadlinesenseBegin"    -> OnHeadlinesenseBegin
  "OnHeadlinesenseComplete" -> OnHeadlinesenseComplete
  "OnHeadlinesenseFailure"  -> OnHeadlinesenseFailure
  "OnHeadlinesense"         -> OnHeadlinesense
  "OnRSSBegin"              -> OnRSSBegin
  "OnRSSComplete"           -> OnRSSComplete
  "OnRSSFailure"            -> OnRSSFailure
  
  "OnSchedule5MinutesToGo"  -> OnSchedule5MinutesToGo
  "OnScheduleRead"          -> OnScheduleRead
  "OnScheduleInput"         -> OnScheduleInput
  
  "OnSSTPBreak"             -> OnSSTPBreak
  "OnSSTPBlacklisting"      -> OnSSTPBlacklisting
  
  "OnScreenSaverStart"      -> OnScreenSaverStart
  "OnScreenSaverEnd"        -> OnScreenSaverEnd
  "OnBatteryNotify"         -> OnBatteryNotify
  "OnBatteryCritical"       -> OnBatteryCritical
  "OnBatteryLow"            -> OnBatteryLow
  "OnPowerSuspend"          -> OnPowerSuspend
  "OnPowerResume"           -> OnPowerResume
  "OnDisplayChange"         -> OnDisplayChange
  "OnNetworkChange"         -> OnNetworkChange
  "OnVolumeChange"          -> OnVolumeChange
  "OnMuteChange"            -> OnMuteChange
  "OnWindowStateRestore"    -> OnWindowStateRestore
  "OnWindowStateMinimize"   -> OnWindowStateMinimize
  "OnDesktopChange"         -> OnDesktopChange
  "OnFullscreenAppEnter"    -> OnFullscreenAppEnter
  "OnFullscreenAppLeave"    -> OnFullscreenAppLeave
  "OnEmbryoExist"           -> OnEmbryoExist
  "OnKeyDown"               -> OnKeyDown
  "OnKeyUp"                 -> OnKeyUp
  "OnKeyPress"              -> OnKeyPress
  "OnClipboardChange"       -> OnClipboardChange
  
  "OnPluginInstall"         -> OnPluginInstall
  "OnPluginUninstall"       -> OnPluginUninstall
  "OnPluginRequest"         -> OnPluginRequest
  
  "OnSNTPCompare"           -> OnSNTPCompare
  "OnTranslate"             -> OnTranslate
  "OnNotifyInformation"     -> OnNotifyInformation
  "OnNotifyOSInfo"          -> OnNotifyOSInfo
  "OnNotifyUserInfo"        -> OnNotifyUserInfo
  "OnNotifyDressupInfo"     -> OnNotifyDressupInfo
  "OnNotifyShellInfo"       -> OnNotifyShellInfo
  "OnNotifyBalloonInfo"     -> OnNotifyBalloonInfo
  
  "basewareversion"         -> NotifyBasewareVersion
  "hwnd"                    -> NotifyHwnd
  "uniqueid"                -> NotifyUniqueId
  "capability"              -> NotifyCapability
  "ownerghostname"          -> NotifyOwnerGhostName
  
  other                     -> OnEvent other

--------------------------------------------------------------------------------
-- SHIORI Resources
--------------------------------------------------------------------------------

-- | SHIORI resource identifiers (requested via GET with no ID header)
data ShioriResource
  -- SHIORI info
  = ResVersion           -- ^ version - SHIORI DLL version
  | ResCraftman          -- ^ craftman - SHIORI author name
  | ResCraftmanW         -- ^ craftmanw - SHIORI author website
  | ResName              -- ^ name - SHIORI name
  
  -- Ghost info
  | ResHomeUrl           -- ^ homeurl - Ghost homepage URL
  | ResUsername          -- ^ username - User's name
  | ResBirthday          -- ^ birthday - Ghost's birthday
  | ResDefaultSakuraX    -- ^ sakura.defaultx - Main character default X position
  | ResDefaultSakuraY    -- ^ sakura.defaulty - Main character default Y position
  | ResDefaultKeroX      -- ^ kero.defaultx - Second character default X position
  | ResDefaultKeroY      -- ^ kero.defaulty - Second character default Y position
  | ResRecommendSites    -- ^ sakura.recommendsites - Recommended links
  | ResPortalSites       -- ^ sakura.portalsites - Portal links
  | ResBannerUrl         -- ^ sakura.bannerurl - Banner image URL
  
  -- Menu customization
  | ResMenuSidebarBitmap -- ^ menu.sidebar.bitmap.filename
  | ResMenuBgFontColorR  -- ^ menu.background.font.color.r
  | ResMenuBgFontColorG  -- ^ menu.background.font.color.g
  | ResMenuBgFontColorB  -- ^ menu.background.font.color.b
  | ResMenuFgFontColorR  -- ^ menu.foreground.font.color.r
  | ResMenuFgFontColorG  -- ^ menu.foreground.font.color.g
  | ResMenuFgFontColorB  -- ^ menu.foreground.font.color.b
  | ResMenuBgBitmapFile  -- ^ menu.background.bitmap.filename
  | ResMenuFgBitmapFile  -- ^ menu.foreground.bitmap.filename
  | ResMenuDisableFontColorR  -- ^ menu.disable.font.color.r
  | ResMenuDisableFontColorG  -- ^ menu.disable.font.color.g
  | ResMenuDisableFontColorB  -- ^ menu.disable.font.color.b
  | ResMenuSeperatorColorR    -- ^ menu.separator.color.r
  | ResMenuSeperatorColorG    -- ^ menu.separator.color.g
  | ResMenuSeperatorColorB    -- ^ menu.separator.color.b
  | ResMenuSidebarAlignmentX  -- ^ menu.sidebar.alignment.x
  | ResMenuSidebarAlignmentY  -- ^ menu.sidebar.alignment.y
  
  -- Button captions
  | ResUpdateButtonCaption   -- ^ updatebuttoncaption
  | ResVanishButtonCaption   -- ^ vanishbuttoncaption
  
  -- Unknown resource
  | ResUnknown Text
  deriving ( Show, Eq )

-- | Convert resource to ID string
resourceToId :: ShioriResource -> Text
resourceToId r = case r of
  ResVersion              -> "version"
  ResCraftman             -> "craftman"
  ResCraftmanW            -> "craftmanw"
  ResName                 -> "name"
  ResHomeUrl              -> "homeurl"
  ResUsername             -> "username"
  ResBirthday             -> "birthday"
  ResDefaultSakuraX       -> "sakura.defaultx"
  ResDefaultSakuraY       -> "sakura.defaulty"
  ResDefaultKeroX         -> "kero.defaultx"
  ResDefaultKeroY         -> "kero.defaulty"
  ResRecommendSites       -> "sakura.recommendsites"
  ResPortalSites          -> "sakura.portalsites"
  ResBannerUrl            -> "sakura.bannerurl"
  ResMenuSidebarBitmap    -> "menu.sidebar.bitmap.filename"
  ResMenuBgFontColorR     -> "menu.background.font.color.r"
  ResMenuBgFontColorG     -> "menu.background.font.color.g"
  ResMenuBgFontColorB     -> "menu.background.font.color.b"
  ResMenuFgFontColorR     -> "menu.foreground.font.color.r"
  ResMenuFgFontColorG     -> "menu.foreground.font.color.g"
  ResMenuFgFontColorB     -> "menu.foreground.font.color.b"
  ResMenuBgBitmapFile     -> "menu.background.bitmap.filename"
  ResMenuFgBitmapFile     -> "menu.foreground.bitmap.filename"
  ResMenuDisableFontColorR -> "menu.disable.font.color.r"
  ResMenuDisableFontColorG -> "menu.disable.font.color.g"
  ResMenuDisableFontColorB -> "menu.disable.font.color.b"
  ResMenuSeperatorColorR  -> "menu.separator.color.r"
  ResMenuSeperatorColorG  -> "menu.separator.color.g"
  ResMenuSeperatorColorB  -> "menu.separator.color.b"
  ResMenuSidebarAlignmentX -> "menu.sidebar.alignment.x"
  ResMenuSidebarAlignmentY -> "menu.sidebar.alignment.y"
  ResUpdateButtonCaption  -> "updatebuttoncaption"
  ResVanishButtonCaption  -> "vanishbuttoncaption"
  ResUnknown rid          -> rid

-- | Parse resource from ID string
resourceFromId :: Text -> ShioriResource
resourceFromId t = case t of
  "version"                       -> ResVersion
  "craftman"                      -> ResCraftman
  "craftmanw"                     -> ResCraftmanW
  "name"                          -> ResName
  "homeurl"                       -> ResHomeUrl
  "username"                      -> ResUsername
  "birthday"                      -> ResBirthday
  "sakura.defaultx"               -> ResDefaultSakuraX
  "sakura.defaulty"               -> ResDefaultSakuraY
  "kero.defaultx"                 -> ResDefaultKeroX
  "kero.defaulty"                 -> ResDefaultKeroY
  "sakura.recommendsites"         -> ResRecommendSites
  "sakura.portalsites"            -> ResPortalSites
  "sakura.bannerurl"              -> ResBannerUrl
  "menu.sidebar.bitmap.filename"  -> ResMenuSidebarBitmap
  "menu.background.font.color.r"  -> ResMenuBgFontColorR
  "menu.background.font.color.g"  -> ResMenuBgFontColorG
  "menu.background.font.color.b"  -> ResMenuBgFontColorB
  "menu.foreground.font.color.r"  -> ResMenuFgFontColorR
  "menu.foreground.font.color.g"  -> ResMenuFgFontColorG
  "menu.foreground.font.color.b"  -> ResMenuFgFontColorB
  "menu.background.bitmap.filename" -> ResMenuBgBitmapFile
  "menu.foreground.bitmap.filename" -> ResMenuFgBitmapFile
  "menu.disable.font.color.r"     -> ResMenuDisableFontColorR
  "menu.disable.font.color.g"     -> ResMenuDisableFontColorG
  "menu.disable.font.color.b"     -> ResMenuDisableFontColorB
  "menu.separator.color.r"        -> ResMenuSeperatorColorR
  "menu.separator.color.g"        -> ResMenuSeperatorColorG
  "menu.separator.color.b"        -> ResMenuSeperatorColorB
  "menu.sidebar.alignment.x"      -> ResMenuSidebarAlignmentX
  "menu.sidebar.alignment.y"      -> ResMenuSidebarAlignmentY
  "updatebuttoncaption"           -> ResUpdateButtonCaption
  "vanishbuttoncaption"           -> ResVanishButtonCaption
  other                           -> ResUnknown other

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- | Parse a SHIORI request from text
-- Format:
--   GET SHIORI/3.0\r\n
--   Charset: UTF-8\r\n
--   Sender: SSP\r\n
--   SenderType: internal,raise\r\n
--   SecurityLevel: local\r\n
--   Status: choosing,balloon(0=0)\r\n
--   ID: OnBoot\r\n
--   Reference0: 0\r\n
--   \r\n
parseShioriRequest :: Text -> Either Text ShioriRequest
parseShioriRequest input = do
  let rawLines = T.lines input
      lines' = map (T.dropWhileEnd (== '\r')) rawLines
  case lines' of
    []    -> Left "Empty request"
    (firstLine:rest) -> do
      (method, version) <- parseFirstLine firstLine
      let headers = parseHeaders rest
          charset      = fromMaybe "UTF-8" $ Map.lookup "Charset" headers
          sender       = fromMaybe "" $ Map.lookup "Sender" headers
          senderType   = maybe [] parseSenderTypes $ Map.lookup "SenderType" headers
          securityLvl  = Map.lookup "SecurityLevel" headers >>= parseSecurityLevel
          securityOrig = parseSecurityOrigin <$> Map.lookup "SecurityOrigin" headers
          status       = maybe [] parseGhostStatuses $ Map.lookup "Status" headers
          evId         = fromMaybe "" $ Map.lookup "ID" headers
          baseId       = Map.lookup "BaseID" headers
          refs         = parseReferences headers
          passThru     = parsePassThruHeaders headers
          others       = Map.filterWithKey (\k _ -> not $ isSpecialHeader k) headers
      Right ShioriRequest
        { srqMethod         = method
        , srqVersion        = version
        , srqCharset        = charset
        , srqSender         = sender
        , srqSenderType     = senderType
        , srqSecurityLevel  = securityLvl
        , srqSecurityOrigin = securityOrig
        , srqStatus         = status
        , srqId             = evId
        , srqBaseId         = baseId
        , srqReferences     = refs
        , srqPassThru       = passThru
        , srqHeaders        = others
        }
  where
    parseFirstLine line =
      let parts = T.words line
      in case parts of
        [methodStr, versionStr] -> do
          method <- maybe (Left $ "Unknown method: " <> methodStr) Right
                          (parseShioriMethod methodStr)
          version <- maybe (Left $ "Unknown version: " <> versionStr) Right
                          (parseShioriVersion versionStr)
          Right (method, version)
        _ -> Left $ "Invalid first line: " <> line

    parseHeaders :: [Text] -> Map Text Text
    parseHeaders = foldl' parseHeader Map.empty

    parseHeader :: Map Text Text -> Text -> Map Text Text
    parseHeader acc line =
      case T.breakOn ": " line of
        (key, rest)
          | not (T.null rest) -> Map.insert key (T.drop 2 rest) acc
        _ -> acc

    parseReferences :: Map Text Text -> Map Int Text
    parseReferences = Map.foldlWithKey' go Map.empty
      where
        go acc key val
          | "Reference" `T.isPrefixOf` key =
              case T.stripPrefix "Reference" key of
                Just numStr ->
                  case reads (T.unpack numStr) of
                    [(n, "")] -> Map.insert n val acc
                    _         -> acc
                Nothing -> acc
          | otherwise = acc

    parsePassThruHeaders :: Map Text Text -> Map Text Text
    parsePassThruHeaders = Map.foldlWithKey' go Map.empty
      where
        go acc key val
          | "X-SSTP-PassThru-" `T.isPrefixOf` key =
              case T.stripPrefix "X-SSTP-PassThru-" key of
                Just suffix -> Map.insert suffix val acc
                Nothing     -> acc
          | otherwise = acc

    isSpecialHeader :: Text -> Bool
    isSpecialHeader k = k `elem`
      [ "Charset", "Sender", "SenderType", "SecurityLevel", "SecurityOrigin"
      , "Status", "ID", "BaseID" ]
      || "Reference" `T.isPrefixOf` k
      || "X-SSTP-PassThru-" `T.isPrefixOf` k

-- | Parse a SHIORI response from text
parseShioriResponse :: Text -> Either Text ShioriResponse
parseShioriResponse input = do
  let rawLines = T.lines input
      lines' = map (T.dropWhileEnd (== '\r')) rawLines
  case lines' of
    []    -> Left "Empty response"
    (firstLine:rest) -> do
      (version, status) <- parseFirstLine firstLine
      let headers      = parseHeaders rest
          charset      = fromMaybe "UTF-8" $ Map.lookup "Charset" headers
          sender       = fromMaybe "" $ Map.lookup "Sender" headers
          value        = Map.lookup "Value" headers
          valueNotify  = Map.lookup "ValueNotify" headers
          securityLvl  = Map.lookup "SecurityLevel" headers >>= parseSecurityLevel
          marker       = Map.lookup "Marker" headers
          errorLvl     = maybe [] parseErrorLevels $ Map.lookup "ErrorLevel" headers
          errorDesc    = maybe [] (T.splitOn "\x01") $ Map.lookup "ErrorDescription" headers
          balloonOff   = Map.lookup "BalloonOffset" headers >>= parseBalloonOffset
          age          = Map.lookup "Age" headers >>= readMaybeInt
          markerSend   = Map.lookup "MarkerSend" headers
          refs         = parseReferences headers
          passThru     = parsePassThruHeaders headers
          others       = Map.filterWithKey (\k _ -> not $ isSpecialHeader k) headers
      Right ShioriResponse
        { srsVersion          = version
        , srsStatus           = status
        , srsCharset          = charset
        , srsSender           = sender
        , srsValue            = value
        , srsValueNotify      = valueNotify
        , srsSecurityLevel    = securityLvl
        , srsMarker           = marker
        , srsErrorLevel       = errorLvl
        , srsErrorDescription = errorDesc
        , srsBalloonOffset    = balloonOff
        , srsAge              = age
        , srsMarkerSend       = markerSend
        , srsReferences       = refs
        , srsPassThru         = passThru
        , srsHeaders          = others
        }
  where
    parseFirstLine line = do
      let parts = T.words line
      case parts of
        (versionStr:codeStr:msgParts) -> do
          version <- maybe (Left $ "Unknown version: " <> versionStr) Right
                          (parseShioriVersion versionStr)
          code <- case reads (T.unpack codeStr) of
            [(n, "")] -> Right n
            _         -> Left $ "Invalid status code: " <> codeStr
          let msg = T.unwords msgParts
          Right (version, parseShioriStatusCode code (Just msg))
        _ -> Left $ "Invalid first line: " <> line

    parseHeaders :: [Text] -> Map Text Text
    parseHeaders = foldl' parseHeader Map.empty

    parseHeader :: Map Text Text -> Text -> Map Text Text
    parseHeader acc line =
      case T.breakOn ": " line of
        (key, rest)
          | not (T.null rest) -> Map.insert key (T.drop 2 rest) acc
        _ -> acc

    parseReferences :: Map Text Text -> Map Int Text
    parseReferences = Map.foldlWithKey' go Map.empty
      where
        go acc key val
          | "Reference" `T.isPrefixOf` key =
              case T.stripPrefix "Reference" key of
                Just numStr ->
                  case reads (T.unpack numStr) of
                    [(n, "")] -> Map.insert n val acc
                    _         -> acc
                Nothing -> acc
          | otherwise = acc

    parsePassThruHeaders :: Map Text Text -> Map Text Text
    parsePassThruHeaders = Map.foldlWithKey' go Map.empty
      where
        go acc key val
          | "X-SSTP-PassThru-" `T.isPrefixOf` key =
              case T.stripPrefix "X-SSTP-PassThru-" key of
                Just suffix -> Map.insert suffix val acc
                Nothing     -> acc
          | otherwise = acc

    readMaybeInt :: Text -> Maybe Int
    readMaybeInt t = case reads (T.unpack t) of
      [(n, "")] -> Just n
      _         -> Nothing

    isSpecialHeader :: Text -> Bool
    isSpecialHeader k = k `elem`
      [ "Charset", "Sender", "Value", "ValueNotify", "SecurityLevel"
      , "Marker", "ErrorLevel", "ErrorDescription", "BalloonOffset"
      , "Age", "MarkerSend" ]
      || "Reference" `T.isPrefixOf` k
      || "X-SSTP-PassThru-" `T.isPrefixOf` k

--------------------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------------------

-- | Serialize a SHIORI request to text
serializeShioriRequest :: ShioriRequest -> Text
serializeShioriRequest req = TB.toText $ mconcat
  [ -- First line: METHOD VERSION\r\n
    TB.text (serializeShioriMethod (srqMethod req))
  , TB.char ' '
  , TB.text (serializeShioriVersion (srqVersion req))
  , crlf
    -- Required headers
  , header "Charset" (srqCharset req)
  , header "Sender" (srqSender req)
    -- Optional SenderType
  , if null (srqSenderType req)
      then mempty
      else header "SenderType" (serializeSenderTypes (srqSenderType req))
    -- Optional SecurityLevel
  , maybe mempty (header "SecurityLevel" . serializeSecurityLevel) (srqSecurityLevel req)
    -- Optional SecurityOrigin
  , maybe mempty (header "SecurityOrigin" . serializeSecurityOrigin) (srqSecurityOrigin req)
    -- Optional Status
  , if null (srqStatus req)
      then mempty
      else header "Status" (serializeGhostStatuses (srqStatus req))
    -- Optional ID
  , if T.null (srqId req) then mempty else header "ID" (srqId req)
    -- Optional BaseID
  , maybe mempty (header "BaseID") (srqBaseId req)
    -- References
  , mconcat [ refHeader n v | (n, v) <- Map.toList (srqReferences req) ]
    -- X-SSTP-PassThru headers
  , mconcat [ header ("X-SSTP-PassThru-" <> k) v | (k, v) <- Map.toList (srqPassThru req) ]
    -- Other headers
  , mconcat [ header k v | (k, v) <- Map.toList (srqHeaders req) ]
    -- Empty line to end
  , crlf
  ]

-- | Serialize a SHIORI response to text
serializeShioriResponse :: ShioriResponse -> Text
serializeShioriResponse res = TB.toText $ mconcat
  [ -- First line: VERSION CODE MESSAGE\r\n
    TB.text (serializeShioriVersion (srsVersion res))
  , TB.char ' '
  , TB.decimal (statusCodeNumber (srsStatus res))
  , TB.char ' '
  , TB.text (statusCodeMessage (srsStatus res))
  , crlf
    -- Required headers
  , header "Charset" (srsCharset res)
  , header "Sender" (srsSender res)
    -- Optional Value
  , maybe mempty (header "Value") (srsValue res)
    -- Optional ValueNotify
  , maybe mempty (header "ValueNotify") (srsValueNotify res)
    -- Optional SecurityLevel
  , maybe mempty (header "SecurityLevel" . serializeSecurityLevel) (srsSecurityLevel res)
    -- Optional Marker
  , maybe mempty (header "Marker") (srsMarker res)
    -- Optional ErrorLevel (multiple separated by \x01)
  , if null (srsErrorLevel res)
      then mempty
      else header "ErrorLevel" (serializeErrorLevels (srsErrorLevel res))
    -- Optional ErrorDescription (multiple separated by \x01)
  , if null (srsErrorDescription res)
      then mempty
      else header "ErrorDescription" (T.intercalate "\x01" (srsErrorDescription res))
    -- Optional BalloonOffset
  , maybe mempty (header "BalloonOffset" . serializeBalloonOffset) (srsBalloonOffset res)
    -- Optional Age
  , maybe mempty (header "Age" . T.pack . show) (srsAge res)
    -- Optional MarkerSend
  , maybe mempty (header "MarkerSend") (srsMarkerSend res)
    -- References (for communicate)
  , mconcat [ refHeader n v | (n, v) <- Map.toList (srsReferences res) ]
    -- X-SSTP-PassThru headers
  , mconcat [ header ("X-SSTP-PassThru-" <> k) v | (k, v) <- Map.toList (srsPassThru res) ]
    -- Other headers
  , mconcat [ header k v | (k, v) <- Map.toList (srsHeaders res) ]
    -- Empty line to end
  , crlf
  ]

-- | Build a header line: "Key: Value\r\n"
header :: Text -> Text -> TB.TextBuilder
header key val = TB.text key <> ": " <> TB.text val <> crlf

-- | Build a reference header line: "ReferenceN: Value\r\n"
refHeader :: Int -> Text -> TB.TextBuilder
refHeader n val = "Reference" <> TB.decimal n <> ": " <> TB.text val <> crlf

-- | CRLF line ending
crlf :: TB.TextBuilder
crlf = "\r\n"

--------------------------------------------------------------------------------
-- ByteString Serialization (for IPC)
--------------------------------------------------------------------------------

-- | Serialize a SHIORI request to UTF-8 ByteString
serializeShioriRequestBS :: ShioriRequest -> BS.ByteString
serializeShioriRequestBS = TE.encodeUtf8 . serializeShioriRequest

-- | Parse a SHIORI response from ByteString with charset detection.
-- SHIORI responses should be encoded according to the Charset header.
-- Some SHIORI DLLs (like YAYA) may prepend data before the actual response,
-- so we try to find the "SHIORI/" marker and start parsing from there.
-- For 204 No Content responses, we may need to construct a minimal response.
parseShioriResponseBS :: BS.ByteString -> Either Text ShioriResponse
parseShioriResponseBS bs = 
  -- First, try to filter out any null bytes (some DLLs return UTF-16 artifacts)
  let cleanBs = BS.filter (/= 0) bs
      -- Try to find "SHIORI/" marker (some DLLs prepend garbage)
      shioriMarker = BS8.pack "SHIORI/"
      -- Also try to find status line patterns for corrupted responses
      status200 = BS8.pack "200 OK"
      status204 = BS8.pack "204 No Content"
      alignedBs = case BS.breakSubstring shioriMarker cleanBs of
                    (_, rest) | not (BS.null rest) -> rest
                    _ -> 
                      -- No SHIORI marker, check for status patterns
                      case BS.breakSubstring status204 cleanBs of
                        (prefix, rest) | not (BS.null rest) -> 
                          -- Construct a valid response header
                          BS8.pack "SHIORI/3.0 " <> rest
                        _ -> case BS.breakSubstring status200 cleanBs of
                               (prefix, rest) | not (BS.null rest) ->
                                 BS8.pack "SHIORI/3.0 " <> rest
                               _ -> cleanBs  -- Give up, use original
  in case TE.decodeUtf8' alignedBs of
    Right txt -> parseShioriResponse txt
    Left _    ->
      -- Try with lenient UTF-8 decoding (replace invalid bytes)
      let txt = TE.decodeUtf8With (\_ _ -> Just '?') alignedBs
      in parseShioriResponse txt

-- | Lenient UTF-8 decoder that replaces invalid sequences
lenientDecodeUtf8 :: BS.ByteString -> Text
lenientDecodeUtf8 = TE.decodeUtf8With (\_ _ -> Just '\xFFFD')

--------------------------------------------------------------------------------
-- Request Builders
--------------------------------------------------------------------------------

-- | Create a GET request for an event
mkRequest :: Text -> Text -> Map Int Text -> ShioriRequest
mkRequest sender eventId refs = ShioriRequest
  { srqMethod         = MethodGet
  , srqVersion        = Shiori30
  , srqCharset        = "UTF-8"
  , srqSender         = sender
  , srqSenderType     = [SenderInternal]
  , srqSecurityLevel  = Just SecurityLocal
  , srqSecurityOrigin = Nothing
  , srqStatus         = []
  , srqId             = eventId
  , srqBaseId         = Nothing
  , srqReferences     = refs
  , srqPassThru       = Map.empty
  , srqHeaders        = Map.empty
  }

-- | Create a NOTIFY request
mkNotify :: Text -> Text -> Map Int Text -> ShioriRequest
mkNotify sender eventId refs = ShioriRequest
  { srqMethod         = MethodNotify
  , srqVersion        = Shiori30
  , srqCharset        = "UTF-8"
  , srqSender         = sender
  , srqSenderType     = [SenderInternal]
  , srqSecurityLevel  = Just SecurityLocal
  , srqSecurityOrigin = Nothing
  , srqStatus         = []
  , srqId             = eventId
  , srqBaseId         = Nothing
  , srqReferences     = refs
  , srqPassThru       = Map.empty
  , srqHeaders        = Map.empty
  }

-- | Empty response (204 No Content)
emptyResponse :: ShioriResponse
emptyResponse = ShioriResponse
  { srsVersion          = Shiori30
  , srsStatus           = Status204
  , srsCharset          = "UTF-8"
  , srsSender           = ""
  , srsValue            = Nothing
  , srsValueNotify      = Nothing
  , srsSecurityLevel    = Nothing
  , srsMarker           = Nothing
  , srsErrorLevel       = []
  , srsErrorDescription = []
  , srsBalloonOffset    = Nothing
  , srsAge              = Nothing
  , srsMarkerSend       = Nothing
  , srsReferences       = Map.empty
  , srsPassThru         = Map.empty
  , srsHeaders          = Map.empty
  }
