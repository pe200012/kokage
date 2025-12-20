{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Sakura Script AST types
-- Reference: https://ssp.shillest.net/ukadoc/manual/list_sakura_script.html
module Types.SakuraScript
  ( -- * Top-level script
    Script
  , SakuraScript(..)
    -- * Command categories
  , ScopeCmd(..)
  , SurfaceCmd(..)
  , BalloonCmd(..)
  , WaitCmd(..)
  , ChoiceCmd(..)
  , FontCmd(..)
  , EventCmd(..)
  , SoundCmd(..)
  , OpenCmd(..)
  , MetaCmd(..)
  , EnvVar(..)
    -- * Supporting types
    -- ** Colors
  , Color(..)
    -- ** Font
  , FontSize(..)
  , FontToggle(..)
    -- ** Positioning
  , CursorPos(..)
  , HAlign(..)
  , VAlign(..)
  , DesktopAlign(..)
  , BalloonAlignDir(..)
    -- ** Movement
  , MoveSpec(..)
  , MoveBase(..)
    -- ** Surface & Animation
  , AnimAction(..)
  , BindCategory(..)
  , AlignTarget(..)
  , ScaleTarget(..)
    -- ** Balloon
  , BalloonImageSpec(..)
  , ImageClipping(..)
  , MarkerTarget(..)
  , MarkerProp(..)
  , MarkerValue(..)
  , ShadowStyle(..)
    -- ** Choices
  , ChoiceAction(..)
  , ChoiceTimeout(..)
    -- ** Events
  , NotifyOption(..)
  , TimerRaiseOpt(..)
  , UpdateCmd(..)
    -- ** Sound
  , SoundAction(..)
    -- ** Open commands
  , InputBoxOpt(..)
  , DialogOpt(..)
  , CommunicateOpt(..)
    -- ** Meta commands
  , SetProperty(..)
  , GetProperty(..)
  , ReloadTarget(..)
  , ExecuteCmd(..)
  ) where

import           Data.Text ( Text )

--------------------------------------------------------------------------------
-- Top-level script
--------------------------------------------------------------------------------

-- | A script is a sequence of script elements
type Script = [ SakuraScript ]

-- | A single element in a Sakura Script
data SakuraScript
  = SSText !Text              -- ^ Literal text to display
  | SSScope !ScopeCmd         -- ^ Scope switching: \0, \1, \h, \u, \p[n]
  | SSSurface !SurfaceCmd     -- ^ Surface changes: \s[n], animations
  | SSBalloon !BalloonCmd     -- ^ Balloon control: \b[n], \n, \c, etc.
  | SSWait !WaitCmd           -- ^ Wait commands: \w, \_w, \x, etc.
  | SSChoice !ChoiceCmd       -- ^ Choice commands: \q, \_a, \__q, etc.
  | SSFont !FontCmd           -- ^ Font styling: \f[...], \_l, etc.
  | SSEvent !EventCmd         -- ^ Event control: \e, \-, \![raise], etc.
  | SSSound !SoundCmd         -- ^ Sound playback: \_v, \![sound,...]
  | SSOpen !OpenCmd           -- ^ Open commands: \j, \![open,...]
  | SSMeta !MetaCmd           -- ^ Meta/system commands: \![set,...], etc.
  | SSEnvVar !EnvVar          -- ^ Environment variables: %username, etc.
  | SSEscape !Char            -- ^ Escaped character: \\, \%, etc.
  deriving ( Eq, Show )

--------------------------------------------------------------------------------
-- Scope commands
--------------------------------------------------------------------------------

-- | Scope switching commands
data ScopeCmd
  = ScopeMain                 -- ^ \0, \h - Switch to main character (sakura)
  | ScopeKero                 -- ^ \1, \u - Switch to secondary character (kero)
  | ScopeIndex !Int           -- ^ \p[n] - Switch to character by index
  deriving ( Eq, Show )

--------------------------------------------------------------------------------
-- Surface commands
--------------------------------------------------------------------------------

-- | Surface and animation commands
data SurfaceCmd
  = SurfaceChange !Int                              -- ^ \s[n] - Change to surface n
  | SurfaceChangeAlias !Text                        -- ^ \s[alias] - Change by alias name
  | SurfaceAnim !Int !AnimAction                    -- ^ \i[n,...] - Animation control
  | SurfaceAnimWait !Int                            -- ^ Wait for animation to complete
  | SurfaceAnimClear !(Maybe Int)                   -- ^ Clear animation (Nothing = all)
  | SurfaceAnimPause !(Maybe Int)                   -- ^ Pause animation
  | SurfaceAnimResume !(Maybe Int)                  -- ^ Resume animation
  | SurfaceAnimOffset !Int !Int !Int                -- ^ Animation offset (id, x, y)
  | SurfaceBind !BindCategory !Text !Bool           -- ^ Bind category, part, on/off
  | SurfaceLockRepaint !Bool                        -- ^ Lock/unlock repainting
  | SurfaceAlignment !AlignTarget !DesktopAlign     -- ^ Alignment settings
  | SurfaceScaling !ScaleTarget !Int                -- ^ Scaling settings
  | SurfaceAlpha !Int                               -- ^ Alpha transparency (0-255)
  | SurfaceMove !MoveSpec                           -- ^ Move surface position
  | SurfaceOffset !Int !Int                         -- ^ Offset position (x, y)
  deriving ( Eq, Show )

-- | Animation actions
data AnimAction
  = AnimStart                 -- ^ Start animation
  | AnimStop                  -- ^ Stop animation
  | AnimPause                 -- ^ Pause animation
  | AnimResume                -- ^ Resume animation
  deriving ( Eq, Show )

-- | Bind categories for surface parts
data BindCategory
  = BindClothes               -- ^ Clothes category
  | BindAccessory             -- ^ Accessory category
  | BindOther !Text           -- ^ Other named category
  deriving ( Eq, Show )

-- | Alignment target
data AlignTarget
  = AlignDesktop              -- ^ Align to desktop
  | AlignOwner                -- ^ Align to owner window
  deriving ( Eq, Show )

-- | Scaling target
data ScaleTarget
  = ScaleAll                  -- ^ Scale all
  | ScaleSurface              -- ^ Scale surface only
  | ScaleBalloon              -- ^ Scale balloon only
  deriving ( Eq, Show )

-- | Desktop alignment options
data DesktopAlign
  = AlignLeft                 -- ^ Align left
  | AlignRight                -- ^ Align right
  | AlignTop                  -- ^ Align top
  | AlignBottom               -- ^ Align bottom
  | AlignCenter               -- ^ Center alignment
  | AlignFree                 -- ^ Free positioning
  deriving ( Eq, Show )

--------------------------------------------------------------------------------
-- Move specification
--------------------------------------------------------------------------------

-- | Specification for move commands
data MoveSpec
  = MoveSpec { moveX     :: !CursorPos  -- ^ X position or offset
             , moveY     :: !CursorPos  -- ^ Y position or offset
             , moveTime  :: !(Maybe Int) -- ^ Duration in ms (Nothing = instant)
             , moveBase  :: !MoveBase   -- ^ Base for coordinates
             , moveAsync :: !Bool       -- ^ Whether to run asynchronously
             }
  deriving ( Eq, Show )

-- | Base reference for move coordinates
data MoveBase
  = MoveBaseDesktop           -- ^ Relative to desktop
  | MoveBasePrimary           -- ^ Relative to primary monitor
  | MoveBaseOwner             -- ^ Relative to owner window
  | MoveBaseCurrent           -- ^ Relative to current position
  deriving ( Eq, Show )

--------------------------------------------------------------------------------
-- Balloon commands
--------------------------------------------------------------------------------

-- | Balloon control commands
data BalloonCmd
  = BalloonChange !Int                              -- ^ \b[n] - Change to balloon n
  | BalloonHide                                     -- ^ \_b - Hide balloon
  | BalloonShow                                     -- ^ Show balloon
  | BalloonImage !BalloonImageSpec                  -- ^ \_b[...] - Inline image
  | Newline                                         -- ^ \n - Newline
  | NewlineHalf                                     -- ^ \n[half] - Half newline
  | NewlinePercent !Int                             -- ^ \n[percent,n] - Percent newline
  | Clear                                           -- ^ \c - Clear balloon
  | ClearChars !Int                                 -- ^ \_c - Clear n characters
  | ClearLines !Int                                 -- ^ Clear n lines
  | CursorMove !CursorPos !CursorPos                -- ^ \_l[x,y] - Move cursor
  | AutoScrollDisable                               -- ^ Disable auto-scroll
  | AutoScrollEnable                                -- ^ Enable auto-scroll
  | BalloonOffset !Int !Int                         -- ^ Balloon offset (x, y)
  | BalloonAlign !BalloonAlignDir                   -- ^ Balloon alignment
  | BalloonTimeout !(Maybe Int)                     -- ^ Balloon timeout (Nothing = default)
  | Marker !MarkerTarget !MarkerProp !MarkerValue   -- ^ \_m[...] - Marker styling
  | OnlineModeStart                                 -- ^ Start online mode
  | OnlineModeEnd                                   -- ^ End online mode
  | NoUserBreakStart                                -- ^ \_! - Disable user break
  | NoUserBreakEnd                                  -- ^ \_! - Enable user break
  | VerbatimStart                                   -- ^ \![set,verbatim,true] - Start verbatim
  | VerbatimEnd                                     -- ^ \![set,verbatim,false] - End verbatim
  deriving ( Eq, Show )

-- | Balloon image specification
data BalloonImageSpec
  = BalloonImageSpec
  { biFile :: !Text           -- ^ Image file path
  , biX :: !(Maybe Int)    -- ^ X position
  , biY :: !(Maybe Int)    -- ^ Y position
  , biInline :: !Bool           -- ^ Inline with text
  , biOpaque :: !Bool           -- ^ Opaque rendering
  , biUseSelfAlpha :: !Bool           -- ^ Use image's own alpha
  , biClipping :: !(Maybe ImageClipping) -- ^ Clipping region
  }
  deriving ( Eq, Show )

-- | Image clipping specification
data ImageClipping
  = ImageClipping { clipX :: !Int, clipY :: !Int, clipWidth :: !Int, clipHeight :: !Int }
  deriving ( Eq, Show )

-- | Balloon alignment direction
data BalloonAlignDir
  = BalloonAlignNone          -- ^ No specific alignment
  | BalloonAlignLeft          -- ^ Align to left of character
  | BalloonAlignRight         -- ^ Align to right of character
  deriving ( Eq, Show )

-- | Marker target
data MarkerTarget
  = MarkerCursor              -- ^ Cursor marker
  | MarkerAnchor              -- ^ Anchor marker
  | MarkerChoice              -- ^ Choice marker
  | MarkerChoiceHover         -- ^ Choice hover marker
  deriving ( Eq, Show )

-- | Marker property
data MarkerProp
  = MarkerFont                -- ^ Font property
  | MarkerColor               -- ^ Color property
  | MarkerShadow              -- ^ Shadow property
  deriving ( Eq, Show )

-- | Marker value
data MarkerValue
  = MarkerValueColor !Color   -- ^ Color value
  | MarkerValueShadow !ShadowStyle -- ^ Shadow style value
  | MarkerValueDefault        -- ^ Default value
  deriving ( Eq, Show )

-- | Shadow style
data ShadowStyle
  = ShadowNone                -- ^ No shadow
  | ShadowSimple !Color       -- ^ Simple shadow with color
  | ShadowOffset !Color !Int !Int -- ^ Shadow with offset (color, x, y)
  deriving ( Eq, Show )

--------------------------------------------------------------------------------
-- Position types
--------------------------------------------------------------------------------

-- | Cursor position specification
data CursorPos
  = PosAbsolute !Int          -- ^ Absolute pixel position
  | PosEm !Double             -- ^ Em units
  | PosLineHeight !Double     -- ^ Line height units
  | PosPercent !Double        -- ^ Percentage
  | PosRelative !Int          -- ^ Relative offset in pixels
  | PosUnchanged              -- ^ Keep current position
  deriving ( Eq, Show )

-- | Horizontal alignment
data HAlign
  = HAlignLeft                -- ^ Left alignment
  | HAlignCenter              -- ^ Center alignment
  | HAlignRight               -- ^ Right alignment
  | HAlignDefault             -- ^ Default alignment
  deriving ( Eq, Show )

-- | Vertical alignment
data VAlign
  = VAlignTop                 -- ^ Top alignment
  | VAlignMiddle              -- ^ Middle alignment
  | VAlignBottom              -- ^ Bottom alignment
  | VAlignBaseline            -- ^ Baseline alignment
  | VAlignDefault             -- ^ Default alignment
  deriving ( Eq, Show )

--------------------------------------------------------------------------------
-- Wait commands
--------------------------------------------------------------------------------

-- | Wait and timing commands
data WaitCmd
  = WaitSimple !Int           -- ^ \w[n] - Wait n units (50ms each)
  | WaitMs !Int               -- ^ \_w[n] - Wait n milliseconds
  | WaitUntil !Int            -- ^ \__w[n] - Wait until n ms since script start
  | WaitAnimComplete !Int     -- ^ Wait for animation id to complete
  | ClickWait                 -- ^ \x - Wait for click, then clear
  | ClickWaitNoClear          -- ^ \_q - Wait for click, no clear
  | TimeCriticalStart         -- ^ \t - Start time-critical section
  | TimeCriticalEnd           -- ^ End time-critical section
  | QuickStart                -- ^ \_q - Quick session start
  | QuickEnd                  -- ^ Quick session end
  | SyncStart !Text           -- ^ Start sync scope (named)
  | SyncEnd !Text             -- ^ End sync scope
  | SyncScopes ![ Int ]         -- ^ Sync multiple scopes
  deriving ( Eq, Show )

--------------------------------------------------------------------------------
-- Choice commands
--------------------------------------------------------------------------------

-- | Choice and anchor commands
data ChoiceCmd
  = Choice !Text !ChoiceAction                      -- ^ \q[text,action] - Basic choice
  | ChoiceID !Text !Text !ChoiceAction              -- ^ \q[id,text,action] - Choice with ID
  | ChoiceScript !Text !Text                        -- ^ \__q[text,script] - Script choice
  | ChoiceNoTimeout !Text !ChoiceAction             -- ^ \_q[text,action] - No timeout choice
  | ChoiceTimeout !ChoiceTimeout                    -- ^ Choice with timeout
  | Anchor !Text !Text                              -- ^ \_a[id,text] - Named anchor
  | AnchorEnd                                       -- ^ Anchor end
  | ChoiceBlock ![ ( Text, ChoiceAction ) ]             -- ^ \* - Implicit choice block
  deriving ( Eq, Show )

-- | Choice action specification
data ChoiceAction
  = ChoiceEvent !Text         -- ^ Fire event with ID
  | ChoiceScript' !Text       -- ^ Execute script
  | ChoiceURL !Text           -- ^ Open URL
  | ChoiceOnEvent !Text !Text -- ^ On-event with ref and extra
  deriving ( Eq, Show )

-- | Choice timeout specification
data ChoiceTimeout
  = ChoiceTimeoutSpec { ctTimeout :: !Int      -- ^ Timeout in seconds
                      , ctDefault :: !(Maybe Int) -- ^ Default choice index
                      , ctChoices :: ![ ( Text, ChoiceAction ) ] -- ^ Choices
                      }
  deriving ( Eq, Show )

--------------------------------------------------------------------------------
-- Font commands
--------------------------------------------------------------------------------

-- | Font styling commands
data FontCmd
  = FontAlign !HAlign                               -- ^ \f[align,...] - Text alignment
  | FontVAlign !VAlign                              -- ^ Vertical alignment
  | FontName !Text                                  -- ^ \f[name,...] - Font name
  | FontHeight !FontSize                            -- ^ \f[height,...] - Font size
  | FontColor !Color                                -- ^ \f[color,...] - Font color
  | FontShadowColor !ShadowStyle                    -- ^ Shadow color/style
  | FontBold !FontToggle                            -- ^ Bold on/off
  | FontItalic !FontToggle                          -- ^ Italic on/off
  | FontStrike !FontToggle                          -- ^ Strikethrough on/off
  | FontUnderline !FontToggle                       -- ^ Underline on/off
  | FontSub !FontToggle                             -- ^ Subscript on/off
  | FontSup !FontToggle                             -- ^ Superscript on/off
  | FontDefault                                     -- ^ Reset to default
  | FontDisable !Bool                               -- ^ Disable font rendering
  | FontCursor !Color                               -- ^ \_l - Cursor color
  | FontAnchorNormal !Color                         -- ^ Anchor normal color
  | FontAnchorHover !Color                          -- ^ Anchor hover color
  | FontChoiceNormal !Color                         -- ^ Choice normal color
  | FontChoiceHover !Color                          -- ^ Choice hover color
  deriving ( Eq, Show )

-- | Font size specification
data FontSize
  = FontSizeAbsolute !Int     -- ^ Absolute size in points
  | FontSizeRelative !Int     -- ^ Relative size (+/- points)
  | FontSizePercent !Int      -- ^ Percentage of default
  | FontSizeDefault           -- ^ Default size
  deriving ( Eq, Show )

-- | Font toggle state
data FontToggle
  = ToggleOn                  -- ^ Turn on
  | ToggleOff                 -- ^ Turn off
  | ToggleDefault             -- ^ Reset to default
  | ToggleDisable             -- ^ Disable entirely
  deriving ( Eq, Show )

-- | Color specification
data Color
  = ColorRGB !Int !Int !Int   -- ^ RGB values (0-255 each)
  | ColorRGBPercent !Double !Double !Double -- ^ RGB as percentages
  | ColorHex !Text            -- ^ Hex color (#RRGGBB or #RGB)
  | ColorName !Text           -- ^ Named color
  | ColorDefault              -- ^ Default color
  | ColorDisable              -- ^ Disabled state
  deriving ( Eq, Show )

--------------------------------------------------------------------------------
-- Event commands
--------------------------------------------------------------------------------

-- | Event control commands
data EventCmd
  = EventExit                                       -- ^ \e - End script
  | EventClose                                      -- ^ Close ghost
  | EventScript !Text !Text                         -- ^ \- - Chain to script
  | EventRaise !Text ![ Text ]                        -- ^ \![raise,...] - Raise event
  | EventEmbed !Text                                -- ^ \![embed,...] - Embed script
  | EventNotify !Text ![ Text ] ![ NotifyOption ]       -- ^ \![notify,...] - Send notification
  | EventTimerRaise !Text !Int ![ TimerRaiseOpt ]     -- ^ \![timerraise,...] - Timed event
  | EventTimerCancel !Text                          -- ^ Cancel timer
  | EventUpdate !UpdateCmd                          -- ^ Update commands
  | EventGhostChange !Text                          -- ^ Change ghost
  | EventShellChange !Text                          -- ^ Change shell
  | EventBalloonChange !Text                        -- ^ Change balloon
  | EventVanish                                     -- ^ \![vanish] - Vanish ghost
  deriving ( Eq, Show )

-- | Notify options
data NotifyOption
  = NotifyDirect              -- ^ Direct notification
  | NotifyDelayed             -- ^ Delayed notification
  | NotifyNoWait              -- ^ Don't wait for response
  deriving ( Eq, Show )

-- | Timer raise options
data TimerRaiseOpt
  = TimerRepeat !Int          -- ^ Repeat count
  | TimerPersist              -- ^ Persist across restarts
  deriving ( Eq, Show )

-- | Update commands
data UpdateCmd
  = UpdateCheck               -- ^ Check for updates
  | UpdateCheckAll            -- ^ Check all components
  | UpdateGhost               -- ^ Update ghost
  | UpdateShell               -- ^ Update shell
  | UpdateBalloon             -- ^ Update balloon
  deriving ( Eq, Show )

--------------------------------------------------------------------------------
-- Sound commands
--------------------------------------------------------------------------------

-- | Sound playback commands
data SoundCmd
  = SoundPlay !Text                                 -- ^ \_v[file] - Play sound
  | SoundStop                                       -- ^ \_V - Stop sound
  | SoundAction !SoundAction !Text ![ Text ]          -- ^ \![sound,action,...] - Sound control
  deriving ( Eq, Show )

-- | Sound action types
data SoundAction
  = SoundActionPlay           -- ^ Play sound
  | SoundActionLoop           -- ^ Loop sound
  | SoundActionPause          -- ^ Pause sound
  | SoundActionResume         -- ^ Resume sound
  | SoundActionSoundStop      -- ^ Stop sound
  | SoundActionWait           -- ^ Wait for sound to finish
  deriving ( Eq, Show )

--------------------------------------------------------------------------------
-- Open commands
--------------------------------------------------------------------------------

-- | Open/launch commands
data OpenCmd
  = OpenURL !Text                                   -- ^ \j[url] - Open URL in browser
  | OpenBrowser !Text                               -- ^ \![open,browser,...] - Open browser
  | OpenMailer !Text                                -- ^ Open mail client
  | OpenFile !Text                                  -- ^ Open file with default app
  | OpenEditor !Text                                -- ^ Open in editor
  | OpenInputBox !Text ![ InputBoxOpt ]               -- ^ \![open,inputbox,...] - Input dialog
  | OpenDialog !Text !DialogOpt                     -- ^ \![open,dialog,...] - Dialog box
  | OpenCommunicate !Text !CommunicateOpt           -- ^ \![open,communicate,...] - IPC
  | OpenTeachBox                                    -- ^ Open teach box
  | OpenConfigMenu                                  -- ^ Open config menu
  deriving ( Eq, Show )

-- | Input box options
data InputBoxOpt
  = InputDefault !Text        -- ^ Default value
  | InputMaxLength !Int       -- ^ Maximum input length
  | InputPassword             -- ^ Password field (masked)
  | InputMultiline            -- ^ Multiline input
  deriving ( Eq, Show )

-- | Dialog options
data DialogOpt
  = DialogOK                  -- ^ OK button only
  | DialogOKCancel            -- ^ OK and Cancel buttons
  | DialogYesNo               -- ^ Yes and No buttons
  | DialogYesNoCancel         -- ^ Yes, No, and Cancel buttons
  deriving ( Eq, Show )

-- | Communicate options
data CommunicateOpt
  = CommunicateGhost !Text    -- ^ Target ghost name
  | CommunicateID !Text       -- ^ Target ghost ID
  deriving ( Eq, Show )

--------------------------------------------------------------------------------
-- Meta commands
--------------------------------------------------------------------------------

-- | Meta/system commands
data MetaCmd
  = MetaSet !SetProperty                            -- ^ \![set,...] - Set property
  | MetaGet !GetProperty                            -- ^ \![get,...] - Get property
  | MetaReload !ReloadTarget                        -- ^ \![reload,...] - Reload
  | MetaExecute !ExecuteCmd                         -- ^ \![execute,...] - Execute
  | MetaPassiveMode !Bool                           -- ^ Enable/disable passive mode
  | MetaInductionMode !Bool                         -- ^ Enable/disable induction mode
  | MetaLock !Text                                  -- ^ Lock component
  | MetaUnlock !Text                                -- ^ Unlock component
  deriving ( Eq, Show )

-- | Set property targets
data SetProperty
  = SetAutoscroll !Bool       -- ^ Set autoscroll
  | SetVerbatim !Bool         -- ^ Set verbatim mode
  | SetBalloonTimeout !Int    -- ^ Set balloon timeout
  | SetChoiceTimeout !Int     -- ^ Set choice timeout
  | SetWalkSpeed !Int         -- ^ Set walking speed
  | SetTalkInterval !Int      -- ^ Set talk interval
  | SetProperty' !Text !Text  -- ^ Generic property (name, value)
  deriving ( Eq, Show )

-- | Get property targets
data GetProperty
  = GetYear                   -- ^ Get current year
  | GetMonth                  -- ^ Get current month
  | GetDay                    -- ^ Get current day
  | GetHour                   -- ^ Get current hour
  | GetMinute                 -- ^ Get current minute
  | GetSecond                 -- ^ Get current second
  | GetProperty' !Text        -- ^ Generic property
  deriving ( Eq, Show )

-- | Reload targets
data ReloadTarget
  = ReloadGhost               -- ^ Reload ghost
  | ReloadShell               -- ^ Reload shell
  | ReloadBalloon             -- ^ Reload balloon
  | ReloadPlugin              -- ^ Reload plugins
  | ReloadHeadline            -- ^ Reload headlines
  | ReloadAll                 -- ^ Reload everything
  deriving ( Eq, Show )

-- | Execute commands
data ExecuteCmd
  = ExecuteHTTPGet !Text      -- ^ HTTP GET request
  | ExecuteHTTPPost !Text !Text -- ^ HTTP POST request (url, body)
  | ExecuteFile !Text         -- ^ Execute file
  deriving ( Eq, Show )

--------------------------------------------------------------------------------
-- Environment variables
--------------------------------------------------------------------------------

-- | Environment variable references
data EnvVar
  = -- Time variables
    EnvYear                   -- ^ %year
  | EnvMonth                  -- ^ %month
  | EnvDay                    -- ^ %day
  | EnvHour                   -- ^ %hour
  | EnvMinute                 -- ^ %minute
  | EnvSecond                 -- ^ %second
  | EnvWeekday                -- ^ %weekday
    -- Ghost info
  | EnvSelfname               -- ^ %selfname
  | EnvSelfname2              -- ^ %selfname2 (kero name)
  | EnvKeroname               -- ^ %keroname
  | EnvGhostname              -- ^ %ghostname
  | EnvShellname              -- ^ %shellname
    -- User info
  | EnvUsername               -- ^ %username
  | EnvOS                     -- ^ %os
    -- Screen info
  | EnvScreenWidth            -- ^ %screenwidth
  | EnvScreenHeight           -- ^ %screenheight
    -- Surface info
  | EnvSurface                -- ^ Current surface number
  | EnvSurface0               -- ^ Sakura surface
  | EnvSurface1               -- ^ Kero surface
    -- Custom
  | EnvCustom !Text           -- ^ Custom variable
  deriving ( Eq, Show )
