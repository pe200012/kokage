# Kokage Project Status

## Overview
Kokage is a Haskell Ukagaka/Ghost baseware using GI-Gtk4. Reference implementation: ninix-kagari (Ruby).

## Architecture

### Module Structure

```
src/
├── Types/                    # Pure data types (no IO dependencies)
│   ├── Balloon.hs           # BalloonDescript, font/brush settings
│   ├── Event.hs             # ClickEvent, DragEvent, TimerEvent
│   ├── Ghost.hs             # Re-exports Ghost/* modules
│   ├── Ghost/               # Ghost, Shell, Surface types
│   ├── Install.hs           # Installation types
│   ├── Plugin.hs            # Plugin types
│   ├── SakuraScript.hs      # SakuraScript AST
│   └── Shiori.hs            # SHIORI event types
│
├── Kokage/                   # Runtime modules (IO, GTK dependencies)
│   ├── Animation.hs         # SERIKO animation engine
│   ├── Balloon.hs           # Balloon rendering & state
│   ├── Balloon/
│   │   ├── Types.hs         # BalloonState, BalloonConfig (IORef-based)
│   │   └── Render.hs        # Cairo rendering helpers
│   ├── Character.hs         # Character window management
│   ├── Event.hs             # FRP event network
│   ├── Event/
│   │   ├── Config.hs        # Network configuration types
│   │   └── Shiori.hs        # SHIORI dispatch helpers
│   ├── SakuraScript/
│   │   ├── Parser.hs        # Megaparsec parser
│   │   └── Interpreter.hs   # Script execution
│   └── ...
│
└── Kokage.hs                 # Main entry point
```

### Type Module Convention

| Location | Purpose | Dependencies |
|----------|---------|--------------|
| `Types.*` | Pure data types, parsers | No IO, no GTK |
| `Kokage.*.Types` | Runtime state types | IORef, GTK widgets |

**Rationale**: `Types.*` contains serializable, pure data (configs, AST, descriptors). 
`Kokage.*.Types` contains runtime state with mutable references and GTK widget handles.

Example:
- `Types.Balloon` → `BalloonDescript` (parsed from descript.txt)
- `Kokage.Balloon.Types` → `BalloonState` (runtime window, IORef text, etc.)

## Recent Work

### Context Menu Implementation (Completed)
-   **Module:** `Kokage/Menu.hs` - Context menu for ghost windows.
-   **Features:**
    -   Uses `Gio.Menu` and `Gtk.PopoverMenu` for GTK4 compliance.
    -   MVP items: Quit (`app.quit`), Cancel (`app.cancel`).
    -   Actions registered on `Gtk.Application`.
-   **Integration:**
    -   `Kokage.hs`: Registers actions, creates `GestureClick` (Button 3) for right-click.
    -   `Kokage/Event.hs`: Added `ihRightClick` to `InputHandlers`, `cncContextMenu` to `CharacterNetworkConfig`.
    -   Right-click pops up `PopoverMenu` at click location.
-   **Next Steps:** Add submenus for Ghost, Shell, Balloon switching.

### LRU Image Cache (Completed)
-   **Module:** `Kokage/ImageCache.hs` - Simple LRU cache for loaded pixbufs.
-   **Features:**
    -   Configurable max size (default 64 entries).
    -   Logical timestamp-based LRU eviction.
    -   Thread-safe via IORef with atomic updates.
-   **Integration:** Added `asImageCache` to `AnimationState`, used by `compositeAnimation`.
-   **Result:** Animation overlay images loaded once then cached; eliminates repeated disk I/O per frame.

### Animation Rendering Fix (Completed)
-   **Problem:** Animation test showed static image - overlay surfaces weren't rendering.
-   **Cause:** `compositeAnimation` only looked for surface definitions (e.g., `surface5` in surfaces.txt). 
    Animation patterns often reference raw PNG files (e.g., `surface4000.png`) that don't have definitions.
-   **Fix:** Modified `compositeAnimation` in `Kokage/Animation.hs` to fallback to `loadDefaultSurface` 
    when `findSurfaceById` returns `Nothing`, allowing direct PNG loading.
-   **Result:** Animations now render correctly (verified with `animation-test` tool).

### Animation Verification Tool (Completed)
-   Created `app-animation-test` executable to simulate and verify animation logic.
-   Verified `IntervalAlways` (Surface 5): Runs continuously, looping correctly.
-   Verified `IntervalSometimes` (Surface 0): Triggers randomly.
-   Confirmed that `tickAnimations` logic handles intervals, pattern switching, and waits as expected.

### SHIORI Response Fix (Completed)
**Problem:** Only `OnBoot` responses were displayed; timer and mouse event responses were ignored.
**Solution:**
-   Updated `GlobalNetworkConfig` and `CharacterNetworkConfig` in `Kokage/Event.hs` to include a `ScriptHandler` callback.
-   Modified `setupGlobalNetwork` and `setupCharacterNetwork` to use `sendShioriWithCallback` instead of `sendShioriAndLog` for timers and mouse clicks.
-   Passed `displayScript` from `kokageMain` into the FRP network configuration in `Kokage.hs`.
**Result:** Ghosts can now react to `OnSecondChange`, `OnMinuteChange`, and `OnMouseClick`.

### SERIKO Animation Enhancements (Completed)
-   **Periodic Animations**: Implemented `IntervalPeriodic` support.
    -   Added `asPeriodicState` to `AnimationState` to track timers for periodic animations.
    -   Updated `tickAnimations` to manage persistent timers for animations defined with `periodic,N`.
    -   Implemented cooldown logic: timer counts up when animation is not running, triggers when threshold is reached.

### SERIKO Animation System (Completed)
-   **`Kokage/Animation.hs`**: Animation state and logic module.
-   **`Kokage/Character.hs`**: Integrated animation ticking (50ms interval).
-   **`Kokage.hs`**: Main loop timer integration.
-   **Features**:
    -   Intervals: `always`, `random`, `sometimes`, `rarely`, `runonce`, `periodic`.
    -   Patterns: Frame switching with wait times (including random wait ranges).
    -   Compositing: Dynamic overlay on base surface.

### Balloon Font Settings (Completed)
-   **Styles**: Bold, Italic, Underline, Strikethrough.
-   **Shadows**: `ShadowOffset` and `ShadowOutline` with custom colors.

## Backlog
-   **Context Menu Enhancements**: Add submenus for Ghost, Shell, Balloon switching.
-   **SSTP Support**: Network control interface.
-   **Sound Support**: Audio playback.
-   **Preferences UI**: User configuration.

## Build & Test
```bash
cd /mnt/data/Document/Development/CodeCollection/Haskell/kokage
stack build
cd test-fdr && stack exec kokage
```
