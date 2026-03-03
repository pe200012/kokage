{-# LANGUAGE OverloadedStrings #-}

module Kokage.Menu
  ( createContextMenu
  , createMenuModel
  , MenuStyle(..)
  , emptyMenuStyle
  , menuStyleFromShellDescript
  , MenuConfig(..)
  , emptyMenuConfig
  , Gtk.PopoverMenu
  ) where

import Prelude ()
import Relude

import qualified Data.Text       as T

import qualified GI.Gdk          as Gdk
import qualified GI.Gio          as Gio
import qualified GI.Gtk          as Gtk

import           System.FilePath ( (</>) )

import           Types.Ghost     ( ShellDescript(..) )

data MenuConfig
  = MenuConfig { mcShells         :: ![ ( Text, Text ) ]
               , mcBalloons       :: ![ ( Text, Text ) ]
               , mcCurrentShell   :: !Text
               , mcCurrentBalloon :: !Text
               , mcIsSticky       :: !Bool
               }
  deriving ( Show, Eq )

emptyMenuConfig :: MenuConfig
emptyMenuConfig
  = MenuConfig { mcShells         = []
               , mcBalloons       = []
               , mcCurrentShell   = ""
               , mcCurrentBalloon = ""
               , mcIsSticky       = False
               }

data MenuStyle
  = MenuStyle
  { msShellPath :: Maybe FilePath
  , msBackgroundBitmapFile :: Maybe Text
  , msForegroundBitmapFile :: Maybe Text
  , msSidebarBitmapFile :: Maybe Text
  , msBackgroundFontColorR :: Maybe Int
  , msBackgroundFontColorG :: Maybe Int
  , msBackgroundFontColorB :: Maybe Int
  , msForegroundFontColorR :: Maybe Int
  , msForegroundFontColorG :: Maybe Int
  , msForegroundFontColorB :: Maybe Int
  , msSeparatorColorR :: Maybe Int
  , msSeparatorColorG :: Maybe Int
  , msSeparatorColorB :: Maybe Int
  , msBackgroundAlignment :: Text
  , msForegroundAlignment :: Text
  , msSidebarAlignment :: Text
  , msFontName :: Maybe Text
  , msFontHeight :: Maybe Int
  }
  deriving ( Show, Eq )

emptyMenuStyle :: MenuStyle
emptyMenuStyle
  = MenuStyle
  { msShellPath = Nothing
  , msBackgroundBitmapFile = Nothing
  , msForegroundBitmapFile = Nothing
  , msSidebarBitmapFile = Nothing
  , msBackgroundFontColorR = Nothing
  , msBackgroundFontColorG = Nothing
  , msBackgroundFontColorB = Nothing
  , msForegroundFontColorR = Nothing
  , msForegroundFontColorG = Nothing
  , msForegroundFontColorB = Nothing
  , msSeparatorColorR = Nothing
  , msSeparatorColorG = Nothing
  , msSeparatorColorB = Nothing
  , msBackgroundAlignment = "lefttop"
  , msForegroundAlignment = "lefttop"
  , msSidebarAlignment = "bottom"
  , msFontName = Nothing
  , msFontHeight = Nothing
  }

menuStyleFromShellDescript :: FilePath -> ShellDescript -> MenuStyle
menuStyleFromShellDescript shellPath desc
  = MenuStyle
  { msShellPath = Just shellPath
  , msBackgroundBitmapFile = shellDescriptMenuBackgroundBitmapFilename desc
  , msForegroundBitmapFile = shellDescriptMenuForegroundBitmapFilename desc
  , msSidebarBitmapFile = shellDescriptMenuSidebarBitmapFilename desc
  , msBackgroundFontColorR = shellDescriptMenuBackgroundFontColorR desc
  , msBackgroundFontColorG = shellDescriptMenuBackgroundFontColorG desc
  , msBackgroundFontColorB = shellDescriptMenuBackgroundFontColorB desc
  , msForegroundFontColorR = shellDescriptMenuForegroundFontColorR desc
  , msForegroundFontColorG = shellDescriptMenuForegroundFontColorG desc
  , msForegroundFontColorB = shellDescriptMenuForegroundFontColorB desc
  , msSeparatorColorR = shellDescriptMenuSeparatorColorR desc
  , msSeparatorColorG = shellDescriptMenuSeparatorColorG desc
  , msSeparatorColorB = shellDescriptMenuSeparatorColorB desc
  , msBackgroundAlignment = shellDescriptMenuBackgroundAlignment desc
  , msForegroundAlignment = shellDescriptMenuForegroundAlignment desc
  , msSidebarAlignment = shellDescriptMenuSidebarAlignment desc
  , msFontName = shellDescriptMenuFontName desc
  , msFontHeight = shellDescriptMenuFontHeight desc
  }

createMenuModel :: MenuConfig -> IO Gio.Menu
createMenuModel config = do
  menu <- Gio.menuNew

  recommendMenu <- Gio.menuNew
  Gio.menuAppend recommendMenu (Just "Placeholder") (Just "app.todo")
  Gio.menuAppendSubmenu menu (Just "Recommend sites") recommendMenu

  portalMenu <- Gio.menuNew
  Gio.menuAppend portalMenu (Just "Placeholder") (Just "app.todo")
  Gio.menuAppendSubmenu menu (Just "Portal sites") portalMenu

  let stickyLabel
        = if mcIsSticky config
          then "Unstick"
          else "Stick"
  Gio.menuAppend menu (Just stickyLabel) (Just "app.stick")

  optionsMenu <- Gio.menuNew
  Gio.menuAppend optionsMenu (Just "Network Update") (Just "app.update")
  Gio.menuAppend optionsMenu (Just "Vanish") (Just "app.vanish")
  Gio.menuAppend optionsMenu (Just "Preferences...") (Just "app.edit_preference")
  Gio.menuAppend optionsMenu (Just "Console") (Just "app.open_console")
  Gio.menuAppend optionsMenu (Just "Ghost Manager") (Just "app.ghost_manager")
  Gio.menuAppend optionsMenu (Just "Script Log") (Just "app.script_log")
  Gio.menuAppend optionsMenu (Just "Input Script") (Just "app.scriptinputbox")
  Gio.menuAppendSubmenu menu (Just "Options") optionsMenu

  changeMenu <- Gio.menuNew
  Gio.menuAppend changeMenu (Just "Placeholder") (Just "app.todo")
  Gio.menuAppendSubmenu menu (Just "Change Ghost") changeMenu

  summonMenu <- Gio.menuNew
  Gio.menuAppend summonMenu (Just "Placeholder") (Just "app.todo")
  Gio.menuAppendSubmenu menu (Just "Call Ghost") summonMenu

  shellMenu <- Gio.menuNew
  populateShellMenu shellMenu (mcShells config) (mcCurrentShell config)
  Gio.menuAppendSubmenu menu (Just "Change Shell") shellMenu

  costumeMenu <- Gio.menuNew
  Gio.menuAppend costumeMenu (Just "Placeholder") (Just "app.todo")
  Gio.menuAppendSubmenu menu (Just "Costume") costumeMenu

  balloonMenu <- Gio.menuNew
  populateBalloonMenu balloonMenu (mcBalloons config) (mcCurrentBalloon config)
  Gio.menuAppendSubmenu menu (Just "Change Balloon") balloonMenu

  infoMenu <- Gio.menuNew
  Gio.menuAppend infoMenu (Just "Usage graph") (Just "app.usage")
  Gio.menuAppend infoMenu (Just "Version") (Just "app.version")
  Gio.menuAppendSubmenu menu (Just "Information") infoMenu

  nekodorifMenu <- Gio.menuNew
  Gio.menuAppend nekodorifMenu (Just "Placeholder") (Just "app.todo")
  Gio.menuAppendSubmenu menu (Just "Nekodorif") nekodorifMenu

  kinokoMenu <- Gio.menuNew
  Gio.menuAppend kinokoMenu (Just "Placeholder") (Just "app.todo")
  Gio.menuAppendSubmenu menu (Just "Kinoko") kinokoMenu

  Gio.menuAppend menu (Just "Close") (Just "app.close")
  Gio.menuAppend menu (Just "Quit") (Just "app.quit")
  Gio.menuAppend menu (Just "Cancel") (Just "app.cancel")

  return menu

populateShellMenu :: Gio.Menu -> [ ( Text, Text ) ] -> Text -> IO ()
populateShellMenu menu shells currentShell = do
  if null shells
    then Gio.menuAppend menu (Just "(No shells available)") Nothing
    else mapM_ addShellItem shells
  where
    addShellItem ( name, path ) = do
      let label
            = if name == currentShell
              then T.concat [ "● ", name ]
              else T.concat [ "  ", name ]
          action = T.concat [ "app.change_shell::", path ]
      Gio.menuAppend menu (Just label) (Just action)

populateBalloonMenu :: Gio.Menu -> [ ( Text, Text ) ] -> Text -> IO ()
populateBalloonMenu menu balloons currentBalloon = do
  if null balloons
    then Gio.menuAppend menu (Just "(No balloons available)") Nothing
    else mapM_ addBalloonItem balloons
  where
    addBalloonItem ( name, path ) = do
      let label
            = if name == currentBalloon
              then T.concat [ "● ", name ]
              else T.concat [ "  ", name ]
          action = T.concat [ "app.change_balloon::", path ]
      Gio.menuAppend menu (Just label) (Just action)

createContextMenu :: Gtk.Window -> MenuStyle -> MenuConfig -> IO Gtk.PopoverMenu
createContextMenu parentWindow style config = do
  menuModel <- createMenuModel config
  popover <- Gtk.popoverMenuNewFromModel (Just menuModel)
  Gtk.widgetSetParent popover parentWindow
  Gtk.popoverSetAutohide popover True
  Gtk.popoverSetHasArrow popover False

  applyMenuStyle popover style

  return popover

applyMenuStyle :: Gtk.PopoverMenu -> MenuStyle -> IO ()
applyMenuStyle _popover style = do
  let css = generateMenuCss style
  if T.null css
    then return ()
    else do
      provider <- Gtk.cssProviderNew
      Gtk.cssProviderLoadFromString provider css
      display <- Gdk.displayGetDefault
      case display of
        Nothing -> return ()
        Just d  -> Gtk.styleContextAddProviderForDisplay d provider 800

generateMenuCss :: MenuStyle -> Text
generateMenuCss style = T.concat [ baseCss, hoverCss ]
  where
    shellPath    = msShellPath style

    bgImageCss   = case ( shellPath, msBackgroundBitmapFile style ) of
      ( Just sp, Just bf ) -> let
          fullPath = T.pack (sp </> T.unpack bf)
          alignCss = alignmentToCss (msBackgroundAlignment style)
        in 
          T.concat
            [ "background-image: url('file://"
            , fullPath
            , "');\n"
            , "background-repeat: repeat-y;\n"
            , "background-position: "
            , alignCss
            , ";\n"
            ]
      _ -> ""

    fontColorCss
      = case ( msBackgroundFontColorR style
             , msBackgroundFontColorG style
             , msBackgroundFontColorB style
             ) of
        ( Just r, Just g, Just b )
          -> T.concat [ "color: rgb(", T.show r, ",", T.show g, ",", T.show b, ");\n" ]
        _ -> ""

    fontCss      = case msFontName style of
      Just name -> T.concat
        [ "font-family: '"
        , name
        , "';\n"
        , maybe "" (\h -> T.concat [ "font-size: ", T.show h, "px;\n" ]) (msFontHeight style)
        ]
      Nothing   -> ""

    hoverCss     = case ( shellPath, msForegroundBitmapFile style ) of
      ( Just sp, Just ff ) -> let
          fullPath = T.pack (sp </> T.unpack ff)
          alignCss = alignmentToCss (msForegroundAlignment style)
          fgColor
            = case ( msForegroundFontColorR style
                   , msForegroundFontColorG style
                   , msForegroundFontColorB style
                   ) of
              ( Just r, Just g, Just b )
                -> T.concat [ "color: rgb(", T.show r, ",", T.show g, ",", T.show b, ");\n" ]
              _ -> ""
        in 
          T.concat
            [ "popover.menu contents modelbutton:hover {\n"
            , "  background-image: url('file://"
            , fullPath
            , "');\n"
            , "  background-repeat: repeat-y;\n"
            , "  background-position: "
            , alignCss
            , ";\n"
            , fgColor
            , "}\n"
            ]
      _ -> ""

    baseCss
      = if T.null bgImageCss && T.null fontColorCss && T.null fontCss
        then ""
        else T.concat [ "popover.menu contents {\n", bgImageCss, fontColorCss, fontCss, "}\n" ]

alignmentToCss :: Text -> Text
alignmentToCss align = case T.toLower align of
  "lefttop" -> "left top"
  "righttop" -> "right top"
  "centertop" -> "center top"
  "leftbottom" -> "left bottom"
  "rightbottom" -> "right bottom"
  "centerbottom" -> "center bottom"
  "top" -> "left top"
  "bottom" -> "left bottom"
  _ -> "left top"
