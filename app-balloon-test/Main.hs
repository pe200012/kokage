{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Simple test program for balloon text rendering.
-- Usage: balloon-test [balloon-path]
-- If no path given, uses a simple default balloon style.
module Main ( main ) where

import Prelude ()
import Relude hiding (on)

import qualified Data.Text                  as T

import           Data.GI.Base               ( AttrOp((:=)), new, on )
import qualified GI.Gio                     as Gio
import qualified GI.GLib                    as GLib
import qualified GI.Gtk                     as Gtk

import           Kokage.Balloon             ( BalloonChoice(..)
                                            , BalloonChoice(..)
                                            , newBalloonState
                                            , showBalloon
                                            , appendText
                                            , appendChar
                                            , appendNewline
                                            , clearBalloon
                                            , setFontSize
                                            , setFontColor
                                            , setFontBold
                                            , setFontItalic
                                            , setFontUnderline
                                            , resetFont
                                            , addChoice
                                            , clearChoices
                                            , setChoiceCallback
                                            , setBalloonPosition
                                            )

main :: IO ()
main = do
  app <- new Gtk.Application
    [ #applicationId := "com.kokage.balloon-test"
    , #flags := [ Gio.ApplicationFlagsFlagsNone ]
    ]

  _ <- on app #activate $ do
    args <- getArgs
    case args of
      [balloonPath] -> runBalloonTest app (Just balloonPath)
      []            -> runBalloonTest app Nothing
      _             -> do
        putStrLn "Usage: balloon-test [balloon-surface-path]"
        putStrLn "  balloon-surface-path: Path to balloon surface directory (optional)"

  _ <- Gio.applicationRun app Nothing
  return ()

runBalloonTest :: Gtk.Application -> Maybe FilePath -> IO ()
runBalloonTest app _mBalloonPath = do
  putStrLn "=== Balloon Text Rendering Test ==="
  putStrLn ""
  
  -- Create balloon state
  balloon <- do
    putStrLn "Using default balloon style"
    newBalloonState app
  
  -- Set up choice callback
  setChoiceCallback balloon $ \choice -> do
    putStrLn $ "[Choice Selected] " <> T.unpack (bcText choice) <> " -> " <> T.unpack (bcId choice)
    hFlush stdout
  
  -- Position balloon
  setBalloonPosition balloon 100 100
  showBalloon balloon
  
  -- Create control window
  controlWindow <- new Gtk.Window
    [ #application := app
    , #title := "Balloon Test Controls"
    , #defaultWidth := 400
    , #defaultHeight := 300
    ]
  
  -- Create vertical box for controls
  vbox <- new Gtk.Box
    [ #orientation := Gtk.OrientationVertical
    , #spacing := 10
    , #marginTop := 10
    , #marginBottom := 10
    , #marginStart := 10
    , #marginEnd := 10
    ]
  
  -- Test counter
  testCounterRef <- newIORef (1 :: Int)
  
  -- Helper to run a test
  let runTest :: T.Text -> IO () -> IO ()
      runTest name action = do
        n <- readIORef testCounterRef
        writeIORef testCounterRef (n + 1)
        putStrLn $ "[Test " <> show n <> "] " <> T.unpack name
        hFlush stdout
        action
  
  -- Button: Basic Text
  btnBasicText <- new Gtk.Button [ #label := "Test: Basic Text" ]
  _ <- on btnBasicText #clicked $ runTest "Basic Text" $ do
    clearBalloon balloon
    clearChoices balloon
    appendText balloon "Hello, World!"
    appendNewline balloon
    appendText balloon "This is a basic text rendering test."
  Gtk.boxAppend vbox btnBasicText
  
  -- Button: Japanese Text
  btnJapanese <- new Gtk.Button [ #label := "Test: Japanese Text" ]
  _ <- on btnJapanese #clicked $ runTest "Japanese Text" $ do
    clearBalloon balloon
    clearChoices balloon
    appendText balloon "こんにちは！"
    appendNewline balloon
    appendText balloon "日本語のテキストレンダリングテストです。"
    appendNewline balloon
    appendText balloon "絵文字も試してみましょう：🎉✨🌸"
  Gtk.boxAppend vbox btnJapanese
  
  -- Button: Font Styles
  btnFontStyles <- new Gtk.Button [ #label := "Test: Font Styles" ]
  _ <- on btnFontStyles #clicked $ runTest "Font Styles" $ do
    clearBalloon balloon
    clearChoices balloon
    appendText balloon "Normal text, "
    setFontBold balloon True
    appendText balloon "Bold text, "
    setFontBold balloon False
    setFontItalic balloon True
    appendText balloon "Italic text, "
    setFontItalic balloon False
    setFontUnderline balloon True
    appendText balloon "Underlined text, "
    setFontUnderline balloon False
    setFontBold balloon True
    setFontItalic balloon True
    appendText balloon "Bold+Italic"
    resetFont balloon
    appendNewline balloon
    appendNewline balloon
    -- Strikethrough
    appendText balloon "Normal, "
    setFontBold balloon True
    setFontUnderline balloon True
    appendText balloon "Bold+Underline, "
    resetFont balloon
    appendNewline balloon
    appendNewline balloon
    -- Font sizes
    setFontSize balloon 24
    appendText balloon "Large"
    resetFont balloon
    appendText balloon " Normal "
    setFontSize balloon 10
    appendText balloon "Small"
    resetFont balloon
  Gtk.boxAppend vbox btnFontStyles
  
  -- Button: Font Colors
  btnFontColors <- new Gtk.Button [ #label := "Test: Font Colors" ]
  _ <- on btnFontColors #clicked $ runTest "Font Colors" $ do
    clearBalloon balloon
    clearChoices balloon
    setFontColor balloon 255 0 0
    appendText balloon "Red "
    setFontColor balloon 0 255 0
    appendText balloon "Green "
    setFontColor balloon 0 0 255
    appendText balloon "Blue "
    setFontColor balloon 255 165 0
    appendText balloon "Orange "
    setFontColor balloon 128 0 128
    appendText balloon "Purple"
    resetFont balloon
  Gtk.boxAppend vbox btnFontColors
  
  -- Button: Choices
  btnChoices <- new Gtk.Button [ #label := "Test: Choices" ]
  _ <- on btnChoices #clicked $ runTest "Choices" $ do
    clearBalloon balloon
    clearChoices balloon
    appendText balloon "Please select an option:"
    appendNewline balloon
    appendNewline balloon
    addChoice balloon (BalloonChoice "Option A" "choice_a" "choice_a")
    addChoice balloon (BalloonChoice "Option B" "choice_b" "choice_b")
    addChoice balloon (BalloonChoice "Option C" "choice_c" "choice_c")
  Gtk.boxAppend vbox btnChoices
  
  -- Button: Character-by-character
  btnCharByChar <- new Gtk.Button [ #label := "Test: Character Animation" ]
  charAnimRunningRef <- newIORef False
  _ <- on btnCharByChar #clicked $ do
    running <- readIORef charAnimRunningRef
    if running
      then putStrLn "[Test] Animation already running"
      else do
        runTest "Character Animation" $ do
          clearBalloon balloon
          clearChoices balloon
          writeIORef charAnimRunningRef True
          
          let text = "This text appears one character at a time..."
              chars = T.unpack text
              
          -- Schedule character-by-character display
          counterRef <- newIORef (0 :: Int)
          void $ GLib.timeoutAdd GLib.PRIORITY_DEFAULT 50 $ do
            i <- readIORef counterRef
            if i < length chars
              then do
                case chars !!? i of
                  Just ch -> do
                    appendChar balloon ch
                    writeIORef counterRef (i + 1)
                    return True
                  Nothing -> do
                    -- Should not happen due to bounds check above, but handle defensively
                    writeIORef charAnimRunningRef False
                    putStrLn "[Test] Character animation: index out of bounds"
                    hFlush stdout
                    return False
              else do
                writeIORef charAnimRunningRef False
                putStrLn "[Test] Character animation complete"
                hFlush stdout
                return False
  Gtk.boxAppend vbox btnCharByChar
  
  -- Button: Long Text (scrolling)
  btnLongText <- new Gtk.Button [ #label := "Test: Long Text (Scroll)" ]
  _ <- on btnLongText #clicked $ runTest "Long Text" $ do
    clearBalloon balloon
    clearChoices balloon
    forM_ [1..20 :: Int] $ \i -> do
      appendText balloon $ T.pack $ "Line " <> show i <> ": Lorem ipsum dolor sit amet"
      appendNewline balloon
  Gtk.boxAppend vbox btnLongText
  
  -- Button: Clear
  btnClear <- new Gtk.Button [ #label := "Clear Balloon" ]
  _ <- on btnClear #clicked $ do
    putStrLn "[Action] Clear balloon"
    hFlush stdout
    clearBalloon balloon
    clearChoices balloon
  Gtk.boxAppend vbox btnClear
  
  -- Button: Quit
  btnQuit <- new Gtk.Button [ #label := "Quit" ]
  _ <- on btnQuit #clicked $ do
    putStrLn "[Action] Quit"
    hFlush stdout
    Gio.applicationQuit app
  Gtk.boxAppend vbox btnQuit
  
  -- Add separator
  separator <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal ]
  Gtk.boxAppend vbox separator
  
  -- Instructions label
  infoLabel <- new Gtk.Label
    [ #label := "Click buttons to test different balloon rendering features.\nChoice selections are logged to console."
    , #wrap := True
    ]
  Gtk.boxAppend vbox infoLabel
  
  Gtk.windowSetChild controlWindow (Just vbox)
  Gtk.windowPresent controlWindow
  
  putStrLn ""
  putStrLn "Control window opened. Click buttons to run tests."
  putStrLn "Choice selections will be printed to console."
  putStrLn ""
  hFlush stdout
