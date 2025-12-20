{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | FRP event network and event types for Kokage.
-- Handles GTK event to FRP event conversion and network setup.
module Kokage.Event
  ( -- * Event Types
    ClickEvent(..)
  , CollisionHit(..)
    -- * Network Setup
  , setupNetwork
    -- * Event Handlers
  , handleClick
  ) where

import qualified Data.Text as T

import qualified GI.Gtk as Gtk

import Reactive.Banana.Frameworks
  ( AddHandler
  , MomentIO
  , fromAddHandler
  , reactimate
  )
import Reactive.Banana.GI.Gtk ( signalE0R )

import Types.Ghost ( CollisionRegion(..) )
import Kokage.Collision ( findCollisionAt )


-- | A click event with coordinates.
data ClickEvent = ClickEvent
  { clickX :: !Int
  , clickY :: !Int
  } deriving ( Show, Eq )

-- | Result of a collision hit test.
data CollisionHit
  = HitRegion !ClickEvent !CollisionRegion  -- ^ Hit a named region
  | HitNothing !ClickEvent                  -- ^ Clicked but hit nothing
  deriving ( Show, Eq )

-- | Process a click event against collision regions.
handleClick :: [ CollisionRegion ] -> (Double, Double) -> CollisionHit
handleClick collisions (x, y) =
  let ix = floor x :: Int
      iy = floor y :: Int
      evt = ClickEvent ix iy
  in case findCollisionAt ix iy collisions of
       Just cr -> HitRegion evt cr
       Nothing -> HitNothing evt

-- | Log a collision hit for debugging.
logCollisionHit :: CollisionHit -> IO ()
logCollisionHit (HitRegion evt cr) =
  putStrLn $ "[Click] Hit collision region '"
           <> T.unpack (crName cr)
           <> "' (index " <> show (crIndex cr) <> ") at ("
           <> show (clickX evt) <> ", " <> show (clickY evt) <> ")"
logCollisionHit (HitNothing evt) =
  putStrLn $ "[Click] No collision at ("
           <> show (clickX evt) <> ", " <> show (clickY evt) <> ")"

-- | Set up the FRP network for the window.
-- Handles window close and click events on collision regions.
-- Takes an AddHandler for click events that was set up before the gesture was added to widget.
setupNetwork
  :: Gtk.Window
  -> AddHandler (Double, Double)
  -> [ CollisionRegion ]
  -> MomentIO ()
setupNetwork window clickHandler collisions = do
  -- Create close event - closeRequest returns Bool, we return False to allow close
  closeE <- signalE0R window #closeRequest False

  -- React to close: we could add cleanup actions here if needed
  reactimate $ (return () :: IO ()) <$ closeE

  -- Get click events from the pre-configured handler
  clickE <- fromAddHandler clickHandler

  -- Process clicks and log results
  let hitE = handleClick collisions <$> clickE
  reactimate $ logCollisionHit <$> hitE
