{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kokage.Animation
  ( -- * Animation State
    ActiveAnim(..)
  , AnimationState(..)
  , newAnimationState
    -- * Animation Logic
  , tickAnimations
  , compositeAnimation
  ) where

import           Control.Monad              ( foldM, when )
import           Data.Int                   ( Int32 )
import           Data.IORef                 ( IORef, newIORef, readIORef, writeIORef, modifyIORef' )
import           Data.List                  ( sortBy )
import           Data.Map.Strict            ( Map )
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 ( catMaybes )
import           Data.Ord                   ( comparing )
import qualified Data.Text                  as T
import           System.Random              ( randomRIO )

import qualified GI.GdkPixbuf               as Pixbuf

import           Kokage.Surface             ( compositeSurface, findSurfaceById, loadDefaultSurface )
import           Types.Ghost                ( Animation(..)
                                            , AnimationInterval(..)
                                            , AnimationPattern(..)
                                            , DrawMethod(..)
                                            , Shell(..)
                                            , SurfaceDefinition(..)
                                            , Surfaces(..)
                                            )

-- | A currently running animation instance.
data ActiveAnim = ActiveAnim
  { aaDef        :: !Animation         -- ^ The animation definition
  , aaStepIndex  :: !Int               -- ^ Current pattern index
  , aaWaitLeft   :: !Int               -- ^ Time remaining for current step (ms)
  , aaVisual     :: !Bool              -- ^ Whether the current step is visual (has valid surfaceId)
  }
  deriving ( Show, Eq )

-- | State container for character animations.
data AnimationState = AnimationState
  { asActiveAnims   :: !(IORef [ActiveAnim])       -- ^ Currently active animations
  , asBasePixbuf    :: !(IORef (Maybe Pixbuf.Pixbuf)) -- ^ The base surface image (without animations)
  , asPeriodicState :: !(IORef (Map Int Int))      -- ^ State for periodic animations (AnimID -> Accumulated Time ms)
  }

-- | Create a new empty animation state.
newAnimationState :: IO AnimationState
newAnimationState = do
  animsRef <- newIORef []
  baseRef <- newIORef Nothing
  periodicRef <- newIORef Map.empty
  return AnimationState
    { asActiveAnims = animsRef
    , asBasePixbuf  = baseRef
    , asPeriodicState = periodicRef
    }

-- | Process one tick (typically 50ms) of animations.
-- Updates the list of active animations and determines if a redraw is needed.
-- Also triggers new animations based on intervals (random, etc.).
tickAnimations
  :: Shell                  -- ^ The shell (to look up surfaces)
  -> SurfaceDefinition      -- ^ The current surface definition
  -> [ActiveAnim]           -- ^ Currently active animations
  -> Map Int Int            -- ^ Current periodic timers
  -> Int                    -- ^ Delta time in ms (e.g., 50)
  -> IO ([ActiveAnim], Map Int Int, Bool) -- ^ (New active list, New timers, Needs redraw)
tickAnimations shell surfDef activeAnims currentTimers delta = do
  -- 1. Update existing animations
  (updatedAnims, changed) <- updateActiveAnims activeAnims delta

  -- 2. Check intervals to start new animations
  -- Only check intervals for animations that aren't already running (unless allowed overlap)
  -- For simplicity, we just check all defined animations against the running list IDs
  let runningIds = map (animId . aaDef) updatedAnims
      candidates = filter (\a -> animId a `notElem` runningIds) (sdAnimations surfDef)

  (finalAnims, finalTimers, newStarted) <- foldM (checkInterval delta) (updatedAnims, currentTimers, False) candidates

  return (finalAnims, finalTimers, changed || newStarted)

  where
    -- Check if an animation should start based on its interval
    checkInterval :: Int -> ([ActiveAnim], Map Int Int, Bool) -> Animation -> IO ([ActiveAnim], Map Int Int, Bool)
    checkInterval dt (accAnims, accTimers, started) anim = do
      (shouldStart, newTimers) <- case animInterval anim of
        IntervalAlways -> return (True, accTimers)
        IntervalRunonce -> return (False, accTimers) -- Handled at init
        IntervalNever -> return (False, accTimers)
        IntervalRandom n -> do
          -- 1 in n chance per second.
          -- If dt is 50ms, that's 20 ticks per second.
          -- Probability per tick = 1 / (n * 20) roughly?
          -- Standard logic: 1/n chance per SECOND.
          -- So per tick (50ms), prob is 1 / (n * (1000/dt))
          let ticksPerSec = 1000.0 / fromIntegral dt :: Double
              chance = 1.0 / (fromIntegral n * ticksPerSec)
          r <- randomRIO (0.0, 1.0) :: IO Double
          return (r < chance, accTimers)
        IntervalPeriodic n -> do
          -- N seconds interval
          let aid = animId anim
              current = Map.findWithDefault 0 aid accTimers
              next = current + dt
              limit = n * 1000
          if next >= limit
            then return (True, Map.insert aid 0 accTimers) -- Reset timer and start
            else return (False, Map.insert aid next accTimers) -- Update timer
        IntervalSometimes -> do -- 1/2 per sec
          let ticksPerSec = 1000.0 / fromIntegral dt
              chance = 1.0 / (2.0 * ticksPerSec)
          r <- randomRIO (0.0, 1.0) :: IO Double
          return (r < chance, accTimers)
        IntervalRarely -> do -- 1/4 per sec
          let ticksPerSec = 1000.0 / fromIntegral dt
              chance = 1.0 / (4.0 * ticksPerSec)
          r <- randomRIO (0.0, 1.0) :: IO Double
          return (r < chance, accTimers)
        _ -> return (False, accTimers) -- Others not implemented yet

      if shouldStart
        then do
          -- Create new active animation
          let patterns = animPatterns anim
          if null patterns
            then return (accAnims, newTimers, started)
            else do
              let firstPat = head patterns
                  -- Initial wait for first pattern
                  wait = apWait firstPat
                  
              actualWait <- case apWaitMax firstPat of
                Just maxW -> randomRIO (wait, maxW)
                Nothing   -> return wait
                
              let newAnim = ActiveAnim
                    { aaDef = anim
                    , aaStepIndex = 0
                    , aaWaitLeft = actualWait
                    , aaVisual = apSurfaceId firstPat >= 0 -- -1 means hidden/none
                    }
              return (accAnims ++ [newAnim], newTimers, True)
        else return (accAnims, newTimers, started)

-- | Update list of active animations.
-- Returns (Updated List, Did Visual State Change)
updateActiveAnims :: [ActiveAnim] -> Int -> IO ([ActiveAnim], Bool)
updateActiveAnims anims delta = do
  (newList, changes) <- foldM step ([], False) anims
  return (newList, changes)
  where
    step (acc, changed) anim = do
      let newWait = aaWaitLeft anim - delta
      if newWait <= 0
        then do
          -- Move to next step
          let nextIndex = aaStepIndex anim + 1
              patterns = animPatterns (aaDef anim)
          
          if nextIndex >= length patterns
            then 
              -- Animation finished
              return (acc, changed || aaVisual anim) 
            else do
              -- Setup next pattern
              let nextPat = patterns !! nextIndex
                  baseWait = apWait nextPat
              
              actualWait <- case apWaitMax nextPat of
                Just maxW -> randomRIO (baseWait, maxW)
                Nothing   -> return baseWait
                
              let newAnim = anim 
                    { aaStepIndex = nextIndex
                    , aaWaitLeft = actualWait
                    , aaVisual = apSurfaceId nextPat >= 0
                    }
              -- Visual state changes if we switch patterns (assuming patterns mostly differ)
              return (acc ++ [newAnim], True)
        else do
          -- Continue waiting
          let newAnim = anim { aaWaitLeft = newWait }
          return (acc ++ [newAnim], changed)

-- | Composite active animations onto the base pixbuf.
-- Returns a NEW Pixbuf (does not modify base).
compositeAnimation
  :: Shell
  -> Pixbuf.Pixbuf -- ^ Base pixbuf
  -> [ActiveAnim]  -- ^ Active animations to apply
  -> IO (Maybe Pixbuf.Pixbuf)
compositeAnimation shell basePixbuf anims = do
  -- Filter visible animations and sort by ID (or z-order if available)
  -- For now just use list order (which implies start order) or definition order?
  -- Usually animations are layered by their definition order in surfaces.txt or sorted by ID.
  -- Types.Ghost has SortOrder but we'll stick to simple list order for now.
  let validAnims = filter (\a -> aaStepIndex a < length (animPatterns (aaDef a))) anims
      sortedAnims = sortBy (comparing (animId . aaDef)) validAnims

  -- Clone base pixbuf to draw on
  mDest <- Pixbuf.pixbufCopy basePixbuf
  case mDest of
    Nothing -> return Nothing
    Just dest -> do
      width <- Pixbuf.pixbufGetWidth dest
      height <- Pixbuf.pixbufGetHeight dest

      -- Apply each animation pattern
      foldM_ (\_ anim -> applyPattern shell dest width height anim) () sortedAnims
      
      return $ Just dest

  where
    applyPattern shell dest destW destH anim = do
      let pat = animPatterns (aaDef anim) !! aaStepIndex anim
          surfId = apSurfaceId pat
          x = apX pat
          y = apY pat
          method = apMethod pat

      -- surfId < 0 means no image (just wait/logic), so skip drawing
      when (surfId >= 0) $ do
        -- Load the surface image for the pattern
        -- Animation patterns often reference surface IDs that are just raw PNG files
        -- (e.g., surface4000.png) without a corresponding surface definition.
        -- We try the definition first, then fallback to loading the raw file.
        -- TODO: Cache these pattern images.
        let surfaces = shellSurfaces shell
        mPatPixbuf <- case findSurfaceById surfId surfaces of
          Just patSurfDef -> compositeSurface (shellPath shell) patSurfDef
          Nothing         -> loadDefaultSurface (shellPath shell) surfId

        case mPatPixbuf of
          Nothing -> return ()
          Just patPixbuf -> do
            w <- Pixbuf.pixbufGetWidth patPixbuf
            h <- Pixbuf.pixbufGetHeight patPixbuf
            
            -- Check bounds
            let destX = max 0 (fromIntegral x :: Int32)
                destY = max 0 (fromIntegral y :: Int32)
            
            -- Composite based on method
            -- Currently only supporting Overlay (and Base as Overlay)
            -- TODO: Implement other methods like overlayfast, replace, etc.
            when (destX < destW && destY < destH) $ do
              let renderW = min w (destW - destX)
                  renderH = min h (destH - destY)
              
              Pixbuf.pixbufComposite
                patPixbuf
                dest
                destX
                destY
                renderW
                renderH
                (fromIntegral x)
                (fromIntegral y)
                1.0
                1.0
                Pixbuf.InterpTypeBilinear
                255

    foldM_ :: Monad m => (a -> b -> m a) -> a -> [b] -> m ()
    foldM_ f a xs = foldM f a xs >> return ()
