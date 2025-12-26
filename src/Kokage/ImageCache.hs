{-# LANGUAGE OverloadedStrings #-}

-- | Simple LRU cache for loaded pixbufs.
-- Avoids repeated disk I/O for animation overlays.
module Kokage.ImageCache
  ( ImageCache
  , newImageCache
  , getCachedImage
  , clearCache
  ) where

import           Data.IORef           ( IORef, newIORef, readIORef, writeIORef, atomicModifyIORef' )
import           Data.Map.Strict      ( Map )
import qualified Data.Map.Strict      as Map
import           Data.List            ( sortBy )
import           Data.Ord             ( comparing, Down(..) )

import qualified GI.GdkPixbuf         as Pixbuf

-- | Cache entry with access time for LRU eviction
data CacheEntry = CacheEntry
  { cePixbuf    :: !Pixbuf.Pixbuf
  , ceAccessTime :: !Int  -- Logical timestamp (increments on each access)
  }

-- | LRU image cache
data ImageCache = ImageCache
  { icEntries   :: !(IORef (Map FilePath CacheEntry))
  , icTime      :: !(IORef Int)      -- Current logical time
  , icMaxSize   :: !Int              -- Maximum entries before eviction
  }

-- | Create a new image cache with the given maximum size
newImageCache :: Int -> IO ImageCache
newImageCache maxSize = do
  entries <- newIORef Map.empty
  time <- newIORef 0
  return ImageCache
    { icEntries = entries
    , icTime = time
    , icMaxSize = maxSize
    }

-- | Get an image from cache, or load it using the provided loader.
-- Updates access time on hit, evicts LRU entries if cache is full.
getCachedImage
  :: ImageCache
  -> FilePath
  -> IO (Maybe Pixbuf.Pixbuf)  -- ^ Loader function
  -> IO (Maybe Pixbuf.Pixbuf)
getCachedImage cache path loader = do
  -- Increment logical time
  newTime <- atomicModifyIORef' (icTime cache) (\t -> (t + 1, t + 1))
  
  -- Check cache
  entries <- readIORef (icEntries cache)
  case Map.lookup path entries of
    Just entry -> do
      -- Cache hit: update access time
      let updated = Map.insert path (entry { ceAccessTime = newTime }) entries
      writeIORef (icEntries cache) updated
      return $ Just (cePixbuf entry)
    
    Nothing -> do
      -- Cache miss: load image
      mPixbuf <- loader
      case mPixbuf of
        Nothing -> return Nothing
        Just pixbuf -> do
          -- Evict if necessary
          currentEntries <- readIORef (icEntries cache)
          let entriesAfterEvict = 
                if Map.size currentEntries >= icMaxSize cache
                  then evictLRU currentEntries
                  else currentEntries
          
          -- Insert new entry
          let newEntry = CacheEntry { cePixbuf = pixbuf, ceAccessTime = newTime }
              finalEntries = Map.insert path newEntry entriesAfterEvict
          writeIORef (icEntries cache) finalEntries
          return $ Just pixbuf

-- | Evict the least recently used entry
evictLRU :: Map FilePath CacheEntry -> Map FilePath CacheEntry
evictLRU entries
  | Map.null entries = entries
  | otherwise = 
      let sorted = sortBy (comparing (ceAccessTime . snd)) (Map.toList entries)
      in case sorted of
           []        -> entries
           ((k,_):_) -> Map.delete k entries

-- | Clear all entries from the cache
clearCache :: ImageCache -> IO ()
clearCache cache = writeIORef (icEntries cache) Map.empty
