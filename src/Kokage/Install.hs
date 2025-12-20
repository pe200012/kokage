{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | NAR (Nanika ARchive) installation module
-- Handles extraction and installation of .nar archives containing
-- ghosts, shells, balloons, plugins, and other ukagaka content.
module Kokage.Install
  ( -- * Installation
    InstallResult(..)
  , installNar
    -- * Directory Structure
  , BaseDir(..)
  , getInstallDir
  ) where

import           Control.Exception          ( catch, SomeException )
import           Control.Monad              ( forM_, when, unless )
import           Data.Text                  ( Text )
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.ByteString.Lazy       as BL
import           System.Directory           ( createDirectoryIfMissing
                                            , doesDirectoryExist
                                            , doesFileExist
                                            , removeDirectoryRecursive
                                            , removeFile
                                            )
import           System.FilePath            ( (</>)
                                            , takeDirectory
                                            )
import           Codec.Archive.Zip          ( Archive
                                            , Entry
                                            , toArchive
                                            , zEntries
                                            , eRelativePath
                                            , fromEntry
                                            )

import           Types.Install              ( InstallType(..)
                                            , InstallDescript(..)
                                            , DeleteList
                                            )
import           Types.Ghost                ( detectCharsetFromBytes
                                            , convertToUtf8
                                            )

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Base directories for ukagaka installation
data BaseDir
  = BaseDir
  { bdGhost :: FilePath       -- ^ ghost/ directory
  , bdBalloon :: FilePath     -- ^ balloon/ directory
  , bdPlugin :: FilePath      -- ^ plugin/ directory
  , bdHeadline :: FilePath    -- ^ headline/ directory
  , bdCalendar :: FilePath    -- ^ calendar/ directory
  , bdCalendarSkin :: FilePath -- ^ calendar/skin/ directory
  }
  deriving ( Show, Eq )

-- | Result of NAR installation
data InstallResult
  = InstallSuccess Text InstallType FilePath [FilePath]
    -- ^ Success: name, type, install path, bundled content paths
  | InstallFailure Text
    -- ^ Failure: error message
  deriving ( Show, Eq )

--------------------------------------------------------------------------------
-- Installation
--------------------------------------------------------------------------------

-- | Get the installation directory for a given install type
getInstallDir :: BaseDir -> InstallType -> Text -> FilePath
getInstallDir base installType dirName = case installType of
  InstallGhost        -> bdGhost base </> T.unpack dirName
  InstallShell        -> bdGhost base </> T.unpack dirName  -- Shell goes in ghost's dir
  InstallSupplement   -> bdGhost base </> T.unpack dirName  -- Supplement goes in ghost's dir
  InstallBalloon      -> bdBalloon base </> T.unpack dirName
  InstallPlugin       -> bdPlugin base </> T.unpack dirName
  InstallHeadline     -> bdHeadline base </> T.unpack dirName
  InstallCalendar     -> bdCalendar base </> T.unpack dirName
  InstallCalendarSkin -> bdCalendarSkin base </> T.unpack dirName
  InstallPackage      -> ""  -- Package doesn't have a single directory
  InstallUnknown _    -> ""  -- Unknown type

-- | Install a NAR archive
installNar :: BaseDir       -- ^ Base installation directories
           -> FilePath      -- ^ Path to the .nar file
           -> IO InstallResult
installNar base narPath = do
  -- Read and parse the archive
  narExists <- doesFileExist narPath
  if not narExists
    then return $ InstallFailure $ "NAR file not found: " <> T.pack narPath
    else catch (installNarImpl base narPath) handleError
  where
    handleError :: SomeException -> IO InstallResult
    handleError e = return $ InstallFailure $ "Installation failed: " <> T.pack (show e)

-- | Internal implementation of NAR installation
installNarImpl :: BaseDir -> FilePath -> IO InstallResult
installNarImpl base narPath = do
  -- Read the archive
  narBytes <- BL.readFile narPath
  let archive = toArchive narBytes
      entries = zEntries archive

  -- Find and read install.txt
  case findEntry "install.txt" entries of
    Nothing -> return $ InstallFailure "No install.txt found in archive"
    Just installEntry -> do
      -- Parse install.txt from the archive
      let installBytes = fromEntry installEntry
          detectedCharset = detectCharsetFromBytes installBytes
          utf8Bytes = convertToUtf8 detectedCharset installBytes
          installContent = TE.decodeUtf8 (BL.toStrict utf8Bytes)

      -- Parse the install descriptor
      instDesc <- parseInstallContent installContent

      -- Validate required fields
      if T.null (idName instDesc)
        then return $ InstallFailure "install.txt missing required 'name' field"
        else if T.null (idDirectory instDesc) && needsDirectory (idType instDesc)
          then return $ InstallFailure "install.txt missing required 'directory' field"
          else do
            -- Determine target directory
            let targetDir = getInstallDir base (idType instDesc) (idDirectory instDesc)

            if null targetDir && needsDirectory (idType instDesc)
              then return $ InstallFailure $ "Unknown install type: " <> T.pack (show (idType instDesc))
              else do
                -- Handle refresh option (clear existing directory)
                when (idRefresh instDesc) $ do
                  exists <- doesDirectoryExist targetDir
                  when exists $ do
                    -- Keep files matching refreshundeletemask
                    let keepMasks = idRefreshUndeleteMask instDesc
                    if null keepMasks
                      then removeDirectoryRecursive targetDir
                      else clearDirectoryWithMask targetDir keepMasks

                -- Create target directory
                createDirectoryIfMissing True targetDir

                -- Handle delete.txt if present (for network updates)
                case findEntry "delete.txt" entries of
                  Nothing -> return ()
                  Just deleteEntry -> do
                    let deleteBytes = fromEntry deleteEntry
                        deleteCharset = detectCharsetFromBytes deleteBytes
                        deleteUtf8 = convertToUtf8 deleteCharset deleteBytes
                        deleteContent = TE.decodeUtf8 (BL.toStrict deleteUtf8)
                        deleteList = parseDeleteContent deleteContent
                    processDeleteList targetDir deleteList

                -- Extract files to target directory
                extractArchive targetDir archive

                -- Handle bundled content
                bundledPaths <- installBundledContent base archive instDesc

                return $ InstallSuccess (idName instDesc) (idType instDesc) targetDir bundledPaths

-- | Check if install type needs a directory
needsDirectory :: InstallType -> Bool
needsDirectory InstallSupplement = False  -- Supplement merges into existing
needsDirectory InstallPackage    = False  -- Package is special
needsDirectory _                 = True

-- | Find an entry by name in the archive (case-insensitive)
findEntry :: FilePath -> [Entry] -> Maybe Entry
findEntry name entries =
  let nameLower = map toLower name
  in find (\e -> map toLower (eRelativePath e) == nameLower) entries
  where
    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c
    find _ [] = Nothing
    find p (x:xs)
      | p x = Just x
      | otherwise = find p xs

-- | Parse install.txt content into InstallDescript
parseInstallContent :: Text -> IO InstallDescript
parseInstallContent content = do
  let emptyInst = emptyInstallDescript
  return $ foldl' parseLine emptyInst (T.lines content)
  where
    emptyInstallDescript = InstallDescript
      { idCharset = ""
      , idName = ""
      , idType = InstallGhost
      , idDirectory = ""
      , idAccept = Nothing
      , idRefresh = False
      , idRefreshUndeleteMask = []
      , idBalloonDirectory = Nothing
      , idBalloonSourceDirectory = Nothing
      , idPluginDirectory = Nothing
      , idPluginSourceDirectory = Nothing
      , idHeadlineDirectory = Nothing
      , idHeadlineSourceDirectory = Nothing
      , idCalendarDirectory = Nothing
      , idCalendarSourceDirectory = Nothing
      , idCalendarSkinDirectory = Nothing
      , idCalendarSkinSourceDirectory = Nothing
      , idRaw = mempty
      }

    parseLine inst line = case T.breakOn "," line of
      (rawKey, rest)
        | not (T.null rest) ->
            let key = T.toLower (T.strip rawKey)
                val = T.strip (T.drop 1 rest)
            in parseKey inst key val
      _ -> inst

    parseKey inst key val
      | key == "charset" = inst { idCharset = val }
      | key == "name" = inst { idName = val }
      | key == "type" = inst { idType = parseInstallType val }
      | key == "directory" = inst { idDirectory = val }
      | key == "accept" = inst { idAccept = Just val }
      | key == "refresh" = inst { idRefresh = val == "1" || T.toLower val == "true" }
      | key == "refreshundeletemask" = inst { idRefreshUndeleteMask = T.splitOn ":" val }
      | key == "balloon.directory" = inst { idBalloonDirectory = Just val }
      | key == "balloon.source.directory" = inst { idBalloonSourceDirectory = Just val }
      | key == "plugin.directory" = inst { idPluginDirectory = Just val }
      | key == "plugin.source.directory" = inst { idPluginSourceDirectory = Just val }
      | key == "headline.directory" = inst { idHeadlineDirectory = Just val }
      | key == "headline.source.directory" = inst { idHeadlineSourceDirectory = Just val }
      | key == "calendar.directory" = inst { idCalendarDirectory = Just val }
      | key == "calendar.source.directory" = inst { idCalendarSourceDirectory = Just val }
      | key == "calendarskin.directory" = inst { idCalendarSkinDirectory = Just val }
      | key == "calendarskin.source.directory" = inst { idCalendarSkinSourceDirectory = Just val }
      | otherwise = inst

    parseInstallType t = case T.toLower (T.strip t) of
      "ghost"        -> InstallGhost
      "shell"        -> InstallShell
      "supplement"   -> InstallSupplement
      "balloon"      -> InstallBalloon
      "plugin"       -> InstallPlugin
      "headline"     -> InstallHeadline
      "calendar"     -> InstallCalendar
      "calendarskin" -> InstallCalendarSkin
      "package"      -> InstallPackage
      other          -> InstallUnknown other

-- | Parse delete.txt content
parseDeleteContent :: Text -> DeleteList
parseDeleteContent content =
  let normalize = T.replace "\\" "/"
  in filter (not . T.null) $ map (normalize . T.strip) (T.lines content)

-- | Process delete list - remove files/directories
processDeleteList :: FilePath -> DeleteList -> IO ()
processDeleteList baseDir deleteList = forM_ deleteList $ \relativePath -> do
  let fullPath = baseDir </> T.unpack relativePath
  -- Check if it's a directory (ends with /)
  if T.isSuffixOf "/" relativePath
    then do
      exists <- doesDirectoryExist fullPath
      when exists $ removeDirectoryRecursive fullPath
    else do
      exists <- doesFileExist fullPath
      when exists $ removeFile fullPath

-- | Clear directory while keeping files matching masks
clearDirectoryWithMask :: FilePath -> [Text] -> IO ()
clearDirectoryWithMask _dir _masks = do
  -- For simplicity, we'll just not clear if there are keep masks
  -- A proper implementation would need glob matching
  -- TODO: Implement proper glob matching for refreshundeletemask
  return ()

-- | Extract archive to target directory
extractArchive :: FilePath -> Archive -> IO ()
extractArchive targetDir archive = do
  let entries = zEntries archive
  forM_ entries $ \entry -> do
    let relativePath = eRelativePath entry
        -- Skip install.txt, delete.txt, developer_options.txt
        skipFiles = ["install.txt", "delete.txt", "developer_options.txt"]
    unless (map toLower relativePath `elem` map (map toLower) skipFiles) $ do
      let fullPath = targetDir </> normalizePathSeparators relativePath
          parentDir = takeDirectory fullPath
      -- Create parent directories
      createDirectoryIfMissing True parentDir
      -- Check if this is a directory entry (ends with /)
      unless (isDirectoryPath relativePath) $ do
        let content = fromEntry entry
        BL.writeFile fullPath content
  where
    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c
    isDirectoryPath p = not (null p) && last p `elem` ['/', '\\']
    normalizePathSeparators = map (\c -> if c == '\\' then '/' else c)

-- | Install bundled content (balloon, plugin, etc.)
installBundledContent :: BaseDir -> Archive -> InstallDescript -> IO [FilePath]
installBundledContent base archive inst = do
  let entries = zEntries archive
  paths <- sequence
    [ installBundled entries (idBalloonDirectory inst) (idBalloonSourceDirectory inst)
                     (bdBalloon base)
    , installBundled entries (idPluginDirectory inst) (idPluginSourceDirectory inst)
                     (bdPlugin base)
    , installBundled entries (idHeadlineDirectory inst) (idHeadlineSourceDirectory inst)
                     (bdHeadline base)
    , installBundled entries (idCalendarDirectory inst) (idCalendarSourceDirectory inst)
                     (bdCalendar base)
    , installBundled entries (idCalendarSkinDirectory inst) (idCalendarSkinSourceDirectory inst)
                     (bdCalendarSkin base)
    ]
  return $ concat paths
  where
    installBundled :: [Entry] -> Maybe Text -> Maybe Text -> FilePath -> IO [FilePath]
    installBundled _ Nothing _ _ = return []
    installBundled allEntries (Just targetName) sourceDir baseTargetDir = do
      let sourcePath = maybe (T.unpack targetName) T.unpack sourceDir
          targetPath = baseTargetDir </> T.unpack targetName
          -- Find entries under source path
          matchingEntries = filter (isUnderPath sourcePath . eRelativePath) allEntries
      -- Create target directory
      createDirectoryIfMissing True targetPath
      -- Extract matching entries
      forM_ matchingEntries $ \entry -> do
        let relativePath = eRelativePath entry
            -- Strip source prefix and apply to target
            strippedPath = stripPrefix sourcePath relativePath
            fullPath = targetPath </> normalizePathSeparators strippedPath
            parentDir = takeDirectory fullPath
        createDirectoryIfMissing True parentDir
        unless (isDirectoryPath relativePath) $ do
          let content = fromEntry entry
          BL.writeFile fullPath content
      return [targetPath]

    isUnderPath prefix path =
      let prefixLower = map toLower prefix
          pathLower = map toLower path
      in prefixLower `isPrefixOf` pathLower || (prefixLower ++ "/") `isPrefixOf` pathLower

    stripPrefix prefix path =
      let prefixLen = length prefix
          stripped = drop prefixLen path
      in case stripped of
           (c:rest) | c `elem` ['/', '\\'] -> rest
           _ -> stripped

    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

    isDirectoryPath p = not (null p) && last p `elem` ['/', '\\']
    normalizePathSeparators = map (\c -> if c == '\\' then '/' else c)
