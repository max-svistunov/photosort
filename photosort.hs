import System.Environment
import System.IO
import System.Console.GetOpt
import System.Posix.Files
import System.Posix.Types
import System.Directory
import System.FilePath
import Data.Char
import Data.List
import Data.Time
import Data.Time.Format
import Data.Maybe
import Data.Function
import Control.Monad
import Control.Exception
import Text.Printf

data Flag = Target | DryRun | CleanUp | Dir String | Help deriving (Eq, Show)

type Path = String
type Event = (UTCTime, UTCTime, Path)

helpHeader = "Sort photographs (.jpg, .jpeg) from dir SRC to dir DST according to configuration in SRC/photosort.conf.\nUsage: ./photosort [OPTION...] SRC DST"

availableOptions :: [OptDescr Flag]
availableOptions =
  [Option ['n'] ["dry-run"] (NoArg DryRun) "do not actually move files, only print what would happen",
   Option ['t'] ["target"] (ReqArg Dir "DIR") "use DIR as the destination",
   Option ['c'] ["cleanup"] (NoArg CleanUp) "remove empty directories in SRC once finished",
   Option ['h'] ["help"] (NoArg Help) "show this help"]

programOptions :: [String] -> IO ([Flag], [String])
programOptions args =
  case getOpt Permute availableOptions args of
    (availableOptions, dirpaths, []) -> return (availableOptions, dirpaths)
    (_, _, errors) -> ioError (userError (concat errors ++ usageInfo helpHeader availableOptions))

getOptionsAndArguments :: IO ([String], String, [Flag])
getOptionsAndArguments = do
  -- Gathering arguments
  args <- getArgs
  (options, dirpaths) <- programOptions(args)

  -- Handling the -t DIR option for specifying the target dir
  let isTargetDirSpecifier option = case option of
                                      (Dir _) -> True
                                      _ -> False
  let optionSpecifyingTargetDir = find isTargetDirSpecifier options

  -- Assigning the sourceDirs and the targetDir depending on -t DIR option
  let (sourceDirs, targetDir) = if optionSpecifyingTargetDir == Nothing
                                   then (init dirpaths, last dirpaths)
                     else (dirpaths, targetDirFromOption)
                       where targetDirFromOption = (\ (Just (Dir path)) -> path) optionSpecifyingTargetDir
  pure (sourceDirs, targetDir, options)

configLineParser :: String -> Maybe (UTCTime, UTCTime, Path)
configLineParser line =
  let -- Time parser:
      parseTime timeStr = parseTimeM True defaultTimeLocale "%Y-%-m-%-d %R" timeStr :: Maybe UTCTime;
      -- Skip empty lines and comment lines:
      skipThisLine = line == "" || head line == '#';
      -- Split the line into starting time, ending time and dir path:
      (start, end, dirPath) = if skipThisLine
                               then ("", "", "")
                               else (list!!0 ++ " " ++ list!!1,
                               list!!2 ++ " " ++ list!!3,
                                    list!!4)
                                 where list = words line;
      -- Parse the line. Returning Nothing unless parsed a correct line:
      parsedStart = if skipThisLine then Nothing else parseTime start;
      parsedEnd = if skipThisLine then Nothing else parseTime end;
      parseFailed = parsedStart == Nothing || parsedEnd == Nothing;
      unJust (Just x) = x in
   if skipThisLine || parseFailed then Nothing
                                  else Just (unJust parsedStart, unJust parsedEnd, dirPath)

readConfigFile :: Path -> IO [Event]
readConfigFile photoDir = do
          contents <- readFile (photoDir ++ "/photosort.conf")
          let maybeParsedEvents = map configLineParser (lines contents)
          let justParsedEvents = filter (\x -> x /= Nothing) maybeParsedEvents
          let parsedEvents = map (\(Just x) -> x) justParsedEvents
          pure parsedEvents

processSourceOneDir :: Path -> IO (Path, [Event])
processSourceOneDir srcDir = do
  events <- readConfigFile srcDir
  pure (srcDir, events)

processSourceDirs :: [Path] -> IO [(Path, [Event])]
processSourceDirs sourceDirs = mapM processSourceOneDir sourceDirs

isPathPhoto :: Path -> Bool
isPathPhoto path = let ext = map toLower (takeExtension path) in
                       ext == ".jpg" || ext == ".jpeg"

-- Try to read a dir, return [] if unsuccessful
readDir :: Path -> IO [Path]
readDir path = do
  result <- try (listDirectory path) :: IO (Either SomeException [Path])
  case result of
    Left exception -> pure []
    Right value -> pure value

-- Recursively get all the photos from a dir and its subdirs
makePhotoList :: Path -> IO [Path]
makePhotoList dir = do
  -- Get "naked" path, which is relative to the current directory
  dirContentsRelativePath <- readDir dir
  let dirContents = if ".nomedia" `elem` dirContentsRelativePath
                       then []
                       -- Get "full" path, which is relative to where we ran the
                       -- program
                       else map (\x -> dir ++ "/" ++ x) dirContentsRelativePath
  -- Get files in the current dir:
  files <- filterM doesFileExist dirContents
  -- Filter photos out of files:
  let photos = filter isPathPhoto files
  -- Get dirs in the current dir:
  dirs <- filterM doesDirectoryExist dirContents
  -- Recursive call to get the photos from the subdirs too
  nestedPhotosNonFlat <- mapM makePhotoList dirs
  -- Flatten the structure containing the nested photos:
  let nestedPhotos = concat nestedPhotosNonFlat
  pure $ photos ++ nestedPhotos

attachTimestamp :: Path -> IO (Path, UTCTime)
attachTimestamp path = do
  timestamp <- getModificationTime path
  pure (path, timestamp)

doesBelongToEvent :: Event -> (Path, UTCTime) -> Bool
doesBelongToEvent (eventStart, eventEnd, _) (_, fileTime) =
  fileTime >= eventStart && fileTime < eventEnd

-- Take timestamped files and an event read from photosort.conf, and remove those not
-- fitting event's time window:
filterBelongingToEvent :: [(Path, UTCTime)] -> Event -> [(Path, UTCTime)]
filterBelongingToEvent timestampedFiles event = filter (doesBelongToEvent event) timestampedFiles

-- Bind together the event and its matching files
eventAndItsFiles :: [(Path, UTCTime)] -> Event -> (Event, [(Path, UTCTime)])
eventAndItsFiles timestampedFiles event = (event, filterBelongingToEvent timestampedFiles event)

-- No longer need event's beginning timestamp and end timestamp, so remove them
-- and keep only event's path:
stripEventStartAndEnd :: (Event, [(Path, UTCTime)]) -> (Path, [(Path, UTCTime)])
stripEventStartAndEnd (event, timestampedFiles) = ((\(_,_,path) -> path) event, timestampedFiles)

-- Create the event corresponding to "other/" and assign to it photos that don't
-- fit into other events' time ranges
addFilesOutsideAnyEvent :: [(Path, UTCTime)] -> [(Path, [(Path, UTCTime)])] -> (Path, [(Path, UTCTime)])
addFilesOutsideAnyEvent timestampedFiles pathsWithTimestampedFiles =
  let allSortedFiles = concat (map snd pathsWithTimestampedFiles);
      isNotAmongSorted timestampedFile = not $ timestampedFile `elem` allSortedFiles
   in ("other/", (filter isNotAmongSorted timestampedFiles))

-- Take timestamped files, take events, and create a "catalogue" whose unit is
-- an event and this event's corresponding photos.
dispatchFilesIntoDestinations :: [(Path, UTCTime)] -> [Event] -> [(Path, [(Path, UTCTime)])]
dispatchFilesIntoDestinations timestampedFiles events =
  let sortedFiles = map stripEventStartAndEnd (map (eventAndItsFiles timestampedFiles) events);
      otherFiles = addFilesOutsideAnyEvent timestampedFiles sortedFiles in
      sortedFiles ++ [otherFiles]

-- Take a SRC_DIR path, and process it to get its events+photos
getDispatchedFilesForSourceDir :: Path -> IO [(Path, [(Path, UTCTime)])]
getDispatchedFilesForSourceDir sourceDir = do
          files <- makePhotoList sourceDir
          timestampedFiles <- mapM attachTimestamp files
          events <- readConfigFile sourceDir
          let dispatched = dispatchFilesIntoDestinations timestampedFiles events
          pure dispatched

-- Take events+photos for each SRC_DIR and pool them together into events+photos
-- for all SRC_DIRs
uniteFilesFromDifferentSourceDirs :: [[(Path, [(Path, UTCTime)])]] -> [(Path, [(Path, UTCTime)])]
uniteFilesFromDifferentSourceDirs dispatchedFilesFromSeveralSourceDirs =
  let flatList = concat dispatchedFilesFromSeveralSourceDirs;
      destDirList = nub $ map fst flatList;
      allFilesForDestDirNested dir = filter (\item -> (fst item) == dir) flatList;
      allFilesForDestDir dir = concat $ map snd (allFilesForDestDirNested dir);
      wrapDirAndFiles dir = (dir, allFilesForDestDir dir) in
      filter (not . null . snd) (map wrapDirAndFiles destDirList)

-- Sorts the photos by their timestamp ascendingly
sortFilesByTimestamp :: (Path, [(Path, UTCTime)]) -> (Path, [(Path, UTCTime)])
sortFilesByTimestamp (path, timestampedFiles) = let sorter = sortBy (compare `on` snd) in
  (path, sorter timestampedFiles)

-- Use the event path to construct new path for every photo. Event path is no
-- longer needed, so discard it.
getRenamingArguments :: (Path, [(Path, UTCTime)]) -> [(Path, Path)]
getRenamingArguments (path, timestampedFiles) =
  let maxNumberLength = length $ show (length timestampedFiles);
      withIndexAttached = zip [0..] timestampedFiles;
      paddingZeroes item = take (maxNumberLength - length (show (fst item))) (repeat '0');
      newName item = path ++ "/" ++ (paddingZeroes item) ++ show (fst item) ++ "_" ++ (takeFileName (fst (snd item))) in
      zip (map fst timestampedFiles) (map newName withIndexAttached)

tryMoveFile :: (Path, Path) -> IO ()
moveFile (oldName, newName) = do
  result <- try (renameFile oldName newName) :: IO (Either SomeException ())
  case result of
    Left exception -> putStrLn $ "While trying to move file: " ++ show exception
    Right value -> pure ()

-- Move file, but only if destination does not exist
moveFile :: (Path, Path) -> IO ()
tryMoveFile (oldName, newName) = do
  clash <- doesPathExist newName
  if clash then putStrLn $ "File exists: " ++ newName
           else tryMoveFile (oldName, newName)

createDir :: Path -> IO ()
createDir path = do
  result <- try (createDirectory path) :: IO (Either SomeException ())
  case result of
    Left exception -> putStrLn $ "While trying to create a directory: " ++ show exception
    Right value -> pure ()

-- Check if directory is empty. In case we can't be sure that it's empty, behave
-- as if it's not empty, because we don't want to delete non-empty directory.
isDirEmpty :: Path -> IO Bool
isDirEmpty path = do
  result <- try (getDirectoryContents path) :: IO (Either SomeException [FilePath])
  case result of
    Left exception -> pure False
    Right value -> pure (value == [".", ".."])

-- Remove a directory if it is definitely empty
removeDir :: Path -> IO ()
removeDir path = do
  result <- try (removeDirectory path) :: IO (Either SomeException ())
  case result of
    Left exception -> putStrLn $ "While trying to remove a directory: " ++ show exception
    Right value -> pure ()

-- Recursively remove a directory and all its subdirectories if they are
-- definitely empty
cleanDir :: Path -> IO ()
cleanDir dir = do
  dirContentsRelativePath <- readDir dir
  let dirContents = map (\x -> dir ++ "/" ++ x) dirContentsRelativePath
  dirs <- filterM doesDirectoryExist dirContents
  mapM cleanDir dirs
  empty <- isDirEmpty dir
  if empty then removeDir dir
           else pure ()

main = do
  -- Get the passed SRC_DIRs, TARGET_DIR and options:
  (sourceDirs, targetDir, options) <- getOptionsAndArguments

  if Help `elem` options || null options
     -- If the --help option was passed or nothing was passed, show help and do
     -- nothing else:
     then do putStrLn $ usageInfo helpHeader availableOptions
     -- Otherwise, start the actions:
     else do
       -- Find the files that need to be moved and process them into
       -- namePairs, which list old path and new path for every photo:
       dispatchedFilesFromAllSourceDirs <- mapM getDispatchedFilesForSourceDir sourceDirs
       let flattenedFilesFromAllSourceDirs = uniteFilesFromDifferentSourceDirs dispatchedFilesFromAllSourceDirs
       let destinationsWithSortedFiles = map sortFilesByTimestamp flattenedFilesFromAllSourceDirs
       let incompleteNamePairs = concat $ map getRenamingArguments destinationsWithSortedFiles
       let namePairs = map (\ (oldName, newName) -> (oldName, targetDir ++ "/" ++ newName)) incompleteNamePairs

       -- Get the directories that will hold the files in the DEST_DIR:
       let dirsToCreate = map (\dir -> targetDir ++ "/" ++ dir) (map fst destinationsWithSortedFiles)

       if DryRun `elem` options
          -- If --dry-run was passed, only tell what would happen
          then do putStrLn $ "Directories to be created:"
                  putStrLn targetDir
                  mapM putStrLn dirsToCreate
                  putStrLn $ "Files to be moved:"
                  mapM_ (\x -> putStrLn $ (show (fst x)) ++ " -> " ++ (show (snd x))) namePairs
          -- If --dry-run was not passed, create DEST_DIR, create the directory
          -- structure, and move the files:
          else do createDir targetDir
                  mapM createDir dirsToCreate
                  mapM_ moveFile namePairs
       if CleanUp `elem` options
          -- If --cleanup was passed, remove all empty directories in every SRC_DIR:
          then do mapM cleanDir sourceDirs
          else do pure [()]

       pure ()
