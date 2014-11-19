-- TODO: exit code (impossible using pipes?)

-- brew install cabal-install
-- cabal update
-- cabal install text-icu
--   Mac: brew install icu4c; cabal install text-icu --extra-include-dirs=/usr/local/opt/icu4c/include --extra-lib-dirs=/usr/local/opt/icu4c/lib
-- cabal install pipes

{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}

----------------------------------------------------------------------

import Debug.Trace

import qualified System.Console.CmdArgs as CmdArgs -- command line switches
import System.Console.CmdArgs ((&=))
import System.Environment (getArgs, getProgName)

import Pipes

import qualified System.Directory as Dir -- doesDirectoryExist
import qualified Distribution.Simple.Utils as Dir -- getDirectoryContentsRecursive
import qualified System.FilePath.Posix as Dir     -- combine

import qualified Text.Regex.Posix as Re
import Data.Bits ((.|.))
import qualified Data.Text.ICU.Regex as ICU

import Control.Monad (when, unless, forever)
import Control.Monad.Reader
import Data.Functor.Identity (Identity)

import Control.Exception (try)
import qualified Data.Text as T -- Text
import qualified Data.Text.IO as T -- readFile
import System.IO (hPutStrLn, stderr, stdin)
import qualified System.IO.Error as IOE
import qualified GHC.IO.Exception as G

----------------------------------------------------------------------

main = do
  options <- CmdArgs.cmdArgs commandLineOptions
  let runPipe pipe = runReaderT (runEffect pipe) options
  runPipe $
        dirsToSearchIn
    >-> expandFilenames
    >-> excludeFilenames
    >-> readFileForGrep
    >-> splitIntoLines
    >-> filterByRegex
    >-> markMatches
    >-> output

----------------------------------------------------------------------

type PipeBase = ReaderT CommandLineOptions IO

----------------------------------------------------------------------

{- yields names found in the command line -}
dirsToSearchIn :: Producer String PipeBase Int
dirsToSearchIn = do
  options <- ask
  for (each (searchIn options)) yield
  return 11

----------------------------------------------------------------------

output :: Consumer (String, Int, Int, T.Text) PipeBase Int
output = do
  forever $ do
    (filename, lineNo, colNo, line) <- await
    liftIO $ putStrLn $ filename ++ ":" ++ show lineNo ++ ":" ++ show colNo ++ ": " ++ T.unpack line
    return 1

-- -- outputT :: Consumer String PipeBase Int
-- outputT = do
--   -- liftIO $ putStrLn "outputT"
--   forever $ do
--     entry <- await
--     liftIO $ putStrLn $ show entry

----------------------------------------------------------------------

{- Yields (filename, lineNo, firstMatchColNo, line) -}
markMatches :: Pipe (String, Int, [(Int, Int)], T.Text) (String, Int, Int, T.Text) PipeBase Int
markMatches = do
  options <- ask
  let startColor = T.pack "\x1B[35;47m"
      resetColor = T.pack "\x1B[0m"
      -- mark start end line | trace (show start ++ ":" ++ show end ++ "  " ++ show line) False = undefined
      mark start end line =
        (T.take start line) `T.append` startColor `T.append` (T.take (end - start) (T.drop start line)) `T.append` resetColor `T.append` (T.drop end line)
      markLine matches line =
        foldl (\l (start, end) -> mark start end l) line matches
      noMarkLine _ line = line
      doMark = if noMark options then noMarkLine else markLine
  forever $ do
    (filename, lineNo, matches, line) <- await
    yield (filename, lineNo, fst (head matches) + 1, doMark matches line)

----------------------------------------------------------------------

{- Yields (filename, lineNo, [(begin, end)] line) -}
filterByRegex :: Pipe (String, Int, T.Text) (String, Int, [(Int, Int)], T.Text) PipeBase Int
filterByRegex = do
  options <- ask
  let regexOptions = if (caseSensetive options) then [ICU.ErrorOnUnknownEscapes] else [ICU.CaseInsensitive, ICU.ErrorOnUnknownEscapes]
      searchFor = lookFor options
  errorOrRegex <- liftIO $ ICU.regex' regexOptions (T.pack searchFor) -- compile regex into Either
  case errorOrRegex of
   -- Left err -> liftIO $ hPutStrLn stderr $ "Error: invalid regular expression '" ++ searchFor ++ "': " ++ (show err)
   Left err -> (liftIO $ hPutStrLn stderr $ "Error: invalid regular expression '" ++ searchFor ++ "': " ++ (show err)) >> return 0
   Right regex ->
     forever $ do
       (filename, lineNo, line) <- await
       liftIO $ ICU.setText regex line
       found <- liftIO $ grepInLine regex line
       unless (null found) $ yield (filename, lineNo, found, line)

{- Returns a list of (start, end) of each regex match in line -}
grepInLine :: ICU.Regex -> T.Text -> IO [(Int, Int)]
grepInLine regex line =
  let finderInit = ICU.find regex 0
      finderNext = ICU.findNext regex
      find finder result = do   -- kind of loop: for (found = finderInit; found; found = finderNext) result += (start, end)
        found <- finder
        case found of
         True -> do
           Just start <- ICU.start regex 0
           Just end <- ICU.end regex 0
           find finderNext ((fromIntegral start, fromIntegral end) : result)
         False ->
           return result
  in do
    ICU.setText regex line
    find finderInit [] >>= return

-- ----------------------------------------------------------------------

splitIntoLines :: Pipe (String, T.Text) (String, Int, T.Text) PipeBase Int
splitIntoLines =
  let enumerateLines fileData = zip [0..] (T.lines fileData)
      processLine filename (lineNo, line) = (filename, lineNo + 1, line)
  in
   forever $ do
     (filename, fileData) <- await
     for (each (enumerateLines fileData)) (yield . (processLine filename))

-- ----------------------------------------------------------------------

readFileForGrep :: Pipe String (String, T.Text) PipeBase Int
readFileForGrep = do
  options <- ask
  let reportBinaryFile = if (binaryFileReport options) then doReport else \_ -> return ()
      doReport filename = hPutStrLn stderr $ filename ++ ": binary file"
  forever $ do
    filename <- await
    let readStdin = filename == "-"
        readFile = if readStdin then T.hGetContents stdin else T.readFile filename
        filenameToShow = if readStdin then "stdin" else filename
    fileContent <- liftIO $ try $ readFile
    case fileContent of
     Left err@(G.IOError {G.ioe_description = err_desc}) -- http://hackage.haskell.org/package/base-4.7.0.1/docs/src/System-IO-Error.html
       | err_desc == "invalid byte sequence" -> liftIO $ reportBinaryFile filename
       | err_desc == "No such file or directory" -> liftIO $ hPutStrLn stderr $ "Warning: Couldn't open " ++ filename ++ ": " ++ err_desc
       | otherwise        -> liftIO $ hPutStrLn stderr $ "Warning: Couldn't open " ++ (show err) ++ " " ++ (show (IOE.ioeGetErrorString err)) ++ " " ++ show err_desc
     Right fileData ->
       yield (filenameToShow, fileData)

-- ----------------------------------------------------------------------

{- Filenames to exclude -}
excludeRegexArchives = ["\\.(bz2|gz|tgz|tbz|rar|zip)$"]
excludeRegexBinaries = ["\\.(pyc|pyo|so|o|a|pyd|dll|lib|dylib|elc|min\\.js)$", "__pycache__/"]
excludeRegexBak = ["\\.(~|#|bak)$"]
excludeRegexImages = ["\\.(png|jpg|gif|bmp|tiff?|pdf|ps|eps|ico|icns)$"]
excludeRegexHtml = ["\\.(htm|html)$"]
excludeRegexDoc = ["\\.(xlsx?|docx?|pptx?)$"]
excludeRegexAC = ["\\.(acmacs|save)$", "obsolete", "build/", "TAGS"]
excludeRegexHidden = ["^\\.", "/\\."]

excludeRegex = excludeRegexArchives ++ excludeRegexBinaries ++ excludeRegexBak ++ excludeRegexImages ++ excludeRegexHtml ++ excludeRegexDoc ++ excludeRegexAC ++ excludeRegexHidden

prepareRegex :: [String] -> [Re.Regex]
-- prepareRegex r | trace "prepareRegex" False = undefined
prepareRegex regex =
  map prepareRegexStr regex
  where
    prepareRegexStr regex = Re.makeRegexOpts (Re.compExtended .|. Re.compIgnoreCase) Re.defaultExecOpt regex

-- ----------------------------------------------------------------------

{- excludes from pipe filenames matching exclusion list(s) -}
excludeFilenames :: Pipe String String PipeBase Int
excludeFilenames = do
  options <- ask
  let rex = prepareRegex excludeRegex
      inExclusionList name = any (\r -> Re.matchTest r name) rex
      notExcluded = case allFiles options of
        True -> \_ -> True
        False -> not . inExclusionList
  forever $ do
    name <- await
    when (notExcluded name) $ yield name

-- ----------------------------------------------------------------------

{- expands directories found in input recursively -}
expandFilenames :: Pipe String String PipeBase Int
expandFilenames = do
  name <- await
  isdir <- liftIO $ Dir.doesDirectoryExist name
  case isdir of
   True -> do
     files <- liftIO $ Dir.getDirectoryContentsRecursive name
     for (each (map (Dir.combine name) files)) yield
   False ->
     yield name
  expandFilenames

----------------------------------------------------------------------

-- https://github.com/ndmitchell/cmdargs

data CommandLineOptions = CommandLineOptions { verbose :: Bool
                                             , caseSensetive :: Bool
                                             , lookFor :: String
                                             , searchIn :: [String]
                                             , allFiles :: Bool
                                             , binaryFileReport :: Bool
                                             , noMark :: Bool
                                             } deriving (Show, CmdArgs.Data, CmdArgs.Typeable)

commandLineOptions :: CommandLineOptions
commandLineOptions = CommandLineOptions
    { verbose = False
    , caseSensetive = False &= CmdArgs.name "case" &= CmdArgs.help "case sensitive search"
    , lookFor = CmdArgs.def &= CmdArgs.argPos 0 &= CmdArgs.typ "LOOK-FOR"
    , searchIn = CmdArgs.def &= CmdArgs.args &= CmdArgs.opt "-" &= CmdArgs.typ "DIRS/FILES"
    , allFiles = CmdArgs.def &= CmdArgs.name "all" &= CmdArgs.help "do not exclude any files"
    , binaryFileReport = CmdArgs.def &= CmdArgs.name "binary" &= CmdArgs.help "report if binary file was tried to search in"
    , noMark = CmdArgs.def &= CmdArgs.help "do not mark matches in the output"
    }
    &= CmdArgs.program prog
    &= CmdArgs.summary (prog ++ ": simple recursive grep replacement")
  where
    prog = "gr"

----------------------------------------------------------------------
