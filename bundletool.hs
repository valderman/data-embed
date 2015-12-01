-- | Main module for @bundletool@, a command line interface to @data-bundle@.
module Main where
import Control.Monad
import Data.Bundle.File
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data Overwrite = Append | Replace | DontOverwrite

data Option
  = Write                  -- Write a bundle to an executable
  | Erase                  -- Erase an existing bundle
  | Check                  -- Check if a file contains a bundle or not
  | List                   -- List all files in a bundle
  | PrintHelp              -- Print help message and exit
  | PrintUsage             -- Print usage message and exit
  | SetOverwrite Overwrite -- Set overwrite mode
  | SetStrip Int           -- Set the number of leading directories to strip
                           -- from file names added to bundle

opts :: [OptDescr Option]
opts =
  [ Option "w" ["write"]     (NoArg Write) $
    "Write a bundle to a file. Do nothing if the file already has a bundle."
  , Option "r" ["replace"]   (NoArg (SetOverwrite Replace)) $
    "When writing a bundle to a file, overwrite any previously existing " ++
    "bundle."
  , Option "a" ["append"]    (NoArg (SetOverwrite Append)) $
    "When writing a bundle to a file, leave the old one in place but " ++
    "append the new one at the end of the file."
  , Option "p" ["strip"]     (ReqArg (SetStrip . read) "NUM") $
    "Strip up to NUM leading directories from file names added to bundle.\n" ++
    "`bundletool -p1 -w outfile dir/infile' will add `dir/infile' to the " ++
    "bundle with the name `infile'."
  , Option "ed" ["erase"]    (NoArg Erase) $
    "Erase previously created bundles from any of the given files."
  , Option "l" ["list"]      (NoArg List) $
    "List all files in the given bundle."
  , Option "c" ["check"]     (NoArg Check) $
    "Check whether the given file contains a bundle or not."
  , Option "?h" ["help"]     (NoArg PrintHelp) $
    "Print this message and exit."
  ]

helpHeader :: String
helpHeader = concat
  [ "bundletool creates, modifies and inspects file bundles for use with the "
  , "`data-bundle' library. It accepts the following options:"
  ]

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute opts args of
    (opts, nonopts, []) -> runAct (getAction opts) (writeOpts opts) nonopts
    (_, _, errs)        -> mapM_ (hPutStr stderr) errs >> exitFailure

-- | Extract write options from the given list of options.
writeOpts :: [Option] -> (Overwrite, Int)
writeOpts = foldl writeOpt (DontOverwrite, 0)
  where
    writeOpt (_, s)   (SetOverwrite ovr) = (ovr, s)
    writeOpt (ovr, _) (SetStrip s)       = (ovr, s)
    writeOpt acc      _                     = acc

-- | Get the action to perform from the given list of options.
getAction :: [Option] -> Option
getAction = foldl getAct PrintUsage
  where
    getAct acc (SetOverwrite _) = acc
    getAct acc (SetStrip _)     = acc
    getAct _   act              = act

runAct :: Option -> (Overwrite, Int) -> [String] -> IO ()
runAct Write (ovr, s) fs = do
  case fs of
    (outf : infs) | not (null infs) -> do
      alreadyHasBundle <- hasBundle outf
      when alreadyHasBundle $ do
        case ovr of
          DontOverwrite -> do
            hPutStrLn stderr $ "file `" ++ outf ++ "' already has a " ++
                               "bundle; aborting"
            exitFailure
          Replace ->
            eraseBundle outf
          _ ->
            return ()
      appendBundle outf (map (FilePath s) infs)
    _ -> do
      hPutStrLn stderr $ "need an output file and at least one input file " ++
                         "to create a bundle"
      hPutStrLn stderr $ "try `bundletool -w outfile infile1 [infile2 ...]'"
      exitFailure
runAct Erase _ outfs = do
  when (null outfs) $ do
    hPutStrLn stderr $ "need at least one file to erase bundle from"
    hPutStrLn stderr $ "try `bundletool -e file1 [file2 ...]'"
    exitFailure
  mapM_ eraseBundle outfs
runAct Check _ infs = do
  case infs of
    [inf] -> do
      ok <- hasBundle inf
      putStrLn $ if ok then "yes" else "no"
    _ -> do
      hPutStrLn stderr $ "need exactly one file to check for bundles"
      hPutStrLn stderr $ "try `bundletool -c file'"
      exitFailure
runAct List _ infs = do
  case infs of
    [inf] -> do
      res <- withBundle inf (mapM_ putStrLn . listBundleFiles)
      case res of
        Right _ -> return ()
        Left e  -> do
          hPutStrLn stderr $ "failed to read bundle: " ++ e
          exitFailure
    _ -> do
      hPutStrLn stderr $ "need exactly one file to list files from"
      hPutStrLn stderr $ "try `bundletool -l file'"
      exitFailure
runAct PrintHelp _ _ = do
  putStr $ usageInfo helpHeader opts
runAct PrintUsage _ _ = do
  putStrLn "usage: bundletool OPTIONS FILE [FILES]"
  putStrLn "try `bundletool --help' for more information"
  exitFailure
