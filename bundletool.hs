-- | Main module for @bundletool@, a command line interface to @data-bundle@.
module Main where
import Control.Monad
import Data.Bundle.File
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data Overwrite = Append | Replace | DontOverwrite

data Action
  = Write (Maybe Overwrite) -- Write a bundle to an executable
  | Erase                   -- Erase an existing bundle
  | Check                   -- Check if a file contains a bundle or not
  | List                    -- List all files in a bundle
  | PrintHelp               -- Print help message and exit
  | PrintUsage              -- Print usage message and exit

overwriteMode :: Overwrite -> Action -> Action
overwriteMode mode (Write _) = Write (Just mode)
overwriteMode _    act       = act

opts :: [OptDescr (Action -> Action)]
opts =
  [ Option "w" ["write"]   (NoArg (const $ Write Nothing)) $
    "Write a bundle to a file. Do nothing if the file already has a bundle."
  , Option "r" ["replace"] (NoArg (overwriteMode Replace)) $
    "When writing a bundle to a file, overwrite any previously existing " ++
    "bundle."
  , Option "a" ["append"]  (NoArg (overwriteMode Append)) $
    "When writing a bundle to a file, leave the old one in place but " ++
    "append the new one at the end of the file."
  , Option "ed" ["erase"]  (NoArg (const Erase)) $
    "List all files in the given bundle."
  , Option "l" ["list"]    (NoArg (const List)) $
    "List all files in the given bundle."
  , Option "c" ["check"]   (NoArg (const Check)) $
    "Check whether the given file contains a bundle or not."
  , Option "?h" ["help"]   (NoArg (const PrintHelp)) $
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
    (acts, nonopts, []) -> runAct (foldr (flip (.)) id acts PrintUsage) nonopts
    (_, _, errs)        -> mapM_ (hPutStr stderr) errs >> exitFailure

runAct :: Action -> [String] -> IO ()
runAct (Write ovr) fs = do
  case fs of
    (outf : infs) | not (null infs) -> do
      alreadyHasBundle <- hasBundle outf
      when alreadyHasBundle $ do
        case maybe DontOverwrite id ovr of
          DontOverwrite -> do
            hPutStrLn stderr $ "file `" ++ outf ++ "' already has a " ++
                               "bundle; aborting"
            exitFailure
          Replace ->
            eraseBundle outf
          _ ->
            return ()
      appendBundle outf (map FilePath infs)
    _ -> do
      hPutStrLn stderr $ "need an output file and at least one input file " ++
                         "to create a bundle"
      hPutStrLn stderr $ "try `bundletool -w outfile infile1 [infile2 ...]'"
      exitFailure
runAct Erase outfs = do
  when (null outfs) $ do
    hPutStrLn stderr $ "need at least one file to erase bundle from"
    hPutStrLn stderr $ "try `bundletool -e file1 [file2 ...]'"
    exitFailure
  mapM_ eraseBundle outfs
runAct Check infs = do
  case infs of
    [inf] -> do
      ok <- hasBundle inf
      putStrLn $ if ok then "yes" else "no"
    _ -> do
      hPutStrLn stderr $ "need exactly one file to check for bundles"
      hPutStrLn stderr $ "try `bundletool -c file'"
      exitFailure
runAct List infs = do
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
runAct PrintHelp _ = do
  putStr $ usageInfo helpHeader opts
runAct PrintUsage _ = do
  putStrLn "usage: bundletool OPTIONS FILE [FILES]"
  putStrLn "try `bundletool --help' for more information"
  exitFailure
