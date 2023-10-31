module Args (Options (..), parseArgs) where

import Control.Arrow (Arrow (first))
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Data.List (isPrefixOf)

-- --dir /tmp/redis-files --dbfilename dump.rdb
data Options = Options
    { optDir :: Maybe FilePath
    , optDbPath :: Maybe FilePath
    }
    deriving (Show)

toOuter :: InnerOptions -> Options
toOuter InnerOptions{innerOptDir, innerOptDbPath} =
    Options
        { optDir = innerOptDir
        , optDbPath = innerOptDbPath
        }

data InnerOptions = InnerOptions
    { innerOptShowHelp :: Bool
    , innerOptDir :: Maybe FilePath
    , innerOptDbPath :: Maybe FilePath
    }
    deriving (Show)

defaultOptions :: InnerOptions
defaultOptions =
    InnerOptions
        { innerOptShowHelp = False
        , innerOptDir = Nothing
        , innerOptDbPath = Nothing
        }

options :: [OptDescr (InnerOptions -> InnerOptions)]
options =
    [ Option
        ['h']
        ["help"]
        ( NoArg
            (\opts -> opts{innerOptShowHelp = True})
        )
        "Show this message"
    , Option
        ['d']
        ["dir"]
        ( OptArg
            (\f o -> o{innerOptDir = f})
            "FILE_PATH"
        )
        "The directory where RDB FILES are stored."
    , Option
        ['f']
        ["dbfilename"]
        ( OptArg
            (\f o -> o{innerOptDbPath = f})
            "FILE_NAME"
        )
        "The name of the RDB  FILE"
    ]

-- Function to split an argument by space and handle options with and without equal sign
-- THIS IS A UNSAFE HACK
combineArg :: [String] -> [String]
combineArg = inner []
  where
    inner acc [] = reverse acc
    inner acc [c] = inner (c : acc) []
    inner acc (c : a : r)
        | "-" `isPrefixOf` c && ('=' `elem` c) = inner (c : acc) (a : r)
        | "-" `isPrefixOf` c = inner ((c ++ "=" ++ a) : acc) r
        | otherwise = inner (a : acc) (a : r)

parseArgs :: IO (Options, [String])
parseArgs = do
    argv <- getArgs
    case getOpt Permute options (combineArg argv) of
        (o, n, []) -> first toOuter <$> handleIt (foldl (flip id) defaultOptions o, n)
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: "
    handleIt (opts, n)
        | innerOptShowHelp opts = do
            putStrLn $ usageInfo header options
            exitSuccess
        | otherwise = return (opts, n)
