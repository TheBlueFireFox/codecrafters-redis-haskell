{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Args qualified
import ConcurrentMemory qualified as CM
import Data.ByteString.Lazy.Char8 qualified as BLC
import Parse.RDB qualified as RDB
import Process qualified
import Server (serve)
import System.FilePath ((</>))

run :: Args.Options -> IO b
run opts = do
    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    BLC.putStrLn "Logs from your program will appear here"

    -- in memory database
    db <- CM.newDB

    -- Uncomment this block to pass stage 1
    let port = "6379"
    putStrLn $ "Redis server listening on port " ++ port

    serve port (Process.process (db, opts))

main :: IO ()
main = do
    (opts, _resArgs) <- Args.parseArgs

    case (Args.optDir opts, Args.optDbPath opts) of
        (Just dir, Just path) -> do
           rdb <- RDB.readFile $ dir </> path
           putStrLn $ either show (const "success") rdb
           --run opts
        _ -> error ""
