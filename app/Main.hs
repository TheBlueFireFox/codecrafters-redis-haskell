{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Args qualified
import ConcurrentMemory qualified as CM
import Data.ByteString.Lazy.Char8 qualified as BLC
import Process qualified
import Server (serve)

main :: IO ()
main = do
    (opts, _resArgs) <- Args.parseArgs
    print opts

    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    BLC.putStrLn "Logs from your program will appear here"

    -- in memory database
    db <- CM.newDB

    -- Uncomment this block to pass stage 1
    let port = "6379"
    putStrLn $ "Redis server listening on port " ++ port

    serve port (Process.process (db, opts))
