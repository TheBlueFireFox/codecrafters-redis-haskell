{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Args qualified
import DB.ConcurrentMemory qualified as CM
import DB.DB qualified as DB
import Data.ByteString.Lazy.Char8 qualified as BLC
import Parse.RDB qualified as RDB
import Parse.RESP qualified as RESP
import Process qualified
import Server (serve)
import System.Directory qualified as Dir
import System.FilePath ((</>))

run :: Args.Options -> Process.DB -> IO b
run opts db = do
    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    BLC.putStrLn "Logs from your program will appear here"

    -- Uncomment this block to pass stage 1
    let port = "6379"
    putStrLn $ "Redis server listening on port " ++ port

    serve port (Process.process (db, opts))

runNoDB :: Args.Options -> IO b
runNoDB opts = run opts =<< CM.newDB

convertDB :: DB.DB BLC.ByteString BLC.ByteString -> DB.DB RESP.RESPDataTypes RESP.RESPDataTypes
convertDB = DB.mapDB t t
  where
    t = RESP.BulkString

rdbHandler :: Args.Options -> FilePath -> IO ()
rdbHandler opts path = do
    rdb <- RDB.readFile path
    -- putStrLn $ either show show rdb
    case rdb of
        Left v -> do
            -- if we are here we know that we had an issue with parsing :(
            BLC.putStrLn v
            runNoDB opts
        Right v -> do
            BLC.putStrLn $ "Loaded Redis DB Version: " <> BLC.pack (show (RDB.rdbVersionNr v))
            let dbRDB = head $ RDB.dbs v -- for now only load db 0
            BLC.putStrLn $ "Loaded DB : " <> BLC.pack (show (RDB.databaseNr dbRDB))
            BLC.putStrLn $ "Hash table size: " <> BLC.pack (show (RDB.hashTableSize dbRDB))
            BLC.putStrLn $ "Expire table size: " <> BLC.pack (show (RDB.hashExpireTableSize dbRDB))
            
            print $ RDB.auxField v

            let store = RDB.store dbRDB
            db <- CM.fromDB $ convertDB store
            run opts db

main :: IO ()
main = do
    (opts, _resArgs) <- Args.parseArgs

    case (Args.optDir opts, Args.optDbPath opts) of
        -- rdb based in memory database
        (Just dir, Just path) -> do
            let filePath = dir </> path
            b <- Dir.doesFileExist filePath
            if not b
                then runNoDB opts
                else rdbHandler opts filePath
        -- new in memory database
        _ -> runNoDB opts
