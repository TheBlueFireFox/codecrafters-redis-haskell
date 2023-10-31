{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Args qualified
import ConcurrentMemory qualified as CM
import DB.DB qualified as DB
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Text.Lazy.Encoding qualified as TL
import Parse.RDB qualified as RDB
import Parse.RESP qualified as RESP
import Process qualified
import Server (serve)
import System.FilePath ((</>))

run :: Args.Options -> Process.DB -> IO b
run opts db = do
    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    BLC.putStrLn "Logs from your program will appear here"

    -- Uncomment this block to pass stage 1
    let port = "6379"
    putStrLn $ "Redis server listening on port " ++ port

    serve port (Process.process (db, opts))

convertDB :: DB.DB BLC.ByteString BLC.ByteString -> DB.DB RESP.RESPDataTypes RESP.RESPDataTypes
convertDB = DB.mapDB t t
  where
    t = RESP.BulkString . TL.decodeUtf8

rdbHandler :: Args.Options -> FilePath -> IO ()
rdbHandler opts path = do
    rdb <- RDB.readFile path
    -- putStrLn $ either show show rdb
    case rdb of
        Left v -> BLC.putStrLn v
        Right v -> do
            BLC.putStrLn $ "Loaded Redis DB Version: " <> BLC.pack (show (RDB.rdbVersionNr v))
            let dbRDB = head $ RDB.dbs v -- for now only load db 0
            BLC.putStrLn $ "Loaded DB : " <> BLC.pack (show (RDB.databaseNr dbRDB))
            BLC.putStrLn $ "Hash table size: " <> BLC.pack (show (RDB.hashTableSize dbRDB))
            BLC.putStrLn $ "Expire table size: " <> BLC.pack (show (RDB.hashExpireTableSize dbRDB))
            let store = RDB.store dbRDB
            db <- CM.fromDB $ convertDB store
            run opts db

main :: IO ()
main = do
    (opts, _resArgs) <- Args.parseArgs

    case (Args.optDir opts, Args.optDbPath opts) of
        -- rdb based in memory database
        (Just dir, Just path) -> do
            rdbHandler opts $ dir </> path
        -- new in memory database
        _ -> run opts =<< CM.newDB
