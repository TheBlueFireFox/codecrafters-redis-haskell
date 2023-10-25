{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO)

import Data.ByteString qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as TE
import Network.Simple.TCP qualified as Tcp

import Process qualified

recvAll :: (MonadIO f) => Tcp.Socket -> f (Maybe BL.ByteString)
recvAll sock = inner mempty
  where
    block = 1024
    inner acc = con acc =<< Tcp.recv sock block

    con acc Nothing = assert (BL.length acc == 0) (pure Nothing)
    con acc (Just d)
        | BB.length d < block = pure $ pure $ acc <> BL.fromStrict d
        | otherwise = inner (acc <> BL.fromStrict d)

process :: BLC.ByteString -> BLC.ByteString
process = Process.process

run :: (Show t) => Tcp.Socket -> t -> IO ()
run socket address = do
    reqRaw <- recvAll socket
    BLC.putStrLn $ "gotten request" <> BLC.pack (show address) <> "."
    case reqRaw of
        Nothing -> do
            pure ()
        Just req -> do
            BLC.putStrLn "Request"
            print $ T.replace (T.pack "\r\n") (T.pack "\\r\\n") $ TE.decodeASCII req
            BLC.putStrLn "Response"
            let res = process req
            print res
            Tcp.sendLazy socket res
            -- loop to get the next command
            run socket address

main :: IO ()
main = do
    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    BLC.putStrLn "Logs from your program will appear here"

    -- Uncomment this block to pass stage 1
    let port = "6379"
    putStrLn $ "Redis server listening on port " ++ port
    Tcp.serve Tcp.HostAny port $ \(socket, address) -> do
        BLC.putStrLn $ "successfully connected client: " <> BLC.pack (show address) <> "."
        run socket address
        BLC.putStrLn $ "Connection Closed " <> BLC.pack (show address) <> "."
        Tcp.closeSock socket
