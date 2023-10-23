{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString qualified as BB
import Data.ByteString.Char8 qualified as BBC
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Text.Lazy as T
import Data.Text.Lazy.Encoding qualified as TE
import Network.Simple.TCP qualified as Tcp
import Process qualified

recvAll :: (MonadIO f) => Tcp.Socket -> f BL.ByteString
recvAll sock = inner mempty
  where
    block = 1024
    inner acc = con acc =<< Tcp.recv sock block

    con acc Nothing = pure acc
    con acc (Just d)
        | BB.length d < block = pure (acc <> BL.fromStrict d)
        | otherwise = inner (acc <> BL.fromStrict d)

pp = Process.process . T.toLower

handler (Left err) = error ("An error occured " ++ T.unpack err)
handler (Right p) = TE.encodeUtf8 p

process = BB.toStrict . handler . Process.process . TE.decodeASCII

main :: IO ()
main = do
    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    BLC.putStrLn "Logs from your program will appear here"

    -- let t = "*1\r\n$4\r\nPING\r\n*"
    -- print t
    -- print $ pp t
    -- let t = "*1\r\n$4\r\nping\r\n*"
    -- print t
    -- print $ pp t
    -- let t = "*1\r\n$4\r\nPinG\r\n*"
    -- print t
    -- print $ pp t

    -- Uncomment this block to pass stage 1
    let port = "6379"
    putStrLn $ "Redis server listening on port " ++ port
    Tcp.serve Tcp.HostAny port $ \(socket, address) -> do
        BLC.putStrLn $ "successfully connected client: " <> BLC.pack (show address) <> "."
        req <- recvAll socket
        BLC.putStrLn "Request"
        BLC.putStrLn req
        BLC.putStrLn "\nResponse"
        let res = process req
        print res
        Tcp.send socket res
        Tcp.closeSock socket
