{-# LANGUAGE OverloadedStrings #-}

module Server (serve) where

import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO)

import Data.ByteString qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as TE
import Network.Simple.TCP qualified as Tcp

recvAll :: (MonadIO f) => Tcp.Socket -> f (Maybe BLC.ByteString)
recvAll sock = inner mempty
  where
    block = 1024
    inner acc = con acc =<< Tcp.recv sock block

    con acc Nothing = assert (BL.length acc == 0) (pure Nothing)
    con acc (Just d)
        | BB.length d < block = pure $ pure $ acc <> BL.fromStrict d
        | otherwise = inner (acc <> BL.fromStrict d)

serve :: MonadIO m => Tcp.ServiceName -> (BLC.ByteString -> IO BLC.ByteString) -> m a
serve port process = do
    Tcp.serve Tcp.HostAny port $ \(socket, address) -> do
        BLC.putStrLn $ "successfully connected client: " <> BLC.pack (show address) <> "."
        run process socket address
        BLC.putStrLn $ "Connection Closed " <> BLC.pack (show address) <> "."
        Tcp.closeSock socket

run :: Show t => (BLC.ByteString -> IO BLC.ByteString) -> Tcp.Socket -> t -> IO ()
run process socket address = do
    reqRaw <- recvAll socket
    BLC.putStrLn $ "gotten request" <> BLC.pack (show address) <> "."
    case reqRaw of
        Nothing -> do
            pure ()
        Just req -> do
            BLC.putStrLn "Request"
            print $ T.replace (T.pack "\r\n") (T.pack "\\r\\n") $ TE.decodeASCII req
            BLC.putStrLn "Response"
            res <- process req
            print res
            Tcp.sendLazy socket res
            -- loop to get the next command
            run process socket address
