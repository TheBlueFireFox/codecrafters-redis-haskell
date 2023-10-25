{-# LANGUAGE OverloadedStrings #-}

module Process (process) where

import ConcurrentMemory qualified as CM
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.Text.Lazy qualified as TL
import ParseRESP qualified as ResPar
import Request qualified as Req

type Key = ResPar.RESPDataTypes
type Value = ResPar.RESPDataTypes
type DB = CM.DB Key Value

type Request = Req.Request
type Response = ResPar.RESPDataTypes

handleError :: TL.Text -> Response
handleError = ResPar.SimpleError

handlePing :: Maybe ResPar.RESPDataTypes -> ResPar.RESPDataTypes
handlePing (Just (ResPar.BulkString a)) = ResPar.BulkString a
handlePing (Just (ResPar.SimpleString a)) = ResPar.SimpleString a
handlePing Nothing = ResPar.SimpleString "PONG"
handlePing _ = handleError "Unsupported Type for <PING>"

handleEcho :: ResPar.RESPDataTypes -> Response
handleEcho payload = case payload of
    ResPar.BulkString _ -> payload
    ResPar.SimpleString _ -> payload
    _ -> handleError "ERR: unknown data format"

returnOk :: Response
returnOk = ResPar.SimpleString "OK"

handleSet :: DB -> ResPar.RESPDataTypes -> ResPar.RESPDataTypes -> IO Response
handleSet db key value = do
    CM.insert key value db
    pure returnOk

handleGet :: DB -> ResPar.RESPDataTypes -> IO Response
handleGet db key = do
    mVal <- CM.lookup key db
    pure $ fromMaybe ResPar.Nulls mVal

handleCommands :: DB -> Request -> IO Response
handleCommands db req = case req of
    Req.PING payload -> pure $ handlePing payload
    Req.ECHO payload -> pure $ handleEcho payload
    Req.SET key value -> handleSet db key value
    Req.GET key -> handleGet db key

processHelper :: DB -> BL.ByteString -> IO Response
processHelper db = maybe (pure $ handleError "Unable to parse request") parse . ResPar.deserialize
  where
    parse parsable = either pure (handleCommands db) $ Req.parse (ResPar.fromRESPDataTypes parsable, parsable)

process :: DB -> BL.ByteString -> IO BL.ByteString
process db input = ResPar.serialize <$> processHelper db input
