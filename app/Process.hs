{-# LANGUAGE OverloadedStrings #-}

module Process (process) where

import Args qualified as Q
import ConcurrentMemory qualified as CM
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.Text.Lazy qualified as TL
import Data.Word (Word64)
import ParseRESP qualified as ResPar
import Request qualified as Req
import Text.Parsec qualified as TL

type Data = (DB, Q.Options)

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

handleSet :: DB -> ResPar.RESPDataTypes -> ResPar.RESPDataTypes -> Maybe Word64 -> IO Response
handleSet db key value expiry = do
    case expiry of
        Nothing -> CM.insert key value db
        Just un -> CM.insertWith key value un db
    pure returnOk

handleGet :: DB -> ResPar.RESPDataTypes -> IO Response
handleGet db key = do
    mVal <- CM.lookup key db
    pure $ fromMaybe ResPar.NullString mVal

extractor :: ResPar.RESPDataTypes -> Either Response TL.Text
extractor p = case p of
    ResPar.BulkString v -> Right v
    ResPar.SimpleString v -> Right v
    _ -> Left $ handleError "Incorrect Type"

handleConfigGet :: Q.Options -> [ResPar.RESPDataTypes] -> Response
handleConfigGet opts payload = either id (ResPar.Arrays . concat) $ inner [] payload
  where
    inner acc [] = Right acc
    inner acc (x : xs) = flip inner xs . (: acc) =<< p =<< extractor x

    str = ResPar.BulkString
    pro x = Right . maybe [ResPar.NullString] (\y -> [str x, str (TL.pack y)])
    p x = case TL.toUpper x of
        "DIR" -> pro x $ Q.optDir opts
        "DBFILENAME" -> pro x $ Q.optDbPath opts
        _ -> Left $ handleError "Unknown config key"

handleCommands :: Data -> Request -> IO Response
handleCommands (db, opts) req = case req of
    Req.Ping payload -> pure $ handlePing payload
    Req.Echo payload -> pure $ handleEcho payload
    Req.Set key value expiry -> handleSet db key value expiry
    Req.Get key -> handleGet db key
    Req.ConfigGet params -> pure $ handleConfigGet opts params

processHelper :: Data -> BL.ByteString -> IO Response
processHelper d = maybe (pure $ handleError "Unable to parse request") parse . ResPar.deserialize
  where
    parse parsable = either pure (handleCommands d) $ Req.parse (ResPar.fromRESPDataTypes parsable, parsable)

process :: Data -> BL.ByteString -> IO BL.ByteString
process d input = ResPar.serialize <$> processHelper d input
