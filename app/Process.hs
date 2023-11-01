{-# LANGUAGE OverloadedStrings #-}

module Process (process, DB) where

import Args qualified as Q
import ConcurrentMemory qualified as CM
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.Text.Lazy qualified as TL
import Data.Word (Word64)
import Parse.RESP (RESPDataTypes)
import Parse.RESP qualified as RESP
import Request qualified as Req

type Data = (DB, Q.Options)

type Key = RESP.RESPDataTypes
type Value = RESP.RESPDataTypes
type DB = CM.DB Key Value

type Request = Req.Request
type Response = RESP.RESPDataTypes

handleError :: TL.Text -> Response
handleError = RESP.SimpleError

handlePing :: Maybe RESP.RESPDataTypes -> RESP.RESPDataTypes
handlePing (Just (RESP.BulkString a)) = RESP.BulkString a
handlePing (Just (RESP.SimpleString a)) = RESP.SimpleString a
handlePing Nothing = RESP.SimpleString "PONG"
handlePing _ = handleError "Unsupported Type for <PING>"

handleEcho :: RESP.RESPDataTypes -> Response
handleEcho payload = case payload of
    RESP.BulkString _ -> payload
    RESP.SimpleString _ -> payload
    _ -> handleError "ERR: unknown data format"

returnOk :: Response
returnOk = RESP.SimpleString "OK"

handleSet :: DB -> RESP.RESPDataTypes -> RESP.RESPDataTypes -> Maybe Word64 -> IO Response
handleSet db key value expiry = do
    case expiry of
        Nothing -> CM.insert key value db
        Just un -> CM.insertWith key value un db
    pure returnOk

handleGet :: DB -> RESP.RESPDataTypes -> IO Response
handleGet db key = do
    mVal <- CM.lookup key db
    pure $ fromMaybe RESP.NullString mVal

extractor :: RESP.RESPDataTypes -> Either Response TL.Text
extractor p = case p of
    RESP.BulkString v -> Right v
    RESP.SimpleString v -> Right v
    _ -> Left $ handleError "Incorrect Type"

handleConfigGet :: Q.Options -> [RESP.RESPDataTypes] -> Response
handleConfigGet opts payload = either id (RESP.Arrays . concat) $ inner [] payload
  where
    inner acc [] = Right acc
    inner acc (x : xs) = flip inner xs . (: acc) =<< p =<< extractor x

    str = RESP.BulkString
    pro x = Right . maybe [RESP.NullString] (\y -> [str x, str (TL.pack y)])
    p x = case TL.toUpper x of
        "DIR" -> pro x $ Q.optDir opts
        "DBFILENAME" -> pro x $ Q.optDbPath opts
        _ -> Left $ handleError "Unknown config key"

handleKeys :: DB -> RESPDataTypes -> IO Response
handleKeys db pattern = do
    keys <- CM.keys db
    helper keys pattern
  where
    helper keys (RESP.BulkString s) = matcher s keys
    helper keys (RESP.SimpleString s) = matcher s keys
    helper _ _ = pure RESP.NullArrays

    compStr (RESP.RString s) (RESP.RString p) = s == p
    compStr _ _ = False

    matcher s keys =
        if s == "*"
            then pure $ RESP.Arrays keys
            else pure $ RESP.Arrays $ filterMap keys
    filterMap = map snd . filter (compStr (RESP.fromRESPDataTypes pattern) . fst) . map (\x -> (RESP.fromRESPDataTypes x, x))

handleCommands :: Data -> Request -> IO Response
handleCommands (db, opts) req = case req of
    Req.Ping payload -> pure $ handlePing payload
    Req.Echo payload -> pure $ handleEcho payload
    Req.Set key value expiry -> handleSet db key value expiry
    Req.Get key -> handleGet db key
    Req.ConfigGet params -> pure $ handleConfigGet opts params
    Req.Keys pattern -> handleKeys db pattern

processHelper :: Data -> BL.ByteString -> IO Response
processHelper d = maybe (pure $ handleError "Unable to parse request") parse . RESP.deserialize
  where
    parse parsable = either pure (handleCommands d) $ Req.parse (RESP.fromRESPDataTypes parsable, parsable)

process :: Data -> BL.ByteString -> IO BL.ByteString
process d input = RESP.serialize <$> processHelper d input
