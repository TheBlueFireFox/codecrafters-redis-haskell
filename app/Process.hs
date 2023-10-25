{-# LANGUAGE OverloadedStrings #-}

module Process (process) where

import Data.ByteString.Lazy qualified as BL
import Data.Text.Lazy qualified as TL
import ParseRESP qualified as ResPair
import ParseRESP qualified as ResPar
import Request qualified as Req

type Request = Req.Request
type Response = ResPar.RESPDataTypes

handleError :: TL.Text -> Response
handleError = ResPar.SimpleError

handlePing :: Maybe ResPar.RESPDataTypes -> ResPar.RESPDataTypes
handlePing (Just (ResPair.BulkString a)) = ResPair.BulkString a
handlePing (Just (ResPair.SimpleString a)) = ResPair.SimpleString a
handlePing Nothing = ResPar.SimpleString "PONG"
handlePing _ = handleError "Unsupported Type for <PING>"

handleEcho :: ResPar.RESPDataTypes -> Response
handleEcho payload = case payload of
    ResPar.BulkString _ -> payload
    ResPar.SimpleString _ -> payload
    _ -> handleError "ERR: unknown data format"

handleCommands :: Request -> Response
handleCommands req = case req of
    Req.PING payload -> handlePing payload
    Req.ECHO payload -> handleEcho payload

processHelper :: BL.ByteString -> Response
processHelper = maybe (handleError "Unable to parse request") parse . ResPar.deserialize
  where
    parse parsable = either id handleCommands $ Req.parse (ResPar.fromRESPDataTypes parsable, parsable)

process :: BL.ByteString -> BL.ByteString
process = ResPar.serialize . processHelper
