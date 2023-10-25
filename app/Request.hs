{-# LANGUAGE OverloadedStrings #-}

module Request (Request (..), Command (..), parse) where

import Data.String (IsString)
import Data.Text.Lazy qualified as TL
import ParseRESP qualified as Par

data Command
    = PING
    | ECHO
    deriving (Show)

data Request = Request
    { command :: Command
    , payload :: Maybe Par.RESPDataTypes
    }
    deriving (Show)

parsePing :: [Par.RESPDataTypes] -> Request
parsePing [] = Request{command = PING, payload = Nothing}
parsePing [payload] = Request{command = PING, payload = Just payload}
parsePing x = Request{command = PING, payload = Just $ Par.Arrays x}

parseEcho :: [Par.RESPDataTypes] -> Request
parseEcho [] = Request{command = ECHO, payload = Nothing}
parseEcho payload = Request{command = ECHO, payload = Just $ Par.Arrays payload}

processCmdHelper :: (IsString a) => TL.Text -> [Par.RESPDataTypes] -> Either a Request
processCmdHelper com respPayload = case TL.toUpper com of
    "PING" -> Right $ parsePing $ tail respPayload
    "ECHO" -> Right $ parseEcho $ tail respPayload
    _ -> Left "Unknown command"

-- * 1\r\n$4\r\nping\r\n
processCmd :: (IsString a) => ([Par.RedisDataTypes], [Par.RESPDataTypes]) -> Either a Request
processCmd (red, resp) = case red of
    [] -> Left "Empty command"
    (Par.RString com : _) -> processCmdHelper com resp
    _ -> Left "Unexpect redis type"

parse :: (Par.RedisDataTypes, Par.RESPDataTypes) -> Either TL.Text Request
parse (red, resp) = case (red, resp) of
    (Par.RString _, _) -> processCmd ([red], [resp])
    (Par.RArray l, Par.Arrays r) -> processCmd (l, r)
    _ -> Left "No Command Found"
