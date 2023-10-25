{-# LANGUAGE OverloadedStrings #-}

module Request (Request (..), parse) where

import Text.Printf (printf)

import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB

import ParseRESP qualified as Par

data Request
    = PING (Maybe Par.RESPDataTypes)
    | ECHO Par.RESPDataTypes
    deriving (Show)

type Error = Par.RESPDataTypes

handleError :: TL.Text -> Error
handleError = Par.SimpleError

strToTeHelper :: String -> TL.Text
strToTeHelper = TLB.toLazyText . TLB.fromString

handleErrorNumArgsRange :: Int -> (Int, Int) -> Error
handleErrorNumArgsRange given (mi, ma)
    | mi == ma = handleError $ strToTeHelper $ printf "wrong number of arguments (given %d, expected %d)" given mi
    | otherwise = handleError $ strToTeHelper $ printf "wrong number of arguments (given %d, expected in range %d - %d)" given mi ma

handleErrorNumArgs :: Int -> Int -> Error
handleErrorNumArgs given expected = handleErrorNumArgsRange given (expected, expected)

parsePing :: [Par.RESPDataTypes] -> Either Error Request
parsePing input = case input of
    [] -> Right $ PING Nothing
    [a@(Par.BulkString _)] -> Right $ PING $ Just a
    [a@(Par.SimpleString _)] -> Right $ PING $ Just a
    [_] -> Left $ handleError "Unsupported Type"
    v -> Left $ handleErrorNumArgsRange (length v) (0, 1)

parseEcho :: [Par.RESPDataTypes] -> Either Error Request
parseEcho payload = case payload of
    [a@(Par.BulkString _)] -> Right $ ECHO a
    [a@(Par.SimpleString _)] -> Right $ ECHO a
    [_] -> Left $ handleError "ERR: unknown data format"
    v -> Left $ handleErrorNumArgs (length v) 1

processCmdHelper :: TL.Text -> [Par.RESPDataTypes] -> Either Error Request
processCmdHelper com respPayload = case TL.toUpper com of
    "PING" -> parsePing $ tail respPayload
    "ECHO" -> parseEcho $ tail respPayload
    _ -> Left $ handleError "Unknown command"

-- * 1\r\n$4\r\nping\r\n
processCmd :: ([Par.RedisDataTypes], [Par.RESPDataTypes]) -> Either Error Request
processCmd (red, resp) = case red of
    (Par.RString com : _) -> processCmdHelper com resp
    [] -> Left $ handleError "Empty command"
    _ -> Left $ handleError "Unexpect redis type"

parse :: (Par.RedisDataTypes, Par.RESPDataTypes) -> Either Error Request
parse (red, resp) = case (red, resp) of
    (Par.RString _, _) -> processCmd ([red], [resp])
    (Par.RArray l, Par.Arrays r) -> processCmd (l, r)
    _ -> Left $ handleError "No Command Found"
