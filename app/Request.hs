{-# LANGUAGE OverloadedStrings #-}

module Request (Request (..), parse, Key, Value) where

import Text.Printf (printf)

import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Read qualified as TR

import Control.Arrow (left, right)
import Data.Word (Word64)
import ParseRESP qualified as Par

type Key = Par.RESPDataTypes
type Value = Par.RESPDataTypes
type ExpireTimeMS = Maybe Word64

data Request
    = PING (Maybe Par.RESPDataTypes)
    | ECHO Par.RESPDataTypes
    | SET Key Value ExpireTimeMS
    | GET Key
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

parseSet :: [Par.RESPDataTypes] -> Either Error Request
parseSet payload = case payload of
    (a@(Par.BulkString _) : b : ex) -> SET a b <$> parseSetExpireHelper ex
    (a@(Par.SimpleString _) : b : ex) -> SET a b <$> parseSetExpireHelper ex
    v -> Left $ handleErrorNumArgs (length v) 2
  where
    extractor p = case p of
        Par.BulkString v -> Right v
        Par.SimpleString v -> Right v
        _ -> Left $ handleError "Incorrect Type"

    parseSetExpireHelper d = case d of
        [] -> Right Nothing
        [v, p] -> do
            rp <- extractor p
            rv <- extractor v
            processSetExpireHelper rv rp
        v -> Left $ handleErrorNumArgs (length v) 2

    processSetExpireHelper v p = case TL.toUpper v of
        "PX" -> left (\c -> handleError ("Error: " <> TL.pack c)) . right (Just . fst) $ toInt p
        _ -> Left $ handleError "Unexpected \"Set\" extension"

    toInt = TR.decimal

parseGet :: [Par.RESPDataTypes] -> Either Error Request
parseGet payload = case payload of
    [a@(Par.BulkString _)] -> Right $ GET a
    [a@(Par.SimpleString _)] -> Right $ GET a
    [_] -> Left $ handleError "ERR: unknown data format"
    v -> Left $ handleErrorNumArgs (length v) 1

-- * 1\r\n$4\r\nping\r\n
processCmd :: ([Par.RedisDataTypes], [Par.RESPDataTypes]) -> Either Error Request
processCmd (red, resp) = case red of
    (Par.RString com : _) -> processCmdHelper com
    [] -> Left $ handleError "Empty command"
    _ -> Left $ handleError "Unexpect redis type"
  where
    handler p = p $ tail resp
    processCmdHelper com = case TL.toUpper com of
        "PING" -> handler parsePing
        "ECHO" -> handler parseEcho
        "SET" -> handler parseSet
        "GET" -> handler parseGet
        _ -> Left $ handleError "Unknown command"

parse :: (Par.RedisDataTypes, Par.RESPDataTypes) -> Either Error Request
parse (red, resp) = case (red, resp) of
    (Par.RString _, _) -> processCmd ([red], [resp])
    (Par.RArray l, Par.Arrays r) -> processCmd (l, r)
    _ -> Left $ handleError "No Command Found"
