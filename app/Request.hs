{-# LANGUAGE OverloadedStrings #-}

module Request (Request (..), parse, Key, Value) where

import Text.Printf (printf)

import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Read qualified as TR
import Data.Text.Lazy.Encoding qualified as TE

import Control.Arrow (left, right)
import Data.Word (Word64)
import Parse.RESP qualified as Par

type Key = Par.RESPDataTypes
type Value = Par.RESPDataTypes
type ExpireTimeMS = Maybe Word64

data Request
    = Ping (Maybe Par.RESPDataTypes)
    | Echo Par.RESPDataTypes
    | Set Key Value ExpireTimeMS
    | Get Key
    | ConfigGet [Par.RESPDataTypes]
    | Keys Par.RESPDataTypes
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
    [] -> Right $ Ping Nothing
    [a@(Par.BulkString _)] -> Right $ Ping $ Just a
    [a@(Par.SimpleString _)] -> Right $ Ping $ Just a
    [_] -> Left $ handleError "Unsupported Type"
    v -> Left $ handleErrorNumArgsRange (length v) (0, 1)

parseEcho :: [Par.RESPDataTypes] -> Either Error Request
parseEcho payload = case payload of
    [a@(Par.BulkString _)] -> Right $ Echo a
    [a@(Par.SimpleString _)] -> Right $ Echo a
    [_] -> Left $ handleError "ERR: unknown data format"
    v -> Left $ handleErrorNumArgs (length v) 1

extractor :: Par.RESPDataTypes -> Either Error TL.Text
extractor p = case p of
    Par.BulkString v -> Right $ TE.decodeUtf8 v
    Par.SimpleString v -> Right v
    _ -> Left $ handleError "Incorrect Type"

parseSet :: [Par.RESPDataTypes] -> Either Error Request
parseSet payload = case payload of
    (a@(Par.BulkString _) : b : ex) -> Set a b <$> parseSetExpireHelper ex
    (a@(Par.SimpleString _) : b : ex) -> Set a b <$> parseSetExpireHelper ex
    v -> Left $ handleErrorNumArgs (length v) 2
  where
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
    [a@(Par.BulkString _)] -> Right $ Get a
    [a@(Par.SimpleString _)] -> Right $ Get a
    [_] -> Left $ handleError "ERR: unknown data format"
    v -> Left $ handleErrorNumArgs (length v) 1

parseConfigGet :: [Par.RESPDataTypes] -> Either Error Request
parseConfigGet payload = p =<< inner [] payload
  where
    p [] = Left $ handleErrorNumArgs 0 1
    p pp = Right $ ConfigGet pp
    inner acc [] = Right acc
    inner acc (x : xs) = case x of
        a@(Par.BulkString _) -> inner (a : acc) xs
        a@(Par.SimpleString _) -> inner (a : acc) xs
        _ -> Left $ handleError "ERR: unknown data format"

parseConfig :: [Par.RESPDataTypes] -> Either Error Request
parseConfig [] = Left $ handleErrorNumArgs 0 1
parseConfig (v : vs) = p =<< extractor v
  where
    p com = case TL.toUpper com of
        "GET" -> parseConfigGet vs
        _ -> Left $ handleError "Unknown command"

parseKeys :: [Par.RESPDataTypes] -> Either Error Request
parseKeys [v@(Par.BulkString _)] = Right $ Keys v
parseKeys [v@(Par.SimpleString _)] = Right $ Keys v
parseKeys v = Left $ handleErrorNumArgs (length v) 1

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
        "CONFIG" -> handler parseConfig
        "KEYS" -> handler parseKeys
        _ -> Left $ handleError "Unknown command"

parse :: (Par.RedisDataTypes, Par.RESPDataTypes) -> Either Error Request
parse (red, resp) = case (red, resp) of
    (Par.RString _, _) -> processCmd ([red], [resp])
    (Par.RArray l, Par.Arrays r) -> processCmd (l, r)
    _ -> Left $ handleError "No Command Found"
