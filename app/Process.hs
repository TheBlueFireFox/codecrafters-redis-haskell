{-# LANGUAGE OverloadedStrings #-}

module Process (process) where

import Data.ByteString.Lazy qualified as BL
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Debug.Trace (trace)
import ParseRESP qualified as ResPar
import Request qualified as Req
import Text.Printf (printf)
import qualified ParseRESP as ResPair

type Request = Req.Request
type Response = ResPar.RESPDataTypes
type Payload = Maybe ResPar.RESPDataTypes

strToTeHelper :: String -> TL.Text
strToTeHelper = TLB.toLazyText . TLB.fromString

handleError :: TL.Text -> Response
handleError = ResPar.SimpleError

handleErrorNumArgsRange :: Int -> (Int, Int) -> Response
handleErrorNumArgsRange given (mi, ma)
    | mi == ma = handleError $ strToTeHelper $ printf "wrong number of arguments (given %d, expected %d)" given mi
    | otherwise = handleError $ strToTeHelper $ printf "wrong number of arguments (given %d, expected in range %d - %d)" given mi ma

handleErrorNumArgs :: Int -> Int -> ResPar.RESPDataTypes
handleErrorNumArgs given expected = handleErrorNumArgsRange given (expected, expected)

handlePing :: Payload -> Response
handlePing (Just (ResPar.Arrays [])) = ResPar.SimpleString "PONG"
handlePing (Just (ResPar.Arrays [a])) = handlePing $ Just a
handlePing (Just (ResPar.Arrays v)) = handleErrorNumArgsRange (length v) (0, 1)
handlePing (Just (ResPair.BulkString a)) = ResPair.BulkString a
handlePing (Just (ResPair.SimpleString a)) = ResPair.SimpleString a
handlePing Nothing = ResPar.SimpleString "PONG"
handlePing t = trace ("payload " ++ show t) $ handleError "Unsupported Type for <PING>"

handleEcho :: Payload -> Response
handleEcho Nothing = handleErrorNumArgs 0 1
handleEcho (Just payload) = case payload of
    ResPar.Arrays [a] -> a
    ResPar.Arrays vs -> handleErrorNumArgs (length vs) 1
    _ -> handleError "ERR: unknown data format"

handleCommands :: Request -> Response
handleCommands Req.Request{Req.command, Req.payload} = case command of
    Req.PING -> handlePing payload
    Req.ECHO -> handleEcho payload

processHelper :: BL.ByteString -> Response
processHelper = maybe (handleError "Unable to parse request") parse . ResPar.deserialize
  where
    parse parsable = either handleError handleCommands $ Req.parse (ResPar.fromRESPDataTypes parsable, parsable)

process :: BL.ByteString -> BL.ByteString
process = ResPar.serialize . processHelper
