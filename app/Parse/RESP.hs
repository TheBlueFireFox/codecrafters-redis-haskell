{-# LANGUAGE OverloadedStrings #-}

module Parse.RESP (serialize, deserialize, RESPDataTypes (..), fromRESPDataTypes, RedisDataTypes (..)) where

import Control.Arrow (Arrow (first))
import Data.Int (Int64)
import Data.Word (Word8)

import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TE

data RedisDataTypes
    = RString TL.Text
    | RError TL.Text
    | RInteger Int64
    | RArray [RedisDataTypes]
    | RNull
    deriving (Show)

fromRESPDataTypes :: RESPDataTypes -> RedisDataTypes
fromRESPDataTypes dt = case dt of
    SimpleError e -> RError e
    SimpleString v -> RString v
    Integers v -> RInteger v
    BulkString v -> RString $ TE.decodeUtf8 v
    Arrays x -> RArray $ map fromRESPDataTypes x
    NullArrays -> RNull
    NullString -> RNull

{-
-    Simple Strings   RESP2   Simple      +
-    Simple Errors    RESP2   Simple      -
-    Integers         RESP2   Simple      :
-    Bulk Strings     RESP2   Aggregate   $
-    Arrays           RESP2   Aggregate   *
-
-    *2\r\n$4\r\nECHO\r\n$3\r\nhey\r\n
-}
data RESPDataTypes
    = SimpleError TL.Text
    | SimpleString TL.Text
    | BulkString BL.ByteString
    | NullString
    | Integers Int64
    | Arrays [RESPDataTypes]
    | NullArrays
    deriving (Show, Ord, Eq)

sep :: BLC.ByteString
sep = "\r\n"

retCar :: Word8
retCar = toEnum $ fromEnum '\r'

parseSimpleStringHelper :: BLC.ByteString -> Maybe (TL.Text, BLC.ByteString)
parseSimpleStringHelper res = do
    let (str, x) = first TE.decodeASCII $ BL.span (/= retCar) res
    xx <- parseSeparator x
    pure (str, xx)

parseSimpleString :: BL.ByteString -> Maybe (RESPDataTypes, BL.ByteString)
parseSimpleString res = first SimpleString <$> parseSimpleStringHelper res

parseSimpleError :: BLC.ByteString -> Maybe (RESPDataTypes, BLC.ByteString)
parseSimpleError res = first SimpleError <$> parseSimpleStringHelper res

parseIntegers :: BLC.ByteString -> Maybe (RESPDataTypes, BLC.ByteString)
parseIntegers res = do
    (val, res') <- parseDigits res
    res'' <- parseSeparator res'
    pure (Integers val, res'')

parseBulkString :: BLC.ByteString -> Maybe (RESPDataTypes, BLC.ByteString)
parseBulkString res = do
    (size, noDig) <- parseDigits res
    justStr <- parseSeparator noDig
    if size == (-1)
        then pure (NullString, justStr)
        else process size justStr
  where
    process size justStr = do
        let (str, restWithSep) = BL.splitAt size justStr
        noSep <- parseSeparator restWithSep
        pure (BulkString str, noSep)

{-
 -  *2\r\n$5\r\nhello\r\n$5\r\nworld\r\n
 --}
parseArray :: BLC.ByteString -> Maybe (RESPDataTypes, BLC.ByteString)
parseArray res = do
    (i, noDig) <- parseDigits res
    justStr <- parseSeparator noDig
    if i == (-1)
        then pure (NullArrays, justStr)
        else first Arrays <$> inner i [] justStr
  where
    inner i acc currStr
        | i == 0 = pure (reverse acc, currStr)
        | otherwise = do
            (curr, resStr) <- parseDataType currStr
            inner (i - 1) (curr : acc) resStr

-- safety we know that we can use fromIntegral here safely
-- as the the redis spec define it so
parseDigits :: BLC.ByteString -> Maybe (Int64, BLC.ByteString)
parseDigits res = first fromIntegral <$> BLC.readInteger res

parseSeparator :: BLC.ByteString -> Maybe BLC.ByteString
parseSeparator = BLC.stripPrefix sep

{-
-    Simple Strings   RESP2   Simple      +
-    Simple Errors    RESP2   Simple      -
-    Integers         RESP2   Simple      :
-    Bulk Strings     RESP2   Aggregate   $
-    Arrays           RESP2   Aggregate   *
-
-    *2\r\n$4\r\nECHO\r\n$3\r\nhey\r\n
-}
parseDataType :: BLC.ByteString -> Maybe (RESPDataTypes, BLC.ByteString)
parseDataType input = do
    (c, res) <- BLC.uncons input
    case c of
        '+' -> parseSimpleString res
        '-' -> parseSimpleError res
        ':' -> parseIntegers res
        '$' -> parseBulkString res
        '*' -> parseArray res
        _ -> Nothing

{-
-    Simple Strings   RESP2   Simple      +     +OK\r\n
-    Simple Errors    RESP2   Simple      -     -Error message\r\n
-    Integers         RESP2   Simple      :     :[<+|->]<value>\r\n
-    Bulk Strings     RESP2   Aggregate   $     $<length>\r\n<data>\r\n
-    Arrays           RESP2   Aggregate   *     *<number-of-elements>\r\n<element-1>...<element-n>
-
-    *2\r\n$4\r\nECHO\r\n$3\r\nhey\r\n
-}
serialize :: RESPDataTypes -> BL.ByteString
serialize input = case input of
    SimpleString v -> "+" <> enc v <> sep
    SimpleError v -> "-" <> enc v <> sep
    Integers v -> ":" <> int v <> sep
    BulkString v -> "$" <> int (BL.length v) <> sep <> v <> sep
    Arrays v -> "*" <> int (toEnum (length v)) <> sep <> mconcat (map serialize v)
    NullArrays -> "$-1" <> sep
    NullString -> "$-1" <> sep
  where
    enc = TE.encodeUtf8

    int = BB.toLazyByteString . BB.int64Dec

deserialize :: BL.ByteString -> Maybe RESPDataTypes
deserialize values = fst <$> parseDataType values
