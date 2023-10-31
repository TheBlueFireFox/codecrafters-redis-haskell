{-# LANGUAGE OverloadedStrings #-}

module Parse.RDB (
    RDB (..),
    AuxField (..),
    Db (..),
    Key,
    Value,
    readFile,
) where

import Control.Monad (unless)
import Data.Bits (Bits (..), shiftL, (.&.))
import Data.Int (Int32, Int64)
import Data.Word (Word64, Word8)
import Prelude hiding (readFile)

import Control.Monad.Except qualified as E
import Data.Binary.Get qualified as G
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Map.Strict qualified as M
import Debug.Trace (trace)
import System.Directory qualified as Dir

type MyGet a = E.ExceptT BL.ByteString G.Get a

{-
    00000000  52 45 44 49 53 30 30 31  30 fa 09 72 65 64 69 73  |REDIS0010..redis|
    00000010  2d 76 65 72 06 37 2e 30  2e 31 31 fa 0a 72 65 64  |-ver.7.0.11..red|
    00000020  69 73 2d 62 69 74 73 c0  40 fa 05 63 74 69 6d 65  |is-bits.@..ctime|
    00000030  c2 9f 0e 3b 65 fa 08 75  73 65 64 2d 6d 65 6d c2  |...;e..used-mem.|
    00000040  10 20 0e 00 fa 08 61 6f  66 2d 62 61 73 65 c0 00  |. ....aof-base..|
    00000050  fe 00 fb 01 00 00 05 6d  79 6b 65 79 05 6d 79 76  |.......mykey.myv|
    00000060  61 6c ff e5 6a 9a d9 9c  bf 00 83                 |al..j......|
    0000006b

    ----------------------------#
    52 45 44 49 53              # Magic String "REDIS"
    30 30 30 33                 # RDB Version Number as ASCII string. "0003" = 3
    ----------------------------
    FA                          # Auxiliary field
    $string-encoded-key         # May contain arbitrary metadata
    $string-encoded-value       # such as Redis version, creation time, used memory, ...
    ----------------------------
    FE 00                       # Indicates database selector. db number = 00
    FB                          # Indicates a resizedb field
    $length-encoded-int         # Size of the corresponding hash table
    $length-encoded-int         # Size of the corresponding expire hash table
    ----------------------------# Key-Value pair starts
    FD $unsigned-int            # "expiry time in seconds", followed by 4 byte unsigned int
    $value-type                 # 1 byte flag indicating the type of value
    $string-encoded-key         # The key, encoded as a redis string
    $encoded-value              # The value, encoding depends on $value-type
    ----------------------------
    FC $unsigned long           # "expiry time in ms", followed by 8 byte unsigned long
    $value-type                 # 1 byte flag indicating the type of value
    $string-encoded-key         # The key, encoded as a redis string
    $encoded-value              # The value, encoding depends on $value-type
    ----------------------------
    $value-type                 # key-value pair without expiry
    $string-encoded-key
    $encoded-value
    ----------------------------
    FE $length-encoding         # Previous db ends, next db starts.
    ----------------------------
    ...                         # Additional key-value pairs, databases, ...

    FF                          ## End of RDB file indicator
    8-byte-checksum             ## CRC64 checksum of the entire file.
-}

data RDB = RDB
    { rdbVersionNr :: Int
    , auxField :: Maybe AuxField
    , dbs :: [Db]
    , fileCheckSum :: BL.ByteString
    }
    deriving (Show)

data AuxField = AuxField
    { redisVersion :: BL.ByteString
    , redisBits :: BL.ByteString
    , ctime :: BL.ByteString
    , usedMem :: BL.ByteString
    }
    deriving (Show)

data Db = Db
    { databaseNr :: Int32
    , store :: KVStore
    , expiryStore :: KVExpiryStore
    }
    deriving (Show)

type Key = BL.ByteString
type Value = BL.ByteString
type KVStore = M.Map Key Value

type ValueExpiry = (BL.ByteString, Maybe Word64)
type KVExpiryStore = M.Map Key ValueExpiry

data ValueTypes
    = StringEncoding
    | ListEncoding
    | SetEncoding
    | SortedEncoding
    | HashEncoding
    | ZipmapEncoding
    | ZiplistEncoding
    | IntsetEncoding
    | SortedZipListEncoding
    | HashmapZipListEncoding
    | ListZipListEncoding
    deriving (Show)

toValueType :: Word8 -> Maybe ValueTypes
toValueType v = case v of
    0 -> Just StringEncoding
    1 -> Just ListEncoding
    2 -> Just SetEncoding
    3 -> Just SortedEncoding
    4 -> Just HashEncoding
    9 -> Just ZipmapEncoding
    10 -> Just ZiplistEncoding
    11 -> Just IntsetEncoding
    12 -> Just SortedZipListEncoding
    13 -> Just HashmapZipListEncoding
    14 -> Just ListZipListEncoding
    _ -> Nothing

data OpCodes
    = EOF
    | SELECTDB
    | EXPIRETIME
    | EXPIRETIMEMS
    | RESIZEDB
    | AUX
    | NONE
    deriving (Show, Eq, Enum)

parseOpcode :: (Eq a, Num a) => a -> OpCodes
parseOpcode opCode = case opCode of
    0xFF -> EOF
    0xFE -> SELECTDB
    0xFD -> EXPIRETIME
    0xFC -> EXPIRETIMEMS
    0xFB -> RESIZEDB
    0xFA -> AUX
    _ -> NONE

getOpcode :: MyGet OpCodes
getOpcode = parseOpcode <$> getWord8

getWord8 :: MyGet Word8
getWord8 = E.lift G.getWord8

getByteString :: Int64 -> MyGet BL.ByteString
getByteString = E.lift . G.getLazyByteString

{-
    Bits 	How to parse
    00 	    The next 6 bits represent the length
    01 	    Read one additional byte. The combined 14 bits represent the length
    10 	    Discard the remaining 6 bits. The next 4 bytes from the stream represent the length
    11 	    The next object is encoded in a special format. The remaining 6 bits indicate the format.
            May be used to store numbers or Strings, see String Encoding
-}

getVariableLenNr :: MyGet Int64
getVariableLenNr = do
    lRaw <- getWord8
    let m = lRaw `shiftL` 6
    let mRes = lRaw .&. complement (0b11 `shiftR` 6)
    case m of
        0b00 -> pure $ fromIntegral mRes
        0b01 -> do
            n <- getWord8
            let nInt64 = fromIntegral n :: Int64
                mResInt64 = fromIntegral mRes :: Int64
            pure $ (mResInt64 `shiftL` 8) .|. nInt64
        0b10 -> fromIntegral <$> E.lift G.getWord32be
        0b11 -> todo
        _ -> E.throwError $ "Impossible lenght detected... No clue how this case will ever be called. <" <> BLC.pack (show m) <> ">"

getDB :: MyGet Db
getDB = do
    dbId <- getVariableLenNr
    resizedbFildIndicatorOp <- getOpcode
    unless (RESIZEDB == resizedbFildIndicatorOp) $ E.throwError "Missing Resize DB Indicator Field"
    lenHashTable <- getVariableLenNr
    lenExpireHashTable <- getVariableLenNr
    pure
        Db
            { databaseNr = fromIntegral dbId
            , store = mempty
            , expiryStore = mempty
            }

getDBs :: MyGet [Db]
getDBs = inner []
  where
    dbHelper acc dbSelOp = case dbSelOp of
        SELECTDB -> (inner . (: acc)) =<< getDB
        EOF -> pure $ reverse acc
        _ -> E.throwError $ "unexpected database database selector <" <> BLC.pack (show (fromEnum dbSelOp)) <> ">"
    inner acc = dbHelper acc =<< getOpcode

getAuxField :: MyGet (Maybe AuxField)
getAuxField = do
    auxField <- getOpcode
    unless (AUX == auxField) $ E.throwError "Missing aux field"
    E.lift skipIt
  where
    skipIt = do
        v <- G.lookAhead G.getWord8
        if SELECTDB /= parseOpcode v
            then do
                _ <- G.getWord8
                skipIt
            else pure Nothing

checkRedisHeader :: MyGet ()
checkRedisHeader = do
    r <- getByteString 5
    unless ("REDIS" == r) $ E.throwError "Missing magic string"

getRedisVersionNr :: MyGet Int
getRedisVersionNr = do
    verDecStr <- getByteString 4
    let ver = BLC.readInt verDecStr
    maybe (E.throwError "Unable to parse Redis Version Nr.") (pure . fst) ver

getChecksum :: MyGet BL.ByteString
getChecksum = getByteString 8

process :: MyGet RDB
process = do
    checkRedisHeader
    version <- getRedisVersionNr
    auxField <- getAuxField
    dbs <- getDBs
    checkSum <- getChecksum
    pure
        RDB
            { rdbVersionNr = version
            , auxField = auxField
            , dbs = dbs
            , fileCheckSum = checkSum
            }

readFile :: FilePath -> IO (Either BL.ByteString RDB)
readFile filePath = do
    b <- Dir.doesFileExist filePath
    if not b
        then pure $ Left "RDB File not found"
        else processHelper <$> BL.readFile filePath
  where
    processHelper = G.runGet (E.runExceptT process)

todo :: a
todo = error "todo"
