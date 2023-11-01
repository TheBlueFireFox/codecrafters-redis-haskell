module DB.DB (Until, Key, Value (..), DB, insert, insertWith, lookup, mapDB, keys, values) where

import Prelude hiding (lookup)

import Data.Bifunctor (bimap)
import Data.Map.Strict qualified as M
import Data.Word (Word64)

type Key a = a

type Until = Word64

data Value v = Value {payload :: v, timer :: Maybe Until}
    deriving (Show)

type DB k v = M.Map (Key k) (Value v)

insert :: (Ord k) => k -> p -> M.Map k (Value p) -> M.Map k (Value p)
insert key val db =
    let
        value = Value{payload = val, timer = Nothing}
     in
        M.insert key value db

insertWith :: (Ord k) => k -> p -> Word64 -> M.Map k (Value p) -> M.Map k (Value p)
insertWith key val untilNs db =
    let
        value = Value{payload = val, timer = Just untilNs}
     in
        M.insert key value db

lookup :: (Ord k) => k -> Until -> DB k v -> (Maybe v, Maybe (DB k v))
lookup key cTime db = inner $ M.lookup key db
  where
    helper payload Nothing = (Just payload, Nothing)
    helper payload (Just un)
        | un > cTime = (Just payload, Nothing)
        | otherwise = (Nothing, Just (M.delete key db))

    inner Nothing = (Nothing, Nothing)
    inner (Just (Value{payload, timer})) = helper payload timer

mapDB :: (Ord k2) => (k1 -> k2) -> (v1 -> v2) -> DB k1 v1 -> DB k2 v2
mapDB fk fv = M.fromList . inner . M.toList
  where
    inner = map (bimap fk mValue)
    mValue Value{payload, timer} = Value{payload = fv payload, timer}

keys :: M.Map b1 b2 -> [b1]
keys = map fst . M.toList

values :: M.Map a (Value b) -> [b]
values = map (payload . snd) . M.toList
