module ConcurrentMemory (DB, newDB, insert, lookup) where

import Prelude hiding (lookup)

import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TVar qualified as TVar
import Data.Map.Strict qualified as M

type DB k v = TVar.TVar (M.Map k v)

newDB :: IO (TVar.TVar (M.Map k v))
newDB = STM.atomically $ STM.newTVar M.empty

insert :: (Ord k) => k -> v -> DB k v -> IO ()
insert key val db = STM.atomically $ do
    m <- TVar.readTVar db
    let mWith = M.insert key val m
    TVar.writeTVar db mWith

lookup :: (Ord k) => k -> DB k v -> IO (Maybe v)
lookup key db = M.lookup key <$> STM.readTVarIO db
