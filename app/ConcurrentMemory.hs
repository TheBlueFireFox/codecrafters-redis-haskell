module ConcurrentMemory (DB, newDB, insert, insertWith, lookup) where

import Prelude hiding (lookup)

import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TVar qualified as TVar
import Data.Map.Strict qualified as M
import Data.Time qualified as T
import Data.Time.Clock.POSIX qualified as TP
import Data.Word (Word64)

type Until = Word64

data Value v = Value {payload :: v, timer :: Maybe Until}
    deriving (Show)

type DB k v = TVar.TVar (M.Map k (Value v))

newDB :: IO (TVar.TVar (M.Map k v))
newDB = STM.atomically $ STM.newTVar M.empty

insert :: (Ord k) => k -> v -> DB k v -> IO ()
insert key val db = STM.atomically $ do
    m <- TVar.readTVar db
    let value = Value{payload = val, timer = Nothing}
    let mWith = M.insert key value m
    TVar.writeTVar db mWith

insertWith :: (Ord k) => k -> v -> Until -> DB k v -> IO ()
insertWith key val expMs db = do
    t <- getTimeStamp
    let tNs = expMs * 1000000
    let expNs = t + tNs
    -- putStrLn $ "time now:" ++ show t ++ " until: " ++ show expNs
    STM.atomically $ helper expNs
  where
    helper expNs = do
        m <- TVar.readTVar db
        let value = Value{payload = val, timer = Just expNs}
        let mWith = M.insert key value m
        TVar.writeTVar db mWith

lookup :: (Ord k) => k -> DB k v -> IO (Maybe v)
lookup key db = do
    t <- getTimeStamp
    STM.atomically $ do
        m <- STM.readTVar db
        let val = M.lookup key m
        maybe (pure Nothing) (helper t m) val
  where
    helper t m (Value{payload, timer}) = case timer of
        Nothing -> pure $ Just payload
        Just un ->
            if un > t
                then pure $ Just payload
                else Nothing <$ STM.writeTVar db (M.delete key m)

-- returns in NS
getTimeStamp :: IO Word64
getTimeStamp = h <$> T.getCurrentTime
  where
    h = floor . (1e9 *) . T.nominalDiffTimeToSeconds . TP.utcTimeToPOSIXSeconds
