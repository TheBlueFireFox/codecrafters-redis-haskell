module ConcurrentMemory (DB, newDB, fromDB, toDB, insert, insertWith, lookup) where

import Prelude hiding (lookup)

import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TVar qualified as TVar
import Data.Map.Strict qualified as M
import Data.Time qualified as T
import Data.Time.Clock.POSIX qualified as TP
import Data.Word (Word64)

import DB.DB qualified as DB

type DB k v = TVar.TVar (DB.DB k v)

newDB :: IO (DB k v)
newDB = STM.atomically $ STM.newTVar M.empty

fromDB :: DB.DB k v -> IO (DB k v)
fromDB = STM.atomically . STM.newTVar

toDB :: DB k v -> IO (DB.DB k v)
toDB = STM.readTVarIO

insert :: (Ord k) => k -> v -> DB k v -> IO ()
insert key val db = STM.atomically $ do
    m <- TVar.readTVar db
    TVar.writeTVar db $ DB.insert key val m

insertWith :: (Ord k) => k -> v -> DB.Until -> DB k v -> IO ()
insertWith key val expMs db = do
    t <- getTimeStamp
    let tNs = expMs * 1000000
    let expNs = t + tNs
    STM.atomically $ do
        m <- TVar.readTVar db
        TVar.writeTVar db $ DB.insertWith key val expNs m

lookup :: (Ord k) => k -> DB k v -> IO (Maybe v)
lookup key db = do
    t <- getTimeStamp
    STM.atomically $ do
        m <- STM.readTVar db
        let (res, nextDB) = DB.lookup key t m
        case nextDB of
            Nothing -> pure res
            Just newM -> do
                _ <- STM.writeTVar db newM
                pure res

-- returns in NS
getTimeStamp :: IO Word64
getTimeStamp = h <$> T.getCurrentTime
  where
    h = floor . (1e9 *) . T.nominalDiffTimeToSeconds . TP.utcTimeToPOSIXSeconds
