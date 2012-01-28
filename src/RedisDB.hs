{-# LANGUAGE OverloadedStrings #-}

{-|

Redis DB snaplet.

-}

module RedisDB

where

import qualified Control.Category as C ((.))
import Control.Monad.State
import Control.Monad.Trans
import Control.Concurrent.MVar
import Data.Lens.Common
import Data.Text (Text)
import Database.Redis.Redis
import Snap.Core
import Snap.Snaplet


------------------------------------------------------------------------------
-- | Description text used in redisDBInit as makeSnaplet argument.
description :: Text
description = "Redis snaplet."


------------------------------------------------------------------------------
-- | Snaplet's data type. DB connection reference is stored.
data RedisDB = RedisDB
    { dbRef :: MVar Redis
    }


------------------------------------------------------------------------------
-- | Get Redis connection handle from RedisDB snaplet.
--
-- @todo Implement WithRedis instance for apps with this.
getRedisDB :: (MonadIO m, MonadState app m) => Lens app (Snaplet RedisDB) -> m Redis
getRedisDB snaplet = do
  (RedisDB cv) <- gets (getL ((C..) snapletValue snaplet))  
  r <- liftIO $ readMVar cv
  return r


------------------------------------------------------------------------------
-- | Make RedisDB snaplet and initialize database connection.
redisDBInit :: String -> String -> SnapletInit b RedisDB
redisDBInit host port = makeSnaplet "snaplet-redis" description Nothing $ do
  conn <- liftIO $ connect host port
  cv <- liftIO $ newEmptyMVar
  liftIO $ putMVar cv conn
  return $ RedisDB cv
