{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Redis DB snaplet.

-}

module RedisDB (RedisDB
                , withRedisDB
                , redisDBInit)
where

import Prelude hiding ((.)) 
import Control.Category ((.))
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Control

import Data.Lens.Common
import Data.Lens.Template
import Data.Text (Text)

import Data.Pool
import Database.Redis.Redis

import Data.Time.Clock

import Snap.Core
import Snap.Snaplet


------------------------------------------------------------------------------
-- | Description text used in redisDBInit as makeSnaplet argument.
description :: Text
description = "Redis snaplet."

keepAlive :: NominalDiffTime
keepAlive = 60

poolSize = 5
subpoolSize = 1

------------------------------------------------------------------------------
-- | Snaplet's data type. DB connection pool is stored.
data RedisDB = RedisDB
    { _dbPool :: Pool Redis
    }

makeLens ''RedisDB

------------------------------------------------------------------------------
-- | Perform action using Redis connection from RedisDB snaplet pool.
--
-- @todo Implement WithRedis instance for apps with this.
withRedisDB :: (MonadBaseControl IO m, MonadState app m) => Lens app (Snaplet RedisDB) -> (Redis -> m b) -> m b
withRedisDB snaplet action = do
  p <- gets $ getL (dbPool . snapletValue . snaplet)
  withResource p action


------------------------------------------------------------------------------
-- | Make RedisDB snaplet and initialize database connection.
redisDBInit :: String -> String -> SnapletInit b RedisDB
redisDBInit host port = makeSnaplet "snaplet-redis" description Nothing $ do
  pool <- liftIO $ 
    createPool (connect host port) disconnect poolSize keepAlive subpoolSize
  return $ RedisDB pool
