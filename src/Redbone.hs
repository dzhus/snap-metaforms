{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Backbone.sync handler snaplet with Redis storage.

-}

module Redbone where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State

import Data.Aeson as A

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BZ

import Data.Lens.Common
import Data.Lens.Template
import qualified Data.Map as M
import Data.Maybe

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import Text.Templating.Heist

import RedisDB
import Database.Redis.Redis

import Util

data Redbone = Redbone
             { _database :: Snaplet RedisDB
             }

makeLens ''Redbone


------------------------------------------------------------------------------
-- | Render empty form for model.
emptyForm :: HasHeist b => Handler b Redbone ()
emptyForm = ifTop $ render "index"


------------------------------------------------------------------------------
-- | Get JSON metamodel.
metamodel :: Handler b Redbone ()
metamodel = ifTop $ do
  modelName <- getModelName
  serveFile ("resources/static/js/models/" ++ modelName ++ ".js")


------------------------------------------------------------------------------
-- | Extract model name from request path parameter.
getModelName:: MonadSnap m => m String
getModelName = fromParam "model"


------------------------------------------------------------------------------
-- | Get Redis handle and model instance key
--
-- @see getRedisDB
-- @todo WithRedis
prepareRedis :: (MonadSnap m, MonadState app m) => Lens app (Snaplet RedisDB) -> m (Redis, String)
prepareRedis snaplet = do
  model <- getModelName
  id <- fromParam "id"
  db <- getRedisDB snaplet
  return (db, redisKey model id)
      where
        redisKey modelName id = modelName ++ ":" ++ id


------------------------------------------------------------------------------
-- | Encode Redis HGETALL reply to JSON.
--
-- @internal Note using explicit ByteString type over BS s as
-- suggested by redis because BS s doesn't imply ToJSON s
hgetallToJson :: [ByteString] -> BZ.ByteString
hgetallToJson r = A.encode $ M.fromList (toPairs r)




------------------------------------------------------------------------------
-- | Read instance from Redis.
read' :: Handler b Redbone ()
read' = ifTop $ do
  (db, key) <- prepareRedis database
  r <- liftIO $ hgetall db key
  j <- fromRMultiBulk' r

  when (null j)
      notFound
  modifyResponse $ setContentType "application/json"
  writeLBS (hgetallToJson j)

------------------------------------------------------------------------------
-- | Delete instance from Redis.
delete :: Handler b Redbone ()
delete = ifTop $ do
  (db, key) <- prepareRedis database

  -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.7
  -- Return removed entity description when DELETE is successfull
  r <- liftIO $ hgetall db key
  j <- fromRMultiBulk' r
  when (null j)
      notFound

  -- What if key gets removed between two queries?
  r <- liftIO $ del db key
  n <- fromRInt r
  when (n == 0)
       notFound
  modifyResponse $ setContentType "application/json"
  writeLBS (hgetallToJson j)

-----------------------------------------------------------------------------
-- | CRUD routes for models.
routes :: HasHeist b => [(ByteString, Handler b Redbone ())]
routes = [ (":model/", method GET emptyForm)
         , (":model/model", method GET metamodel)
           
--         , ("/:model/", method POST create)
         , (":model/:id", method GET read')
         , (":model/:id", method DELETE delete)
         ]


------------------------------------------------------------------------------
-- | Connect to Redis and set routes.
redboneInit :: HasHeist b => SnapletInit b Redbone
redboneInit = makeSnaplet "redbone" "Backbone.js backend with Redis storage" Nothing $
          do
            r <- nestSnaplet "" database $ redisDBInit "127.0.0.1" "6379"
            addRoutes routes
            return $ Redbone r
