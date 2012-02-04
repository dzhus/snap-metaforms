{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Backbone.sync handler snaplet with Redis storage.

-}

module Redbone (Redbone
               , redboneInit)
where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State

import Data.Aeson as A

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BZ (ByteString)
import qualified Data.ByteString.UTF8 as BU (fromString)
import qualified Data.ByteString.Lazy.UTF8 as BZU (fromString)

import Data.Lens.Common
import Data.Lens.Template

import qualified Data.Map as M

import Data.Maybe

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import Text.Templating.Heist

import Network.WebSockets
import Network.WebSockets.Snap
import qualified Network.WebSockets.Util.PubSub as PS

import RedisDB
import Database.Redis.Redis

import Util


data Event = Create | Update | Delete

------------------------------------------------------------------------------
-- | Event which can occur to model instance.
data ModelEvent = ModelEvent String String Event

------------------------------------------------------------------------------
-- | Redbone snaplet state type.
data Redbone = Redbone
             { _database :: Snaplet RedisDB
             , _events :: PS.PubSub Hybi10
             }

makeLens ''Redbone


------------------------------------------------------------------------------
-- | Render empty form for model.
emptyForm :: HasHeist b => Handler b Redbone ()
emptyForm = ifTop $ render "index"


------------------------------------------------------------------------------
-- | Serve JSON metamodel.
metamodel :: Handler b Redbone ()
metamodel = ifTop $ do
  modelName <- getModelName
  serveFile ("resources/static/js/models/" ++ modelName ++ ".js")


------------------------------------------------------------------------------
-- | Extract model name from request path parameter.
getModelName:: MonadSnap m => m String
getModelName = fromParam "model"


------------------------------------------------------------------------------
-- | Extract model id from request parameter.
getModelId:: MonadSnap m => m String
getModelId= fromParam "id"


------------------------------------------------------------------------------
-- | Build Redis key given model name and id
modelKey :: String -> String -> String
modelKey model id = model ++ ":" ++ id


------------------------------------------------------------------------------
-- | Get Redis key which stores id counter for model
modelIdKey :: String -> String
modelIdKey model = "global:" ++ model ++ ":id"


------------------------------------------------------------------------------
-- | Get Redis key which stores timeline for model
modelTimeline :: String -> String
modelTimeline model = "global:" ++ model ++ ":timeline"


------------------------------------------------------------------------------
-- | Get Redis handle and model instance key
--
-- @see getRedisDB
-- @todo WithRedis
prepareRedis :: (MonadSnap m, MonadState app m) => Lens app (Snaplet RedisDB) -> m (Redis, String)
prepareRedis snaplet = do
  model <- getModelName
  id <- getModelId
  db <- getRedisDB snaplet
  return (db, modelKey model id)

------------------------------------------------------------------------------
-- | Builder for WebSockets message containing JSON describing creation or
-- deletion of model instance.
--
modelMessage :: String -> (String -> String -> Network.WebSockets.Message p)
modelMessage event = \id model ->
    let
        response :: [(String, String)]
        response = [("event", event),
                    ("id", id), 
                    ("model", model)]
    in
      DataMessage $ Text $ A.encode $ M.fromList response


creationMessage = modelMessage "create"
deletionMessage = modelMessage "delete"


------------------------------------------------------------------------------
-- | Encode Redis HGETALL reply to ByteString with JSON.
--
-- @internal Note using explicit ByteString type over BS s as
-- suggested by redis because BS s doesn't imply ToJSON s
hgetallToJson :: [ByteString] -> BZ.ByteString
hgetallToJson r = A.encode $ M.fromList (toPairs r)


------------------------------------------------------------------------------
-- | Decode ByteString with JSON to list of hash keys & values for
-- Redis HMSET
--
-- @return Nothing if parsing failed
jsonToHsetall :: BZ.ByteString -> Maybe [(ByteString, ByteString)]
jsonToHsetall s =
    let
        j = A.decode s
    in
      case j of
        Nothing -> Nothing
        Just m -> 
             -- Omit fields with null values and "id" key
            Just (map (\(k, v) -> (k, fromJust v)) $
                  filter (\(k, v) -> (isJust v && k /= "id")) $
                  M.toList m)


------------------------------------------------------------------------------
-- | Create new instance in Redis.
create :: Handler b Redbone ()
create = ifTop $ do
  -- Parse request body to list of pairs
  -- @todo Use readRequestBody
  j <- jsonToHsetall <$> getRequestBody
  when (isNothing j)
       serverError

  db <- getRedisDB database

  -- Take id from global:model:id
  model <- getModelName
  r <- liftIO $ incr db $ modelIdKey model
  newId <- show <$> fromRInt r

  -- Save new instance
  r <- liftIO $ hmset db (modelKey model newId) (fromJust j)
  s <- liftIO $ lpush db (modelTimeline model) newId
  ps <- gets _events
  liftIO $ PS.publish ps $ creationMessage model newId

  -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.5:
  --
  -- the response SHOULD be 201 (Created) and contain an entity which
  -- describes the status of the request and refers to the new
  -- resource
  modifyResponse $ (setContentType "application/json" . setResponseCode 201)
  -- Tell client new instance id in response JSON.
  writeLBS $ A.encode $ M.fromList $ ("id", BU.fromString newId):(fromJust j)
  return ()


------------------------------------------------------------------------------
-- | Read instance from Redis.
read' :: HasHeist b => Handler b Redbone ()
read' = ifTop $ do
  (db, key) <- prepareRedis database
  id <- fromParam "id"
  when (null id)
       pass

  r <- liftIO $ hgetall db key
  j <- fromRMultiBulk' r
  when (null j)
      notFound

  modifyResponse $ setContentType "application/json"
  writeLBS (hgetallToJson j)


------------------------------------------------------------------------------
-- | Serve list of 10 latest instances stored in Redis.
--
-- @todo Adjustable item limit.
timeline :: HasHeist b => Handler b Redbone ()
timeline = ifTop $ do
  (db, key) <- prepareRedis database
  model <- getModelName

  r <- liftIO $ lrange db (modelTimeline model) (0, 9)
  j <- fromRMultiBulk' r

  modifyResponse $ setContentType "application/json"
  writeLBS (enc' j)
    where
      enc' :: [ByteString] -> BZ.ByteString
      enc' j = A.encode j


------------------------------------------------------------------------------
-- | WebSockets handler which pushes instance creation/deletion events
-- to client.
--
-- @todo Adjustable item limit.
modelEvents :: HasHeist b => Handler b Redbone ()
modelEvents = ifTop $ do
  ps <- gets _events
  liftSnap $ runWebSocketsSnap (\r -> do
                                  acceptRequest r
                                  PS.subscribe ps)
  

------------------------------------------------------------------------------
-- | Update existing instance in Redis.
update :: Handler b Redbone ()
update = ifTop $ do
  j <- jsonToHsetall <$> getRequestBody
  when (isNothing j)
       serverError

  (db, key) <- prepareRedis database
  -- @todo Report 201 if previously existed
  r <- liftIO $ hmset db key (fromJust j)
  modifyResponse $ setResponseCode 204
  return()

------------------------------------------------------------------------------
-- | Delete instance from Redis (including timeline).
delete :: Handler b Redbone ()
delete = ifTop $ do
  (db, key) <- prepareRedis database
  id <- getModelId
  model <- getModelName

  -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.7
  --
  -- A successful response SHOULD be 200 (OK) if the response includes
  -- an entity describing the status
  r <- liftIO $ hgetall db key
  j <- fromRMultiBulk' r
  when (null j)
      notFound

  -- @todo What if key gets removed between two queries?
  r <- liftIO $ lrem db (modelTimeline model) 1 id
  r <- liftIO $ del db key
  n <- fromRInt r
  when (n == 0)
       notFound

  ps <- gets _events
  liftIO $ PS.publish ps $ creationMessage model id
  modifyResponse $ setContentType "application/json"
  writeLBS (hgetallToJson j)


-----------------------------------------------------------------------------
-- | CRUD routes for models.
routes :: HasHeist b => [(ByteString, Handler b Redbone ())]
routes = [ (":model/", method GET emptyForm)
         , (":model/model", method GET metamodel)
         , (":model/timeline", method GET timeline)
         , (":model/events", modelEvents)
         , (":model", method POST create)
         , (":model/:id", method GET read')
         , (":model/:id", method PUT update)
         , (":model/:id", method DELETE delete)
         ]


------------------------------------------------------------------------------
-- | Connect to Redis and set routes.
redboneInit :: HasHeist b => SnapletInit b Redbone
redboneInit = makeSnaplet "redbone" "Backbone.js backend with Redis storage" Nothing $
          do
            r <- nestSnaplet "" database $ redisDBInit "127.0.0.1" "6379"
            p <- liftIO PS.newPubSub
            addRoutes routes
            return $ Redbone r p
