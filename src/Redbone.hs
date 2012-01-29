{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Backbone.sync handler snaplet with Redis storage.

-}

module Redbone where

import Control.Applicative
import Control.Monad.Trans

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
-- | Build Redis key given model name and id
redisKey :: String -> String -> String
redisKey modelName id = modelName ++ ":" ++ id


------------------------------------------------------------------------------
-- | Encode Redis HGETALL reply to JSON.
--
-- @internal Note using explicit ByteString type over BS s as
-- suggested by redis because BS s doesn't imply ToJSON s
hgetallToJson :: [ByteString] -> BZ.ByteString
hgetallToJson r = A.encode $ M.fromList (fromPairs r)


------------------------------------------------------------------------------
-- | Read instance from Redis.
read' :: Handler b Redbone ()
read' = ifTop $ do
  model <- getModelName
  id <- fromParam "id"
  db <- getRedisDB database
  r <- liftIO $ hgetall db $ redisKey model id
  j <- fromRMultiBulk' r

  putResponse resp
  -- @todo Do this with liftM
  let
      z = hgetallToJson j
   in
     writeLBS z
    where
      resp = setContentType "application/json" emptyResponse

-----------------------------------------------------------------------------
-- | CRUD routes for models.
routes :: HasHeist b => [(ByteString, Handler b Redbone ())]
routes = [ ("/:model/", method GET emptyForm)
         , ("/:model/model", method GET metamodel)
           
--         , ("/:model/", method POST create)
         , ("/:model/:id/", method GET read')
--         , ("/:model/:id", method DELETE delete)
         ]


------------------------------------------------------------------------------
-- | Connect to Redis and set routes.
redboneInit :: HasHeist b => SnapletInit b Redbone
redboneInit = makeSnaplet "redbone" "Backbone.js backend with Redis storage" Nothing $
          do
            r <- nestSnaplet "" database $ redisDBInit "127.0.0.1" "6379"
            addRoutes routes
            return $ Redbone r
