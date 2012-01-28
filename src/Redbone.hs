{-# LANGUAGE OverloadedStrings #-}

{-|

Backbone.sync handler snaplet with Redis storage

-}

module Redbone where

import Control.Applicative
import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import Data.Lens.Template
import Data.Maybe

import Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Internal

import Database.Redis.Redis

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import Text.Templating.Heist

data Redbone = Redbone
    { _redis :: Redis
    }


------------------------------------------------------------------------------
-- | Render empty form for model.
emptyForm :: HasHeist b => Handler b Redbone ()
emptyForm = ifTop $ render "index"


------------------------------------------------------------------------------
-- | Get JSON metamodel.
metamodel :: Handler b Redbone ()
metamodel = ifTop $ do
  modelName <- getModelName
  serveFile ("resources/static/js/models/" ++ (toString modelName) ++ ".js")


------------------------------------------------------------------------------
-- | Extract model name from request path parameter.
getModelName:: MonadSnap m => m ByteString
getModelName = do
  modelName <- fromMaybe "" <$> getParam "model"
  return modelName


------------------------------------------------------------------------------
-- | CRUD routes for models.
routes :: HasHeist b => [(ByteString, Handler b Redbone ())]
routes = [ ("/:model/", method GET emptyForm)
         , ("/:model/model", method GET metamodel)
         ]


------------------------------------------------------------------------------
-- | Connect to Redis and set routes.
redboneInit :: HasHeist b => SnapletInit b Redbone
redboneInit = makeSnaplet "redbone" "Backbone.js backend with Redis storage" Nothing $
          do
            addRoutes routes
            redis <- liftIO $ connect "127.0.0.1" "6379"
            return $ Redbone redis