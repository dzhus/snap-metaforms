{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Build application from Redson snaplet and utility handlers.

-}

module Application (appInit)

where

import Control.Monad.Trans

import Data.ByteString (ByteString)
import Data.Lens.Template
import Data.Time.Clock

import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Util.FileServe

import Snap.Snaplet.Redson

------------------------------------------------------------------------------
-- | Application snaplet state type: Redson, Heist.
data App = App
    { _heist :: Snaplet (Heist App)
    , _redson :: Snaplet Redson
    , _startTime :: UTCTime
    }

type AppHandler = Handler App App

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [
          ("resources/static", serveDirectory "resources/static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Forms application" Nothing $ do
  r <- nestSnaplet "rb" redson $ redsonInit
  h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
  sTime <- liftIO getCurrentTime
  addRoutes routes
  return $ App h r sTime
