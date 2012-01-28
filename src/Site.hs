{-# LANGUAGE OverloadedStrings #-}

{-|

Build application from Redbone snaplet and utility handlers.

-}

module Site
  ( app
  ) where

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)

import Application
import qualified Redbone as R

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [
          ("resources/static", serveDirectory "resources/static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "Forms application" Nothing $ do
  r <- nestSnaplet "rb" redbone $ R.redboneInit
  h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
  sTime <- liftIO getCurrentTime
  addRoutes routes
  return $ App h r sTime
