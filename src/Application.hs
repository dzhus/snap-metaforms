{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Build application from Redson snaplet and utility handlers.

-}

module Application (appInit)

where

import Prelude hiding (lookup)

import Control.Monad.IO.Class
import Data.Functor
import Data.Maybe

import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as BU (toString)
import Data.Configurator
import Data.Lens.Template
import Data.Time.Clock

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth hiding (session)
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe

import Web.ClientSession

import Snap.Snaplet.Redson


------------------------------------------------------------------------------
-- | Application snaplet state type: Redson, Heist.
data App = App
    { _heist :: Snaplet (Heist App)
    , _redson :: Snaplet (Redson App)
    , _session :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _startTime :: UTCTime
    }

type AppHandler = Handler App App

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
-- | Render empty form for model.
emptyForm :: AppHandler ()
emptyForm = ifTop $ render "index"


------------------------------------------------------------------------------
-- | Serve JSON metamodel.
metamodel :: AppHandler ()
metamodel = ifTop $ do
  modelName <- (BU.toString . fromMaybe "") <$> getParam "model"
  serveFile $ "resources/models/" ++ modelName ++ ".js"


------------------------------------------------------------------------------
-- | Redirect using 303 See Other to login form.
--
-- Used after unsuccessful access/login attempt or logout.
redirectToLogin :: MonadSnap m => m a
redirectToLogin = redirect' "/login/" 303


------------------------------------------------------------------------------
-- | If user is not logged in, redirect to login page, pass to
-- handler otherwise.
authOrLogin :: AppHandler () -> AppHandler ()
authOrLogin h = requireUser auth redirectToLogin h


------------------------------------------------------------------------------
-- | Render empty login form.
loginForm :: AppHandler ()
loginForm = do
  serveFile $ "resources/templates/login.html"


------------------------------------------------------------------------------
-- | Login user.
doLogin :: AppHandler ()
doLogin = ifTop $ do
  l <- fromMaybe "" <$> getParam "login"
  p <- fromMaybe "" <$> getParam "password"
  r <- maybe False (const True) <$> getParam "remember"
  res <- with auth $ loginByUsername l (ClearText p) r
  case res of
    Left err -> redirectToLogin
    Right user -> redirect "/_/scp/"


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/_/:model/", method GET $ authOrLogin emptyForm)
         , ("/_/:model/model/", authOrLogin $ method GET metamodel)
         , ("/login/", method GET loginForm)
         , ("/login/", method POST doLogin)
         , ("/logout/", with auth $ logout >> redirectToLogin)
         , ("/s/", serveDirectory "resources/static")
         ]


sessionTimeout :: Maybe Int
sessionTimeout = Nothing


------------------------------------------------------------------------------
-- | The application initializer.
appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Forms application" Nothing $ do
  r <- nestSnaplet "_" redson $ redsonInit auth

  h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
  addAuthSplices auth

  cfg <- getSnapletUserConfig
  sesKey <- liftIO $
            lookupDefault "resources/private/client_session_key.aes"
                          cfg "session-key"
  rmbKey <- liftIO $
            lookupDefault "resources/private/site_key.txt"
                          cfg "remember-key"
  rmbPer <- liftIO $
            lookupDefault 14 
                          cfg "remember-period"
  authDb <- liftIO $
            lookupDefault "resources/private/users.json"
                          cfg "user-db"

  s <- nestSnaplet "session" session $
       initCookieSessionManager sesKey "_session" sessionTimeout
  a <- nestSnaplet "auth" auth $
       initJsonFileAuthManager
       defAuthSettings{ asSiteKey = rmbKey
                      , asRememberPeriod = Just (rmbPer * 24 * 60 * 60)}
                               session authDb

  sTime <- liftIO getCurrentTime

  addRoutes routes

  return $ App h r s a sTime
