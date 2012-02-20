{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|

Build application from Redson snaplet and utility handlers.

-}

module Application (appInit)

where

import Control.Monad.IO.Class
import Data.Functor
import Data.Maybe

import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as BU (toString)
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
    , _redson :: Snaplet Redson
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
  serveFile $ "resources/static/js/models/" ++ modelName ++ ".js"


------------------------------------------------------------------------------
-- | Redirect using 303 See Other to login form.
--
-- Used after unsuccessful access/login attempt or logout.
redirectToLogin :: MonadSnap m => m a
redirectToLogin = redirect' "login" 303


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
    Right user -> redirect "/rs/scp"


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("rs/:model/", method GET emptyForm)
         , ("rs/:model/model", method GET metamodel)
         , ("login", method GET loginForm)
         , ("login", method POST doLogin)
         , ("logout", with auth $ logout >> redirectToLogin)
         , ("resources/static", serveDirectory "resources/static")
         ]


-- | Path to AES key file
keyPath :: FilePath
keyPath = "resources/private/" ++ defaultKeyFile

userDB :: FilePath
userDB = "resources/private/users.json"

sessionTimeout :: Maybe Int
sessionTimeout = Nothing


------------------------------------------------------------------------------
-- | The application initializer.
appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Forms application" Nothing $ do
  r <- nestSnaplet "rs" redson $ redsonInit

  h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
  addAuthSplices auth

  s <- nestSnaplet "session" session $
       initCookieSessionManager keyPath "_session" sessionTimeout
  a <- nestSnaplet "auth" auth $
       initJsonFileAuthManager defAuthSettings session userDB

  sTime <- liftIO getCurrentTime

  addRoutes routes

  return $ App h r s a sTime
