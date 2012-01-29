{-# LANGUAGE OverloadedStrings #-}

module Util where

import Control.Applicative

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import Data.Maybe

import Database.Redis.ByteStringClass

import Snap.Core

------------------------------------------------------------------------------
-- | Get parameter value from Request or return empty string
fromParam :: MonadSnap m => ByteString -> m String
fromParam p = do
  par <- fromMaybe "" <$> getParam p
  return $ toString par


------------------------------------------------------------------------------
-- | Short-circuit MonadSnap flow with 404 Not found
notFound :: MonadSnap m => m ()
notFound = do
  modifyResponse $ setResponseCode 404
  r <- getResponse
  finishWith r


------------------------------------------------------------------------------
-- | Short-circuit MonadSnap flow with 500 Server error
serverError :: MonadSnap m => m ()
serverError = do
  modifyResponse $ setResponseCode 500
  r <- getResponse
  finishWith r


------------------------------------------------------------------------------
-- | Transform list ["k1", "v1", .. "kn", "vn"] to
-- [("k1", "v1"), .. ("kn", "vn")].
--
-- We use this to process Redis hgetall reply.
toPairs :: BS s => [s] -> [(s, s)]
toPairs l = 
    toPairs' l []
        where
          toPairs' [] d = d
          toPairs' (k:(v:t)) d = toPairs' t (d ++ [(k, v)])
