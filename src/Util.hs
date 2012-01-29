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
-- | Short-circuit MonadSnap flow with 404
notFound :: MonadSnap m => m ()
notFound = do
  modifyResponse $ setResponseCode 404
  r <- getResponse
  finishWith r

------------------------------------------------------------------------------
-- | Transform list ["k1", "v1", .. "kn", "vn"] to
-- [("k1", "v1"), .. ("kn", "vn")].
--
-- We use this to process Redis hgetall reply.
fromPairs :: BS s => [s] -> [(s, s)]
fromPairs l = 
    fromPairs' l []
        where
          fromPairs' [] d = d
          fromPairs' (k:(v:t)) d = fromPairs' t (d ++ [(k, v)])
