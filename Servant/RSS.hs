{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module    : Servant.RSS
-- Copyright : (c) John Soo, 2020
-- License   : BSD3
-- Maintainer: John Soo <jsoo1@asu.edu>
--
-- Servant support for RSS feeds.
--
-- Provide your desired feed type `ToXml` instance from /xmlbf/.
-- Your type will be de/serialized automatically from a `Servant` API.
module Servant.RSS where

import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Network.HTTP.Media ((//))
import Servant.API (Accept (..), MimeRender (..), MimeUnrender (..))
import Xmlbf (FromXml (..), ToXml (..), encode, parse)
import Xmlbf.Xeno (fromRawXml)

-- | Content-Type /application\/rss+xml/. Use in Servant endpoints.
--
-- @
-- data MyRSSFeed = ...
--
-- instance ToXml MyRSSFeed where
--     ...
--
-- instance FromXml MyRSSFeed where
--     ...
--
-- type API = ...
--   :\<|\> "foo" :> Get '[RSS] MyRSSFeed
-- @
data RSS

instance Accept RSS where
  contentType = const ("application" // "rss+xml")

instance ToXml a => MimeRender RSS a where
  mimeRender _ = toLazyByteString . encode . toXml

instance FromXml a => MimeUnrender RSS a where
  mimeUnrender _ bs = fromRawXml (toStrict bs) >>= parse fromXml
