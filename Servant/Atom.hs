{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module    : Servant.Atom
-- Copyright : (c) John Soo, 2020
-- License   : BSD3
-- Maintainer: John Soo <jsoo1@asu.edu>
--
-- Servant support for Atom feeds.
--
-- Provide your desired feed type `ToXml` instance from /xmlbf/.
-- Your type will be de/serialized automatically from a `Servant` API.
module Servant.Atom where

import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Network.HTTP.Media ((//))
import Servant.API (Accept (..), MimeRender (..), MimeUnrender (..))
import Xmlbf (FromXml (..), ToXml (..), encode, runParser)
import Xmlbf.Xeno (fromRawXml)

-- | Content-Type /application\/atom+xml/. Use in Servant endpoints.
--
-- @
-- data MyAtomFeed = ...
--
-- instance ToXml MyAtomFeed where
--     ...
--
-- instance FromXml MyAtomFeed where
--     ...
--
-- type API = ...
--     :\<|\> "foo" :> Get '[Atom] MyAtomFeed
-- @
data Atom

instance Accept Atom where
  contentType = const ("application" // "atom+xml")

instance ToXml a => MimeRender Atom a where
  mimeRender _ = toLazyByteString . encode . toXml

instance FromXml a => MimeUnrender Atom a where
  mimeUnrender _ bs = fromRawXml (toStrict bs) >>= runParser fromXml
