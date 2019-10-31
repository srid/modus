{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Control.Monad.IO.Class
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Obelisk.Backend
import Obelisk.ExecutableConfig.Lookup

import Common.Route

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_routeEncoder = fullRouteEncoder
  , _backend_run = \serve -> do
      dataDir <- getBackendTextConfig "data-directory"
      liftIO $ print dataDir
      serve $ const $ return ()
  }
  where
    getBackendTextConfig name =
      fmap (T.strip . T.decodeUtf8) . Map.lookup ("backend/" <> name) <$> getConfigs
