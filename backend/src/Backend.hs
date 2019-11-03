{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map
import Path

import Obelisk.Backend
import Obelisk.ExecutableConfig.Lookup
import Obelisk.Route

import Snap

import Common.Route

import qualified Backend.Plugin.TT as TT
import qualified Backend.Plugin.Wiki as Wiki

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_routeEncoder = fullRouteEncoder
  , _backend_run = \serve -> do
      Just (Just dataDir) <- fmap parseAbsDir <$> getBackendConfig "data-directory"
      liftIO $ print dataDir
      -- items <- liftIO $ getAllData dataDir
      -- liftIO $ Shower.printer items
      serve $ \case
        BackendRoute_Missing :/ () ->
          writeLBS "404"
        BackendRoute_GetData :/ () -> do
          d <- liftIO $ getAllData dataDir
          writeLBS $ Aeson.encode d
  }
  where
    getAllData dataDir = (,)
      <$> TT.loadData dataDir
      <*> Wiki.loadData dataDir
    getBackendConfig name =
      fmap BSC.unpack . Map.lookup ("backend/" <> name) <$> getConfigs
