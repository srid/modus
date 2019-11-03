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
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Functor.Identity (Identity (..))
import qualified Data.Map as Map
import Path

import Obelisk.Backend
import Obelisk.ExecutableConfig.Lookup

import qualified Shower
import Snap

import Common.Route

import qualified Backend.Plugin.TT as TT

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_routeEncoder = fullRouteEncoder
  , _backend_run = \serve -> do
      Just (Just dataDir) <- fmap parseAbsDir <$> getBackendConfig "data-directory"
      liftIO $ print dataDir
      items <- liftIO $ getAllData dataDir
      liftIO $ Shower.printer items
      serve $ \case
        BackendRoute_Missing :=> Identity () ->
          writeLBS "404"
        BackendRoute_GetData :=> Identity () -> do
          d <- liftIO $ getAllData dataDir
          writeLBS $ Aeson.encode d
  }
  where
    getAllData dataDir =
      TT.loadFile $ dataDir </> [relfile|diary/2019/10/31.tt|]
    getBackendConfig name =
      fmap BSC.unpack . Map.lookup ("backend/" <> name) <$> getConfigs
