{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Control.Monad.IO.Class
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Functor.Identity (Identity (..))
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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
      Just dataDir <- getBackendTextConfig "data-directory"
      liftIO $ print dataDir
      items <- liftIO $ getAllData dataDir
      liftIO $ Shower.printer items
      serve $ \case
        BackendRoute_Missing :=> Identity () ->
          writeText "404"
        BackendRoute_GetData :=> Identity () -> do
          d <- liftIO $ getAllData dataDir
          -- TODO: Return JSON
          writeText $ T.pack $ Shower.shower d
  }
  where
    getAllData dataDir =
      TT.loadFile $ T.unpack dataDir ++ "/diary/2019/10/31.tt"
    getBackendTextConfig name =
      fmap (T.strip . T.decodeUtf8) . Map.lookup ("backend/" <> name) <$> getConfigs
