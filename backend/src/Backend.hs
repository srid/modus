{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Control.Monad.IO.Class
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Shower

import Obelisk.Backend
import Obelisk.ExecutableConfig.Lookup

import Common.Route

import qualified Backend.TTParser as P

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_routeEncoder = fullRouteEncoder
  , _backend_run = \serve -> do
      Just dataDir <- getBackendTextConfig "data-directory"
      liftIO $ print dataDir
      items <- liftIO $ P.parseFile $ T.unpack dataDir ++ "/diary/2019/10/31.tt"
      liftIO $ Shower.printer items
      serve $ const $ return ()
  }
  where
    getBackendTextConfig name =
      fmap (T.strip . T.decodeUtf8) . Map.lookup ("backend/" <> name) <$> getConfigs
