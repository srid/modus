{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Backend
  ( backend
  ) where

import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Path

import Obelisk.Backend
import Obelisk.ExecutableConfig.Lookup
import Obelisk.Route

import qualified Rib.Pandoc as Pandoc
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
      serve $ \case
        BackendRoute_Missing :/ () ->
          writeLBS "404"
        BackendRoute_Api :/ r ->
          writeLBS =<< liftIO (loadPluginData dataDir r)
  }
  where
    getBackendConfig name =
      fmap BSC.unpack . Map.lookup ("backend/" <> name) <$> getConfigs

    loadPluginData :: Path Abs Dir -> R ApiRoute -> IO BSL.ByteString
    loadPluginData dataDir = \case
      ApiRoute_TT :/ IndexOnlyRoute :/ () -> Aeson.encode <$>
        TT.loadData dataDir
      ApiRoute_Wiki :/ wr -> case wr of
        WikiRoute_Index :/ () -> Aeson.encode <$>
          Wiki.loadData dataDir
        WikiRoute_Show :/ noteName' -> Aeson.encode <$> do
          let noteName = fromJust $ parseRelFile $ T.unpack noteName'
              noteFile = dataDir </> [reldir|wiki|] </> noteName
          markdownToHtml <$> Wiki.loadFile noteFile

    -- Convert markdown text to HTML text.
    markdownToHtml :: Text -> Either String Text
    markdownToHtml = bimap show id
                     . Pandoc.render'
                     . Pandoc.parsePure Pandoc.readMarkdown
