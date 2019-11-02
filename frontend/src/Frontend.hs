{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

import qualified Common.Plugin.TT as TT
import Common.Route

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "📔Modus Dev"
  , _frontend_body = do
      el "h1" $ text "Modus dev"
      el "div" $ do
        d <- getData
        widgetHold_ (text "Loading...") $ ffor d $ \case
          Nothing -> text "Error decoding data"
          Just items -> text $ T.pack $ show items
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s
  }

getData
  :: (MonadHold t m, PostBuild t m, Prerender js t m)
  => m (Event t (Maybe [TT.Item]))
getData = fmap switchDyn $ prerender (pure never) $ do
  pb <- getPostBuild
  getAndDecode $ renderBackendRoute enc (BackendRoute_GetData :/ ()) <$ pb
  where
    Right (enc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName) = checkEncoder fullRouteEncoder
