{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Monad
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Reflex.Dom.Core

import qualified Common.Plugin.TT as TT
import Common.Route

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      elAttr "base" ("href" =: "/") blank
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
      el "title" $ text "📔Modus Dev"
      elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: static @"semantic.min.css") blank
  , _frontend_body = divClass "ui container" $ do
      elClass "h1" "ui header" $ text "Modus dev"
      divClass "ui segment" $ do
        d <- getData
        widgetHold_ (text "Loading...") $ ffor d $ \case
          Nothing -> text "Error decoding data"
          Just days -> do
            divClass "ui striped table" $ do
              el "thead" $ el "tr" $ do
                el "th" $ text "Day"
                el "th" $ text "Start"
                el "th" $ text "End"
                el "th" $ text "Category"
              el "tbody" $ forM_ days $ \(day, items) -> do
                forM_ items $ \(TT.Item start end category) -> do
                  el "tr" $ do
                    el "td" $ text $ T.pack $ show day
                    el "td" $ text $ T.pack $ show start
                    el "td" $ text $ T.pack $ show end
                    forM_ category $ \cat ->
                      divClass "ui basic right pointing label" $ text cat
      divClass "ui segment" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s
  }

getData
  :: (MonadHold t m, PostBuild t m, Prerender js t m)
  => m (Event t (Maybe TT.Data))
getData = fmap switchDyn $ prerender (pure never) $ do
  pb <- getPostBuild
  getAndDecode $ renderBackendRoute enc (BackendRoute_GetData :/ ()) <$ pb
  where
    Right (enc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName) = checkEncoder fullRouteEncoder
