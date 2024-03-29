{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend where

import Control.Monad.Fix
import Data.Aeson (FromJSON)
import Data.Either.Combinators (fromRight')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as T
import Path

import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import qualified Common.Plugin.TT as TT
import qualified Common.Plugin.Wiki as Wiki
import Common.Route

data Tab
  = Tab_Home
  | Tab_Wiki
  | Tab_TT
  deriving (Eq, Ord, Bounded, Enum)

tabTitle :: Tab -> Text
tabTitle = \case
    Tab_Home -> "Home"
    Tab_Wiki -> "Wiki"
    Tab_TT -> "TT"

tabRoute :: Tab -> R FrontendRoute
tabRoute = \case
  Tab_Home -> FrontendRoute_Main :/ ()
  Tab_Wiki -> FrontendRoute_Wiki :/ WikiRoute_Index :/ ()
  Tab_TT -> FrontendRoute_TT :/ IndexOnlyRoute :/ ()

withTabs
  :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m, RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m)
  => RoutedT t (R FrontendRoute) m ()
  -> RoutedT t (R FrontendRoute) m ()
withTabs w = do
  divClass "ui top attached tabular menu" $ do
    activeTab :: Dynamic t Tab <- subRoute $ \case
      FrontendRoute_Main -> pure Tab_Home
      FrontendRoute_Wiki -> pure Tab_Wiki
      FrontendRoute_TT -> pure Tab_TT
    forM_ [minBound .. maxBound] $ \(tab :: Tab) -> do
      let attr = ffor ((== tab) <$> activeTab) $ \case
            False -> ("class" =: "item")
            True -> ("class" =: "active item")
      elDynAttr "div" attr $ routeLink (tabRoute tab) $ text $ tabTitle tab
  divClass "ui bottom attached active tab segment" w

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      elAttr "base" ("href" =: "/") blank
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
      el "title" $ text "📔Modus Dev"
      elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: static @"semantic.min.css") blank
  , _frontend_body = divClass "ui container" $ do
      elClass "h1" "ui header" $ text "Modus dev"
      withTabs $ subRoute_ $ \case
        FrontendRoute_Main -> text "Hello"
        FrontendRoute_TT ->
          renderPlugin @TT.Data (ApiRoute_TT :/ IndexOnlyRoute :/ ()) $ \days -> do
            durations <- fmap concat $ divClass "ui striped table" $ do
              el "thead" $ el "tr" $ do
                el "th" $ text "Day"
                el "th" $ text "Start"
                el "th" $ text "End"
                el "th" $ text "Duration"
                el "th" $ text "Category"
              el "tbody" $ forM days $ \(day, items) -> do
                forM items $ \item@(TT.Item timeRange category) ->
                  el "tr" $ do
                    el "td" $ text $ show day
                    el "td" $ clockHand $ TT.timeRangeStart timeRange
                    el "td" $ clockHand $ TT.timeRangeEnd timeRange
                    el "td" $ clockHand $ TT.timeRangeDuration timeRange
                    el "td" $ forM_ category $ \cat ->
                      divClass "ui basic right pointing label" $ text cat
                    pure item
            let durationMap = flipfoldl' (\(TT.Item timeRange cat) -> Map.insertWith mappend cat [TT._timeRange_duration timeRange])
                              mempty durations
            -- TODO: Use gantt chart like layout
            divClass "ui striped table" $ do
              el "thead" $ el "tr" $ do
                el "th" $ text "Category"
                el "th" $ text "Total hours"
              el "tbody" $ forM_ (Map.toList durationMap) $ \(category, items) ->
                el "tr" $ do
                  el "td" $ forM_ category $ \cat ->
                    divClass "ui basic right pointing label" $ text cat
                  el "td" $ clockHand $ TT.unpackClockHand $ sum items
            divClass "ui message" $ do
              divClass "header" $ text "Total hours"
              el "p" $ clockHand $ TT.unpackClockHand $ sum $ TT._timeRange_duration . TT._item_timeRange <$> durations
        FrontendRoute_Wiki -> subRoute_ $ \case
          WikiRoute_Index ->
            renderPlugin @Wiki.Data (ApiRoute_Wiki :/ WikiRoute_Index :/ ()) $ \notes ->
              forM_ notes $ \f -> do
                let noteName = T.pack $ toFilePath f
                el "li" $ routeLink (FrontendRoute_Wiki :/ WikiRoute_Show :/ noteName) $
                  text noteName
          WikiRoute_Show -> do
            noteName' <- askRoute
            -- TODO: avoid the dyn_
            dyn_ $ ffor noteName' $ \noteName ->
              renderPlugin @(Either String Text) (ApiRoute_Wiki :/ WikiRoute_Show :/ noteName) $
                \case
                  Left err -> divClass "ui negative message" $ do
                    divClass "header" $ text "Unable to render source document"
                    el "p" $ text $ T.pack err
                  Right content -> prerender_ blank $
                    void $ elDynHtml' "div" $ constDyn content

      divClass "ui segment" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s
  }
  where
    clockHand :: DomBuilder t m => (Natural, Natural) -> m ()
    clockHand (h, m) = do
      text $ show h
      text "h"
      when (m > 0) $
        text $ show m

renderPlugin
  :: forall a t m js.
     (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js t m, FromJSON a)
  => R ApiRoute
  -> (a -> m ())
  -> m ()
renderPlugin r f = do
  resp <- getApi $ BackendRoute_Api :/ r
  widgetHold_ (text "Loading...") $ ffor resp $
    maybe (text "Error decoding plugin data") f

-- | Fetch data from the backend API
getApi
  :: forall a t m js.
     (MonadHold t m, PostBuild t m, Prerender js t m, FromJSON a)
  => R BackendRoute
  -- ^ The Api route to fetch
  -> m (Event t (Maybe a))
  -- ^ The `a` here should specify the type of the JSON decoded data that the
  -- given Api route returns.
getApi br = fmap switchDyn $ prerender (pure never) $ do
  pb <- getPostBuild
  getAndDecode $ renderBackendRoute enc br <$ pb
  where
    enc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
    enc = fromRight' $ checkEncoder fullRouteEncoder
