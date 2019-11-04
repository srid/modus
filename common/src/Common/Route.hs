{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Functor.Identity
import Data.Text (Text)

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute (R ApiRoute)

data ApiRoute :: * -> * where
  ApiRoute_TT :: ApiRoute (R IndexOnlyRoute)
  ApiRoute_Wiki :: ApiRoute (R WikiRoute)

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_TT :: FrontendRoute (R IndexOnlyRoute)
  FrontendRoute_Wiki :: FrontendRoute (R WikiRoute)

-- FIXME: This should probably have one constructor dealing with any plugin.
data WikiRoute :: * -> * where
  WikiRoute_Index :: WikiRoute ()
  WikiRoute_Show :: WikiRoute Text

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Api -> PathSegment "api" $ pathComponentEncoder $ \case
        ApiRoute_TT -> PathSegment "tt" indexOnlyRouteEncoder
        ApiRoute_Wiki -> PathSegment "wiki" wikiRouteEncoder
  )
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      FrontendRoute_TT -> PathSegment "tt" indexOnlyRouteEncoder
      FrontendRoute_Wiki -> PathSegment "wiki" wikiRouteEncoder
  )
  where
    wikiRouteEncoder :: Encoder (Either Text) (Either Text) (R WikiRoute) PageName
    wikiRouteEncoder = pathComponentEncoder $ \case
      WikiRoute_Index -> PathEnd $ unitEncoder mempty
      WikiRoute_Show -> PathSegment "show" singlePathSegmentEncoder

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''ApiRoute
  , ''WikiRoute
  ]
