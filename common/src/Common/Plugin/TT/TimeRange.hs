{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Common.Plugin.TT.TimeRange
  ( TimeRange
  , InvalidTimeRange(..)
  , mkTimeRange
  )
where

import Control.Monad.Catch
import Data.Aeson
import GHC.Generics
import Numeric.Natural

data TimeRange = MkTimeRange
  { _timeRange_start :: Natural
  , _timeRange_duration :: Natural
  }
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data InvalidTimeRange
  = InvalidHour
  | InvalidMin
  | InvalidOrder
  deriving Show

instance Exception InvalidTimeRange

mkTimeRange :: forall m. MonadThrow m => (Natural, Natural) -> (Natural, Natural) -> m TimeRange
mkTimeRange start end = do
  x <- clockMins start
  y <- clockMins end
  if y > x
    then pure $ MkTimeRange x (y-x)
    else throwM InvalidOrder
  where
    validateHour = validateLessThan 24 InvalidHour
    validateMin = validateLessThan 60 InvalidMin
    validateLessThan :: Natural -> InvalidTimeRange -> Natural -> m Natural
    validateLessThan bound e n = if 0 <= n && n < bound
      then pure n
      else throwM e
    clockMins at = do
      h <- validateHour $ fst at
      m <- validateMin $ snd at
      pure $ h * 60 + m
