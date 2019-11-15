{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Common.Plugin.TT.TimeRange
  ( TimeRange(_timeRange_start, _timeRange_duration)
  , InvalidTimeRange(..)
  , timeRangeStart
  , timeRangeEnd
  , timeRangeDuration
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

timeRangeStart :: TimeRange -> (Natural, Natural)
timeRangeStart (MkTimeRange x _) = unpackClockHand x

timeRangeEnd :: TimeRange -> (Natural, Natural)
timeRangeEnd (MkTimeRange start duration) = unpackClockHand $ start + duration

timeRangeDuration :: TimeRange -> (Natural, Natural)
timeRangeDuration (MkTimeRange _ duration) = unpackClockHand duration

unpackClockHand :: Natural -> (Natural, Natural)
unpackClockHand mins = (mins `div` 60, mins `mod` 60)

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
