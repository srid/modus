{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Plugin.TT
  ( module Common.Plugin.TT.TimeRange
  , Category
  , Item(..)
  , Data
  )
where

import Data.Aeson
import Data.List.NonEmpty
import Data.Text (Text)
import Data.Time.Calendar
import GHC.Generics

import Common.Plugin.TT.TimeRange

type Category = NonEmpty Text

data Item = Item
  { _item_timeRange :: TimeRange
  , _item_category :: Category
  }
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

type Data = [(Day, [Item])]
