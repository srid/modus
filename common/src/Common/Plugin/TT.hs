{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Plugin.TT where

import Data.Aeson
import Data.List.NonEmpty
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.LocalTime
import GHC.Generics

type Category = NonEmpty Text

data Item = Item
  { _item_start :: TimeOfDay
  , _item_end :: TimeOfDay
  , _item_category :: Category
  }
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

type Data = [(Day, [Item])]
