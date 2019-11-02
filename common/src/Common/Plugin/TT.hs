{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Plugin.TT where

import Data.Aeson
import Data.Text (Text)
import Data.Time.LocalTime
import GHC.Generics

type Category = [Text] -- TODO: should be non empty list

data Item = Item
  { _item_start :: TimeOfDay
  , _item_end :: TimeOfDay
  , _item_category :: Category
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
