{-# LANGUAGE OverloadedStrings #-}
module Common.Plugin.TT where

import Data.Text (Text)
import Data.Time.LocalTime

type Category = [Text] -- TODO: should be non empty list

data Item = Item
  { _item_start :: TimeOfDay
  , _item_end :: TimeOfDay
  , _item_category :: Category
  }
  deriving (Eq, Show)
