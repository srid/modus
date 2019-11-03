{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Plugin.Wiki where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Path

data Note = Note
  { _note_fileName :: Path Rel File
  , _note_content :: Text
  }
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

type Data = [Note]
