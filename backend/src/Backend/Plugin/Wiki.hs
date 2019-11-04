{-# LANGUAGE OverloadedStrings #-}

module Backend.Plugin.Wiki where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Path
import Path.IO


import Common.Plugin.Wiki

pluginExt :: Text
pluginExt = ".md"

-- | Load all .md files under ___ directory.
loadData :: Path Abs Dir -> IO Data
loadData dataDir = do
  (_, files) <- listDirRecurRel dataDir
  let mdFiles = filter ((== T.unpack pluginExt) . fileExtension) files
  pure $ filename <$> mdFiles

loadFile :: Path Abs File -> IO Text
loadFile f = T.readFile $ toFilePath f
