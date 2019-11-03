{-# LANGUAGE OverloadedStrings #-}

module Backend.Plugin.Wiki where

import Control.Monad
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
  forM mdFiles $ \f -> do
    -- TODO: We shouldn't really be reading all files at once.
    -- Design routes to access notes by URL.
    note <- loadFile $ dataDir </> f
    pure $ Note (filename f) note

loadFile :: Path Abs File -> IO Text
loadFile f = T.readFile $ toFilePath f
