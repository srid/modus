{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Backend.Plugin.TT
  ( loadData
  ) where

import Control.Monad
import Data.Char (isPrint)
import Data.List (sort)
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Void (Void)
import Path
import Path.IO

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Common.Plugin.TT

type Parser = Parsec Void Text

pluginExt :: Text
pluginExt = ".tt"

-- | Load all .tt files under diary/ directory.
loadData :: Path Abs Dir -> IO Data
loadData dataDir = sort <$> do
  let diaryDir = dataDir </> [reldir|diary|]
  (_, files) <- listDirRecurRel diaryDir
  let ttFiles = filter ((== T.unpack pluginExt) . fileExtension) files
  forM ttFiles $ \f -> do
    xs <- loadFile (diaryDir </> f)
    let day = fromJust $ parseMaybe dayPathParser $ T.pack (toFilePath f)
    pure (day, xs)
  where
    dayPathParser :: Parser Day
    dayPathParser = fromGregorian
      <$> (L.decimal <* string "/")
      <*> (L.decimal <* string "/")
      <*> (L.decimal <* (string pluginExt <* eof))

-- | Load a .tt file
loadFile :: Path Abs File -> IO [Item]
loadFile fp = do
  content <- T.pack <$> readFile (toFilePath fp)
  case parse items (toFilePath fp) content of
    Left e -> fail $ errorBundlePretty e
    Right v -> pure v

timeOfDay :: Parser TimeOfDay
timeOfDay = do
  hh <- L.decimal
  _ <- string "h"
  mm <- optional . try $ L.decimal
  case makeTimeOfDayValid hh (fromMaybe 0 mm) 0 of
    Just v -> pure v
    Nothing -> fail $ "invalid time of day: " ++ show hh ++ "h" ++ maybe "" show  mm

timeRange :: Parser (TimeOfDay, TimeOfDay)
timeRange = (,) <$> (timeOfDay <* string "-") <*> timeOfDay

item :: Parser Item
item = do
  (start, end) <- timeRange
  _ <- space1
  cat <- fmap T.pack <$> sepBy1 (some $ printCharExcept '/') (char '/')
  pure $ Item start end $ NEL.fromList cat

-- | Like `printChar` but ignores the given character
printCharExcept :: (MonadParsec e s m, Token s ~ Char) => Char -> m (Token s)
printCharExcept ignoreChar = satisfy predicate <?> "printable character except /"
  where
    predicate c = c /= ignoreChar && isPrint c

items :: Parser [Item]
items = manyLines item

-- Like `many` but expects each item to be on its own line, while ignoring `sc`
-- in between them.
manyLines :: Parser a -> Parser [a]
manyLines p = sc *> many (L.lexeme sc p)

-- Parser for whitespace and comments
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "#")
  empty
