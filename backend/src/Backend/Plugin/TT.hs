{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Backend.Plugin.TT
  ( loadFile
  ) where

import Data.Char (isPrint)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Common.Plugin.TT

type Parser = Parsec Void Text

-- TODO: Use typed paths?
loadFile :: FilePath -> IO [Item]
loadFile fp = do
  content <- T.pack <$> readFile fp
  case parse items fp content of
    Left e -> fail $ errorBundlePretty e
    Right v -> pure v

timeOfDay :: Parser TimeOfDay
timeOfDay = do
  hh <- some numberChar
  _ <- string "h"
  mm <- optional . try $ some numberChar
  case makeTimeOfDayValid (read hh) (maybe 0 read mm) 0 of
    Just v -> pure v
    Nothing -> fail $ "invalid time of day: " ++ hh ++ "h" ++ fromMaybe "" mm

timeRange :: Parser (TimeOfDay, TimeOfDay)
timeRange = (,) <$> (timeOfDay <* string "-") <*> timeOfDay

item :: Parser Item
item = do
  (start, end) <- timeRange
  _ <- space1
  cat <- fmap T.pack <$> sepBy1 (some $ printCharExcept '/') (char '/')
  pure $ Item start end cat

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
