module Input
( readArchive
) where

import Tweet
import TwitterMonad

import Control.Monad.Except
import Data.String.Utils
import Text.ParserCombinators.Parsec

-- This skips the first line containing the headers to not interfere with datatypes
headers :: Parser ()
headers = do
	_ <- anyChar `manyTill` lookAhead newline
	return ()

-- Parses a single quoted character, which is either a regular character or "" which is being
-- replaced by a single such character
quotedChar :: Parser Char
quotedChar = do
	noneOf "\"" <|> try (string "\"\"" >> return '"')

-- A quoted field starts with " and ends with " and every " in between is escaped by preceding it
-- with "
quoted :: Parser String
quoted = do
	between (char '"') (char '"') $ many quotedChar

-- Parses a single line of the CSV file
tweet :: Parser Tweet
tweet = do
	fields <- quoted `sepBy1` char ','
	return $ Tweet
		(fields!!0)
		(fields!!1)
		(fields!!2)
		(fields!!3)
		(fields!!4)
		(fields!!5)
		(fields!!6)
		(fields!!7)
		(fields!!8)
		(split "," $ fields!!9)

-- Parses a complete archive by skipping the headers
parseArchive :: Parser [Tweet]
parseArchive = do
	-- ignore headers
	_ <- headers >> newline
	tweet `sepEndBy` ((newline >> return ()) <|> eof)

-- Converts a read CSV file to a list of Tweets, can fail
readArchive :: String -> TwitterMonad [Tweet]
readArchive input = case parse parseArchive "twitter archive" input of
		Left err -> throwError $ InputError err
		Right tweets -> return tweets

