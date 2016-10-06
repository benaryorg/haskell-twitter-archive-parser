module Main where

import Text.ParserCombinators.Parsec
import Data.List
import Data.Map (insertWith,empty,toAscList)

-- A tweet consists of the following fields:
--  tweet_id, in_reply_to_status_id, in_reply_to_user_id, timestamp, source, text,
--  retweeted_status_id, retweeted_status_user_id, retweeted_status_timestamp, expanded_urls
data Tweet = Tweet
	{
		tweet_id :: String,
		in_reply_to_user_id :: String,
		source :: String,
		text :: String
	} deriving (Show)

-- This skips the first line containing the headers to not interfere with datatypes
headers :: Parser ()
headers = do
	anyChar `manyTill` lookAhead newline
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
	return $ Tweet (fields!!0) (fields!!2) (fields!!4) (fields!!5)

-- Parses a complete archive by skipping the headers
parseArchive :: Parser [Tweet]
parseArchive = do
	-- ignore headers
	headers >> newline
	tweet `sepEndBy` ((newline >> return ()) <|> eof)

-- Calculates the mean of a list of Ints
mean :: [Int] -> Int
mean list = (sum list) `div` (length list)

-- Uses a Map to determine how often each element occurs in the list
occurences :: Ord a => [a] -> [(a,Int)]
occurences = toAscList . foldl (\map e -> insertWith (+) e 1 map) empty

-- Returns the occuring elements sorted so that the highest ones are at the head
mostOccurring :: Ord a => [a] -> [a]
mostOccurring = (map fst) . (sortBy (\(_,n1) (_,n2) -> n2 `compare` n1)) . occurences

-- Generates and prints stats
stat :: [Tweet] -> IO ()
stat tweets = do
	putStr "mean character count: "
	putStrLn $ show $ mean $ map (length . text) tweets
	putStr "number of characters: "
	putStrLn $ show $ length $ concatMap text tweets
	putStr "most used characters: "
	putStrLn $ show $ take 5 $ mostOccurring $ concatMap text tweets
	putStr "most used sources: "
	putStrLn $ show $ take 5 $ mostOccurring $ map source tweets

-- Parses the archive and passes the resulting list of Tweets to the stat function
main :: IO ()
main = do
	text <- getContents
	case parse parseArchive "twitter archive" text of
		Left err -> putStrLn $ show err
		Right dat -> stat dat

