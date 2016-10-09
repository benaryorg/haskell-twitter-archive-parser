module Main where

import Text.ParserCombinators.Parsec
import Data.List
import Data.Map (insertWith,unionWith,empty,toAscList,Map)
import Text.Regex.PCRE ((=~))

-- A tweet consists of the following fields:
--  tweet_id, in_reply_to_status_id, in_reply_to_user_id, timestamp, source, text,
--  retweeted_status_id, retweeted_status_user_id, retweeted_status_timestamp, expanded_urls
data Tweet = Tweet
	{
		tweet_id :: String,
		in_reply_to_user_id :: String,
		source :: String,
		text :: String,
		retweeted_status_id :: String
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
	return $ Tweet
		(fields!!0)
		(fields!!2)
		(fields!!4)
		(fields!!5)
		(fields!!6)

-- Parses a complete archive by skipping the headers
parseArchive :: Parser [Tweet]
parseArchive = do
	-- ignore headers
	headers >> newline
	tweet `sepEndBy` ((newline >> return ()) <|> eof)

-- Returns a list of "@username"s that were mentioned in the tweet
mentions :: Tweet -> [String]
mentions = concat . ((flip foo) pattern) . text
	where
		pattern = "\\B@[_a-zA-Z0-9]+\\b"
		foo = (=~) :: (String -> String -> [[String]])

-- Determines if the tweet is a retweet
retweet :: Tweet -> Bool
retweet = not . null . retweeted_status_id

-- Calculates the mean of a list of Ints
mean :: [Int] -> Int
mean list = (sum list) `div` (length list)

-- Uses a Map to determine how often each element occurs in the list, returns a Map
occurrencesRaw :: Ord a => [a] -> Map a Int
occurrencesRaw = foldl (\map e -> insertWith (+) e 1 map) empty

-- Uses a Map to determine how often each element occurs in the list
occurrences :: Ord a => [a] -> [(a,Int)]
occurrences = toAscList . occurrencesRaw

-- Used for sorting the occurrences descending (highest count first)
compareOccurrencesDesc :: (a,Int) -> (a,Int) -> Ordering
compareOccurrencesDesc (_,x) (_,y) = compare y x

-- Returns the occuring elements sorted so that the highest ones are at the head
sortedOccurrences :: Ord a => [a] -> [(a,Int)]
sortedOccurrences = (sortBy compareOccurrencesDesc) . occurrences

-- Returns the occurrences sorted by descending count without the count
mostOccurring :: Ord a => [a] -> [a]
mostOccurring = (map fst) . sortedOccurrences

-- Generates and prints stats
stat :: [Tweet] -> IO ()
stat tweets = do
	putStr "tweet count: "
	putStrLn $ show $ length tweets
	putStr "number of retweets: "
	putStrLn $ show $ (length . filter (not . null . retweeted_status_id)) tweets
	putStr "number of plain tweets (no mentions or retweets): "
	putStrLn $ show $ (length . filter (null . retweeted_status_id) . filter (null . mentions)) tweets
	putStr "mean character count: "
	putStrLn $ show $ (mean . map (length . text)) tweets
	putStr "number of characters: "
	putStrLn $ show $ (sum . map (length . text)) tweets
	putStr "most used characters: "
	putStrLn $ show $ (take 5 . map fst . sortBy compareOccurrencesDesc . toAscList . (foldl (unionWith (+)) empty) . map (occurrencesRaw . text)) tweets
	putStr "most used sources: "
	putStrLn $ show $ (take 5 . mostOccurring . map source) tweets
	putStr "most often replied to: "
	putStrLn $ show $ (take 5 . mostOccurring . filter (not . null) . map in_reply_to_user_id) tweets
	putStr "most often mentioned: "
	putStrLn $ show $ (take 5 . mostOccurring . concat . filter (not . null) . map mentions . filter (null . retweeted_status_id)) tweets

-- Parses the archive and passes the resulting list of Tweets to the stat function
main :: IO ()
main = do
	text <- getContents
	case parse parseArchive "twitter archive" text of
		Left err -> putStrLn $ show err
		Right dat -> stat dat

