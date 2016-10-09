module Main where

import Data.List
import Data.Map (insertWith,unionWith,empty,toAscList,Map)
import Data.String.Utils
import Text.ParserCombinators.Parsec
import Text.Regex.PCRE ((=~))

-- A tweet with all fields stored in the archive's CVS file
data Tweet = Tweet
	{
		tweet_id :: String,
		in_reply_to_status_id :: String,
		in_reply_to_user_id :: String,
		timestamp :: String,
		source :: String,
		text :: String,
		retweeted_status_id :: String,
		retweeted_status_user_id :: String,
		retweeted_status_timestamp :: String,
		expanded_urls :: [String]
	} deriving (Show)

-- The result of an algorithm
data (Show a) => Result a = Value a
	| List [a]

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

statNumberTweets :: [Tweet] -> Int
statNumberTweets = length

statNumberRetweets :: [Tweet] -> Int
statNumberRetweets = length . filter retweet

statNumberPlainTweets :: [Tweet] -> Int
statNumberPlainTweets = length . filter (null . mentions) . filter (not . retweet)

statMeanNumberMentions :: [Tweet] -> Int
statMeanNumberMentions = mean . map (length . mentions) . filter (not . retweet)

statMeanNumberCharacters :: [Tweet] -> Int
statMeanNumberCharacters = mean . map (length . text) . filter (not . retweet)

statNumberCharacters :: [Tweet] -> Int
statNumberCharacters = sum . map (length . text) . filter (not . retweet)

statMostUsedSources :: [Tweet] -> [String]
statMostUsedSources = take 5 . mostOccurring . map source

statMostUsedCharacters :: [Tweet] -> [Char]
statMostUsedCharacters = take 5 . map fst . sortBy compareOccurrencesDesc . toAscList . (foldl (unionWith (+)) empty) . map (occurrencesRaw . text) . filter (not . retweet)

statMostRepliedTo :: [Tweet] -> [String]
statMostRepliedTo = take 5 . mostOccurring . filter (not . null) . map in_reply_to_user_id . filter (not . retweet)

statMostMentioned :: [Tweet] -> [String]
statMostMentioned = take 5 . mostOccurring . concat . filter (not . null) . map mentions . filter (not . retweet)

-- A list of stats listed with their description and the function of the algorithm
algorithms :: [(String,([Tweet] -> Result String))]
algorithms =
	[
		("tweet count",Value . show . statNumberTweets),
		("number of retweets",Value . show . statNumberRetweets),
		("number of plain tweets (no mentions or retweets)",Value . show . statNumberPlainTweets),
		("mean number of accounts mentioned",Value . show . statMeanNumberMentions),
		("mean character count",Value . show . statMeanNumberCharacters),
		("number of characters",Value . show . statNumberCharacters),
		("most used sources",List . statMostUsedSources),
		("most used characters",List . map show . statMostUsedCharacters),
		("most often replied to",List . statMostRepliedTo),
		("most often mentioned",List . statMostMentioned)
	]

-- Generates and prints stats
statistics :: [Tweet] -> [(String,Result String)]
statistics tweets = map (\(desc,foo) -> (desc,foo tweets)) algorithms

-- Prints a single stat taking care of Values and Lists
printSingleStat :: (String,Result String) -> IO ()
printSingleStat (desc,Value value) = putStrLn (desc++": "++value) >> putStrLn ""
printSingleStat (desc,List list) = putStrLn (desc++":") >> mapM_ (putStrLn . ("- "++)) list >> putStrLn ""

-- Prints all statistics
printStats :: [(String,Result String)] -> IO ()
printStats = mapM_ printSingleStat

-- Parses the archive and passes the resulting list of Tweets to the stat function
main :: IO ()
main = do
	text <- getContents
	case parse parseArchive "twitter archive" text of
		Left err -> putStrLn $ show err
		Right dat -> printStats $ statistics dat

