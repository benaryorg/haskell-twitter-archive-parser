module Main where

import Text.ParserCombinators.Parsec
import Data.List
import Data.Map (insertWith,empty,toAscList)

-- "tweet_id","in_reply_to_status_id","in_reply_to_user_id","timestamp","source","text","retweeted_status_id","retweeted_status_user_id","retweeted_status_timestamp","expanded_urls"
data Tweet = Tweet
	{
		tweet_id :: String,
		text :: String
	} deriving (Show)

parseHeaders :: Parser ()
parseHeaders = do
	anyChar `manyTill` lookAhead newline
	return ()

parseQuotedChar :: Parser Char
parseQuotedChar = do
	noneOf "\"" <|> try (string "\"\"" >> return '"')

parseQuoted :: Parser String
parseQuoted = do
	between (char '"') (char '"') $ many parseQuotedChar

parseCSVLine :: Parser Tweet
parseCSVLine = do
	fields <- parseQuoted `sepBy1` char ','
	-- only return text for now
	return $ Tweet (fields!!0) (fields!!5)

parseArchive :: Parser [Tweet]
parseArchive = do
	-- ignore headers
	parseHeaders
	newline
	parseCSVLine `sepEndBy` ((newline >> return ()) <|> eof)

mean :: [Int] -> Int
mean list = (sum list) `div` (length list)

charCount :: String -> [(Char,Int)]
charCount = toAscList . foldl (\map e -> insertWith (+) e 1 map) empty

stat :: [Tweet] -> IO ()
stat tweets = do
	putStr "mean character count: "
	putStrLn $ show $ mean $ map (length . text) tweets
	putStr "number of characters: "
	putStrLn $ show $ length $ concatMap text tweets
	putStr "most used characters: "
	putStrLn $ show $ take 5 $ map (\(e,_) -> e) $ sortBy (\(_,n1) (_,n2) -> n2 `compare` n1) $ charCount $ concatMap text tweets

main :: IO ()
main = do
	text <- getContents
	case parse parseArchive "twitter archive" text of
		Left err -> putStrLn $ show err
		Right dat -> stat dat

