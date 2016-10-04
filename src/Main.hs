module Main where

import Text.ParserCombinators.Parsec
import Data.List
import Data.Map (insertWith,empty,toAscList)

-- "tweet_id","in_reply_to_status_id","in_reply_to_user_id","timestamp","source","text","retweeted_status_id","retweeted_status_user_id","retweeted_status_timestamp","expanded_urls"
data Tweet = Tweet String

instance Show Tweet where
	show (Tweet text) = text

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
	return $ Tweet $ fields!!5
	--return $ Tweet $ unwords fields

parseArchive :: Parser [Tweet]
parseArchive = do
	-- ignore headers
	parseHeaders
	newline
	parseCSVLine `sepEndBy` ((newline >> return ()) <|> eof)

mean :: [Int] -> Int
mean list = (sum list) `div` (length list)

-- didn't know about maps then
{-
charCount :: String -> [(Char,Int)]
charCount = foldl
		(\list e -> case lookup e list of
			Nothing -> (e,1):list
			Just num -> (e,num+1):(filter (\(i,_) -> i /= e) list)
		)
		[]
-}

charCount :: String -> [(Char,Int)]
charCount = toAscList . foldl (\map e -> insertWith (+) e 1 map) empty

stat :: [Tweet] -> IO ()
stat tweets = do
	putStr "mean character count: "
	putStrLn $ show $ mean $ map (\(Tweet t) -> length t) tweets
	putStr "number of characters: "
	putStrLn $ show $ length $ concatMap (\(Tweet t) ->  t) tweets
	putStr "most used characters: "
	putStrLn $ show $ take 5 $ map (\(e,_) -> e) $ sortBy (\(_,n1) (_,n2) -> n2 `compare` n1) $ charCount $ concatMap (\(Tweet t) ->  t) tweets

main :: IO ()
main = do
	text <- getContents
	case parse parseArchive "twitter archive" text of
		Left err -> putStrLn $ show err
		Right dat -> stat dat

