module Main where

import Arguments
import Input
import Tweet
import TwitterMonad

import Data.List
import Data.Map (insertWith,unionWith,empty,toAscList,Map)
import Data.String.Utils
import Data.Tuple.Utils
import Text.Printf
import Text.Regex.PCRE ((=~))

-- The result of an algorithm
data (Show a) => Result a = Value a
	| NumberStat (Integer,Double,Integer)
	| List [a]

-- Returns a list of "@username"s that were mentioned in the tweet
mentions :: Tweet -> [String]
mentions = concat . ((flip foo) pattern) . text
	where
		pattern = "\\B@[_a-zA-Z0-9]+\\b"
		foo = (=~) :: (String -> String -> [[String]])

-- Determines if the tweet is a retweet
retweet :: Tweet -> Bool
retweet = not . null . retweeted_status_id

-- Applies all functions to the given argument and returns the results
applyAll :: [a -> b] -> a -> [b]
applyAll foos x = map ($ x) foos

-- Calculates the mean of a list of Integrals
mean :: Integral a => [a] -> a
mean list = (sum list) `div` fromIntegral (length list)

-- Calculates the mean of a list of Fractionals
fmean :: Fractional a => [a] -> a
fmean list = (sum list) / fromIntegral (length list)

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

numberStat :: (Integral a,Ord b,Fractional b) => [a] -> Result String
numberStat list = let [a,b,c] = [minimum,fmean,maximum] `applyAll` map fromIntegral list in NumberStat (round a,b,round c)

statNumberTweets :: [Tweet] -> Int
statNumberTweets = length

statNumberRetweets :: [Tweet] -> Int
statNumberRetweets = length . filter retweet

statNumberPlainTweets :: [Tweet] -> Int
statNumberPlainTweets = length . filter (null . mentions) . filter (not . retweet)

statNumberLinks :: [Tweet] -> Int
statNumberLinks = sum . map (length . expanded_urls) . filter (not . retweet)

statMeanNumberCharacters :: [Tweet] -> [Int]
statMeanNumberCharacters = map (length . text) . filter (not . retweet)

statMeanNumberMentions :: [Tweet] -> [Int]
statMeanNumberMentions = map (fromIntegral . length . mentions) . filter (not . retweet)

statMeanNumberLinks :: [Tweet] -> [Int]
statMeanNumberLinks = map (fromIntegral . length . expanded_urls) . filter (not . retweet)

statMeanNumberWords :: [Tweet] -> [Int]
statMeanNumberWords = map (fromIntegral . length . words . text) . filter (not . retweet)

statMeanWordLength :: [Tweet] -> [Int]
statMeanWordLength = concat . map (map (fromIntegral . length) . words . text) . filter (not . retweet)

statMostUsedSources :: [Tweet] -> [String]
statMostUsedSources = take 5 . mostOccurring . map source

statMostUsedCharacters :: [Tweet] -> [Char]
statMostUsedCharacters = take 5 . map fst . sortBy compareOccurrencesDesc . toAscList . (foldl (unionWith (+)) empty) . map (occurrencesRaw . text) . filter (not . retweet)

statMostUsedWords :: [Tweet] -> [String]
statMostUsedWords = take 5 . map fst . sortBy compareOccurrencesDesc . toAscList . (foldl (unionWith (+)) empty) . map (occurrencesRaw . words . text) . filter (not . retweet)

statMostRepliedTo :: [Tweet] -> [String]
statMostRepliedTo = take 5 . mostOccurring . filter (not . null) . map in_reply_to_user_id . filter (not . retweet)

statMostMentioned :: [Tweet] -> [String]
statMostMentioned = take 5 . mostOccurring . concat . filter (not . null) . map mentions . filter (not . retweet)

-- A list of stats listed with their name, description and the function of the algorithm
algorithms :: [(String,String,([Tweet] -> Result String))]
algorithms =
	[
		("tweets","tweet count",Value . show . statNumberTweets),
		("retweets","number of retweets",Value . show . statNumberRetweets),
		("plaintweets","number of plain tweets (no mentions or retweets)",Value . show . statNumberPlainTweets),
		("links","number of links",Value . show . statNumberLinks),
		("meancharacters","mean number of characters",numberStat . statMeanNumberCharacters),
		("meanmentions","mean number of accounts mentioned",numberStat . statMeanNumberMentions),
		("meanlinks","mean number of links",numberStat . statMeanNumberLinks),
		("meanwords","mean number of words",numberStat . statMeanNumberWords),
		("meanwordlength","mean word length",numberStat . statMeanWordLength),
		("mostsources","most used sources",List . statMostUsedSources),
		("mostcharacters","most used characters",List . map show . statMostUsedCharacters),
		("mostwords","most used words",List . statMostUsedWords),
		("mostreplies","most often replied to",List . statMostRepliedTo),
		("mostmentions","most often mentioned",List . statMostMentioned)
	]

-- Generates and prints stats
statistics :: [String] -> [Tweet] -> [(String,String,Result String)]
statistics statlist tweets = map (\(name,desc,foo) -> (name,desc,foo tweets)) (filter ((flip elem) statlist . fst3) algorithms)

-- Prints a single stat taking care of Values and Lists
printSingleStat :: String -> (String,String,Result String) -> String
printSingleStat "plain" (name,desc,Value value) = desc++": "++value
printSingleStat "plain" (name,desc,List list) = desc++":"++concatMap ("\n- "++) list
printSingleStat "plain" (name,desc,NumberStat (x,y,z)) = printf "%s:\n- min: %d\n- avg: %.3f\n- max: %d" desc x y z
printSingleStat "json" (name,desc,Value value) = printf "\"%s\":%s" name (show value)
printSingleStat "json" (name,desc,List list) = printf "\"%s\":[%s]" name $ join "," $ map show list
printSingleStat "json" (name,desc,NumberStat (x,y,z)) = printf "\"%s\":{\"min\":%d,\"avg\":%f,\"max\":%d}" name x y z

-- Prints all statistics
printStats :: String -> [(String,String,Result String)] -> String
printStats "plain" = join "\n\n" . map (printSingleStat "plain")
printStats "json" = printf "{%s}" . join "," . map (printSingleStat "json")

execProgram :: Arguments -> IO String
execProgram (Arguments format statlist True) = return $ "available algorithms:\n"++(join "\n" $ map fst3 algorithms)
execProgram (Arguments format [] printList) = execProgram $ Arguments format (map fst3 algorithms) printList
execProgram (Arguments format statlist list) = do
	text <- getContents
	return $ case readArchive text of
		Right dat -> printStats format $ statistics statlist dat
		Left err -> case err of
			InputError err -> "InputError: "++show err

-- Parses the archive and passes the resulting list of Tweets to the stat function
main :: IO ()
main = do
	argument <- arguments
	output <- execProgram argument
	putStrLn output

