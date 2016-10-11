module Tweet
( Tweet (..)
) where

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

