module Arguments
( Arguments (..)
, arguments
) where

import Data.String.Utils
import Options.Applicative

-- Command line arguments
data Arguments = Arguments
	{
		argPrintFormat :: String,
		argStatFilter :: [String],
		argStatList :: Bool
	}

-- Description of how Arguments have to be parsed
argumentList :: Parser Arguments
argumentList = Arguments
	<$>	strOption ( help "output format"
			<> long "format"
			<> short 'f'
			<> metavar "FORMAT"
			<> value "plain"
		)
	<*> many
		(
			strOption ( help "print this stat; use multiple times for multiple stats" 
				<> long "stat"
				<> short 's'
				<> metavar "STAT"
			)
		)
	<*> flag False True ( help "list available stats"
			<> long "list-stats"
			<> short 'l'
		)

-- Parses and returns the command line arguments
arguments :: IO Arguments
arguments = execParser $ info (helper <*> argumentList)
		( fullDesc
		<> progDesc "Parses a Twitter Archive's CSV on stdin"
		<> header "htap - Haskell Twitter Archive Parser" )

