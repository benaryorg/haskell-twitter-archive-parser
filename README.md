[![Build Status](https://travis-ci.org/benaryorg/haskell-twitter-archive-parser.svg?branch=master)](https://travis-ci.org/benaryorg/haskell-twitter-archive-parser)

# What is this?

This is a program that takes your [Twitter](https://twitter.com) archive's
`tweets.csv` on stdin and then prints some stats about it.

It doesn't have much functionality and could use a little lazy parsing but is
fine otherwise.
Well, there are missing comments but this is just temporary.
It's also probably not best practice, that's why I'm going to rewrite it a
little.

Currently prints (might be incomplete):

- tweet count
- number of retweets
- number of plain tweets (no mentions or retweets)
- number of characters
- number of links
- mean number of accounts mentioned
- mean character count
- mean number of links
- most used sources
- most used characters
- most often replied to
- most often mentioned

# I want \<some stat\>!

Tell me! Open an issue!

# I want to add \<some stat\> myself!

Just send a PR.
This whole thing is licensed under MIT so you're probably fine using it.

# How do I actually use this?

Best is you install [stack](https://docs.haskellstack.org/en/stable/README/).

```bash
# if you are using stack
stack setup
stack build
stack exec twitter-archive-parser < ../tweets.csv

# if you use cabal (assuming you have appropriate dependencies)
cabal run < ../tweets.csv

# if you happen to have ghc (8.0), parsec, regex-pcre and containers installed
# you probably know how to compile and run this yourself
```

## Example Output

I stripped the list output to only two per stat.

```text
tweet count: 112594

number of retweets: 50606

number of plain tweets (no mentions or retweets): 21948

mean number of accounts mentioned: 0

mean character count: 54

number of characters: 3408388

most used sources:
- <a href="https://github.com/veskuh/Tweetian" rel="nofollow">Tweetian for Sailfish OS</a>
- <a href="http://hotot.org" rel="nofollow">Hotot</a>

most used characters:
- ' '
- 'e'

most often replied to:
- 170420365
- 1201898934

most often mentioned:
- @_xxyy
- @Nordwind25
```

## Getting Numbers

In case you want numbers instead of just the top 5 or something similar, at the
moment most output things use `mostOccurring`, just change that to
`sortedOccurrences` and you should get some nice numbers.

There will be an arguments to make the program print numbers by itself soon.

# Output Format

Using `-f` or `--format` you can set the output format to e.g. *plain* or
*json* which then prints in the choosen format.

# Version Information

Currently the *ChangeLog* and versioning are based on the functions that are
not directly stats but make up the API that is used by the statistics.
This will change with the *1.0.0.0* release.
From then on the versioning will be about the statistics and not about the
internal API, mainly because the API will remain frozen then, except for major
releases.

There might be a *2.0.0.0* release which transforms this entire program into a
library which can be used easier by third party programs and a small command
line program to call the library.

