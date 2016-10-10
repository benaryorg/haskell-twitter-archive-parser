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
- mean number of accounts mentioned
- mean character count
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

As mentioned below, the output format might change soon, this will include a
format that gives you the numbers right away.

# Output Format

The output format is easily extensible and is currently focused on readability
but could be rewritten to be e.g. JSON.
This is currently planned for a later release which might also introduce
command line arguments for tweaking the output.

# Version Information

This is actually pretty easy, `v0.3.0.0` marks the beginning of easy
extensibility by having the `mostOccurring` function resulting in clean code.
From then on there will be a lot of `v0.3.x.0` releases for additional
information and `v0.3.0.x` releases for cleanups.

