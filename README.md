# What is this?

This is a program that takes your [Twitter](twitter.com) archive's `tweets.csv`
on stdin and then prints some stats about it.

It doesn't have much functionality and could use a little lazy parsing but is
fine otherwise.
Well, there are missing comments but this is just temporary.
It's also probably not best practice, that's why I'm going to rewrite it a
little.

Currently prints (might be incomplete):

- mean character count
- number of characters
- most used characters
- most used sources
- most often replied to

# I want <some stat>!

Tell me! Open an issue!

# I want to add <some stat> myself!

Just send a PR.
This whole thing is licensed under MIT so you're probably fine using it.

# How do I actually use this?

```bash
# if you are using stack
stack setup
stack build
stack exec twitter-archive-parser < ../tweets.csv

# if you use cabal (assuming you have appropriate dependencies)
cabal run < ../tweets.csv

# if you happen to have ghc (8.0), parsec and containers installed you probably
# know how to compile and run this yourself
```

## Example Output

```text
mean character count: 67
number of characters: 7614380
most used characters: " etni"
```

