module TwitterMonad
( TwitterMonad
, TwitterError (..)
) where

import Text.ParserCombinators.Parsec

data TwitterError = InputError ParseError

type TwitterMonad = Either TwitterError

