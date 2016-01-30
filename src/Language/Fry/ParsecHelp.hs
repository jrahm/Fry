{-# LANGUAGE FlexibleContexts #-}

module Language.Fry.ParsecHelp where

import Language.Fry.Tokenizer
import Language.Fry.AST

import Text.Parsec
import Control.Monad

satisfyToken :: (Stream s m (Token a), Show a) => (Token a -> Bool) -> ParsecT s u m (Token a)
satisfyToken fn = try $ do
    tok <- anyToken
    if fn tok then
        return tok
        else parserFail $ "Unexpected token " ++ show tok

identifier :: (Stream s m (Token a), Show a) => ParsecT s u m String
identifier = token_string <$> satisfyToken isIdentifier
    where isIdentifier (Token _ Identifier _) = True
          isIdentifier _ = False

eos :: (Stream s m (Token a), Show a) => ParsecT s u m ()
eos = void $ satisfyToken isEOS
    where isEOS (Token _ EndOfStatement _) = True
          isEOS _ = False

eob :: (Stream s m (Token a), Show a) => ParsecT s u m ()
eob = void $ satisfyToken isEOB
    where isEOB (Token _ Identifier "end") = True
          isEOB _ = False
