{-# LANGUAGE FlexibleContexts #-}

module Language.Fry.ParsecHelp where

import Language.Fry.Tokenizer
import Language.Fry.AST

import Text.Parsec
import Control.Monad

import Text.Printf
import Safe

data Associativity = LA | RA deriving Show
data Op = Op {op_symbol :: String, op_prec :: Int, op_orient :: Associativity}

takeUntil :: (Stream s m (Token a), Show a) => (Token a -> Bool) -> ParsecT s u m [Token a]
takeUntil fn = many (satisfyToken (not . fn))

takeUntil1 :: (Stream s m (Token a), Show a) => (Token a -> Bool) -> ParsecT s u m [Token a]
takeUntil1 fn = many1 (satisfyToken (not . fn))

satisfyToken :: (Stream s m (Token a), Show a) => (Token a -> Bool) -> ParsecT s u m (Token a)
satisfyToken fn = try $ do
    tok <- anyToken
    if fn tok then
        return tok
        else parserFail $
            let (Token pos _ str) = tok in
            printf "%s: unexpected token %s" (show pos) (show str)

tokStrs :: (Show a, Stream s m (Token a)) => [String] -> ParsecT s u m ()
tokStrs = mapM_ tokStr

tokStr :: (Show a, Stream s m (Token a)) => String -> ParsecT s u m (Token a)
tokStr str =
    satisfyToken isRight <|> parserFail ("Unexpected token; expecting " ++ str)
    where isRight (Token _ _ str') = str == str'

keyword :: (Show a, Stream s m (Token a)) => String -> ParsecT s u m (Token a)
keyword str =
    satisfyToken isRight <|> parserFail ("Unexpected token; expecting " ++ str)
    where isRight (Token _ Identifier str') = str == str'
          isRight _ = False

identifier :: (Stream s m (Token a), Show a) => ParsecT s u m String
identifier = token_string <$> satisfyToken isIdentifier
    where isIdentifier (Token _ Identifier _) = True
          isIdentifier _ = False

isEOS :: Token a -> Bool
isEOS (Token _ EndOfStatement _) = True
isEOS _ = False

eos :: (Stream s m (Token a), Show a) => ParsecT s u m ()
eos = void $ satisfyToken isEOS

eob :: (Stream s m (Token a), Show a) => ParsecT s u m ()
eob = void $ many eos *> satisfyToken isEOB <* many eos
    where isEOB (Token _ Identifier "end") = True
          isEOB _ = False

operator :: (Stream s m (Token a), Show a) => ParsecT s u m String
operator = token_string <$> satisfyToken (\(Token _ t _) -> t == Operator)

openParen :: (Stream s m (Token a), Show a) => ParsecT s u m (Token a)
openParen = satisfyToken (\(Token _ t s) -> s == "(")

closeParen :: (Stream s m (Token a), Show a) => ParsecT s u m (Token a)
closeParen = satisfyToken (\(Token _ t s) -> s == ")")

readValue :: (Stream s m (Token a), Show a, Read b) => ParsecT s u m b
readValue = do
    (Token _ _ str) <- anyToken
    case readMay str of
        Nothing -> parserFail "Unexpected token"
        Just x -> return x

($>) :: (Monad m) => m a -> b -> m b
($>) a b = a >> return b

maybeFail :: String -> Maybe a -> ParsecT s u m a
maybeFail str Nothing = parserFail str
maybeFail _ (Just x) = return x
