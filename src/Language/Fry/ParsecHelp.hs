{-# LANGUAGE FlexibleContexts #-}

module Language.Fry.ParsecHelp where

import Language.Fry.Tokenizer
import Language.Fry.AST
import Control.Arrow

import Text.Parsec
import Control.Monad

import Text.Printf
import Safe
import qualified Data.Set as Set

reserved :: Set.Set String
reserved = Set.fromList ["end"]


data Associativity = LA | RA deriving Show
data Op = Op {op_symbol :: String, op_prec :: Int, op_orient :: Associativity, op_alias :: String}

takeUntil :: (Stream s m (Token a), Show a) => (Token a -> Bool) -> ParsecT s u m [Token a]
takeUntil fn = many (satisfyToken (not . fn))

takeUntil1 :: (Stream s m (Token a), Show a) => (Token a -> Bool) -> ParsecT s u m [Token a]
takeUntil1 fn = many1 (satisfyToken (not . fn))

getAnnot :: (Stream s m (Token a), Show a) => ParsecT s u m a
getAnnot = do
    (Token pos _ _) <- lookAhead anyToken
    return pos

annotate :: (Stream s m (Token sr), Show sr) =>
                ParsecT s u m (sr -> a) -> ParsecT s u m a
annotate sec = do
    annot <- getAnnot
    sec <*> pure annot


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
    where isIdentifier (Token _ Identifier str) = not (str `Set.member` reserved)
          isIdentifier _ = False

number :: (Stream s m (Token a), Show a) => ParsecT s u m Int
number = do (Token _ _ str) <- satisfyToken (token_type >>> (==Number))
            case readMay str of
                Nothing -> parserFail "Expecting number"
                Just x -> return x


isEOS :: Token a -> Bool
isEOS (Token _ EndOfStatement _) = True
isEOS _ = False

eos :: (Stream s m (Token a), Show a) => ParsecT s u m ()
eos = void $ satisfyToken isEOS

comma :: (Stream s m (Token a), Show a) => ParsecT s u m ()
comma = void $ satisfyToken isComma
    where
        isComma (Token _ Comma _) = True
        isComma _ = False



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

openBracket :: (Stream s m (Token a), Show a) => ParsecT s u m (Token a)
openBracket = satisfyToken (\(Token _ t s) -> s == "[")

closeBracket :: (Stream s m (Token a), Show a) => ParsecT s u m (Token a)
closeBracket = satisfyToken (\(Token _ t s) -> s == "]")

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

stringLiteral :: (Stream s m (Token a), Show a) => ParsecT s u m String
stringLiteral = do
    (Token _ _ str) <- satisfyToken isString
    return str
    where
        isString (Token _ String _) = True
        isString _ = False
