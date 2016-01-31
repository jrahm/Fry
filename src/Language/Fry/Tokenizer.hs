module Language.Fry.Tokenizer(TokenType(..), Token(..), tokenize, SourcePos) where

import Text.ParserCombinators.Parsec hiding (Parser, try)
import Text.Parsec.Text hiding (try)
import Text.Parsec.Prim
import Data.Char
import Control.Monad
import Data.Text
import Language.Fry.Pretty

import Data.Set (Set, fromList, member)

data TokenType =
          Identifier
        | EndOfStatement
        | Paren
        | Comma
        | Operator
        | String
        | Number
        | Bracket

        deriving (Show, Eq, Ord)

data Token st = Token {token_annot :: st, token_type :: TokenType, token_string :: String} deriving Show

instance Pretty (Token st) where
    pretty (Token _ typ str) = "Tok " ++ show typ ++ " " ++ show str


operatorChars :: Set Char
operatorChars = fromList "~!@$%^&*/?+/=:'|.><-"

instance Functor Token where
    fmap f (Token x t s) = Token (f x) t s

space2 :: Parser Char
space2 = try escapeNewline <|> satisfy (\c -> isSpace c && not (c == '\n' || c == '\r'))
    where
        escapeNewline = char '\\' *> newline

spaces2 :: Parser String
spaces2 = many space2

sourcePos :: Monad m => ParsecT s u m SourcePos
sourcePos = statePos `liftM` getParserState

mtoken :: Parser (Token SourcePos)
mtoken = spaces2 *> mtoken' <* spaces2
    where
        mtoken' = do
            pos <- sourcePos
            (typ, str) <- mtoken''
            Token <$> pure pos <*> pure typ <*> pure str

        mtoken'' = choice [
              (,) Identifier <$> identifier
            , (,) EndOfStatement <$> many1 newline
            , (,) Paren <$> paren
            , (,) Comma <$> comma
            , (,) Operator <$> operator
            , (,) String <$> rstring
            , (,) Number <$> number
            , (,) Bracket <$> bracket
            ]

        identifier = liftM2 (:) (satisfy isAlpha) (many (satisfy (\c -> isAlphaNum c || isDigit c || c == '_')))
        bracket = return <$> oneOf "[]"
        paren = return <$> oneOf "()"
        comma = string ","
        operator = many1 $ satisfy (`member`operatorChars)
        number = many1 digit
        rstring = char '"' *> many scanString <* char '"'
            where
                scanString =
                    char '\\' *> anyChar <|> satisfy (/='"')

tokenize :: SourceName -> Text -> Either ParseError [Token SourcePos]
tokenize = parse (many mtoken <* eof)
