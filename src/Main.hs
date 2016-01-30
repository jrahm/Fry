module Main where

import Prelude hiding (readFile)

import Text.ParserCombinators.Parsec hiding (Parser, try)
import Text.Parsec.Text hiding (try)
import Text.Parsec.Prim

import System.Environment
import Data.Char
import Data.Maybe

import Data.Text.IO

data Expression =

          Identifier {identifier_name :: String}
        | Dot {dot_lhs :: Expression, dot_rhs :: String}
        | Call {call_lhs :: Expression, call_args :: [Expression]}
        | StringLiteral {string_literal_string :: String}
        | BinOp {binary_operator :: String, binop_lhs :: Expression, binop_rhs :: Expression}

        deriving (Show)

data Statement =

          Package {package_name :: String}
        | Function {function_name :: String, function_body :: [Statement]}
        | StmtExpr Expression

        deriving (Show)

data Module = Module String [Statement]

    deriving (Show)

parseLowerIdentifier :: Parser String
parseLowerIdentifier = do
    c <- satisfy (\c -> c <= 'z' && c >= 'a' || c == '_')
    (:) c <$> many (satisfy (\c -> isAlphaNum c || isDigit c || c == '_'))

space2 :: Parser Char
space2 = satisfy (\c -> isSpace c && not (c == '\n' || c == '\r'))

spaces2 :: Parser String
spaces2 = many space2

skip :: Parser ()
skip = space2 >> spaces2 >> return ()

endStmt :: Parser ()
endStmt = (try (spaces *> char ';') <|> try (spaces2 *> newline)) *> return ()

parseModule :: Parser Module
parseModule = do
    string "package"
    name <- spaces *> parseLowerIdentifier <* endStmt
    stmts <- catMaybes <$> many parseStatement
    eof
    return (Module name stmts)

end :: Parser ()
end = spaces2 *> string "end" *> spaces2 *> return ()

tok p = spaces2 *> p <* spaces2

tok' p = try (tok p)

expr :: Parser Expression
expr = do
       lhs <- leafExpr
       (choice . map try) [
              BinOp <$> binOp1 <*> pure lhs <*> expr
            , BinOp <$> binOp2 <*> pure lhs <*> expr
            , return (Call lhs []) <* string "()"
            , pure lhs
        ]

    where

        identifierStr = many1 (satisfy (\c -> isAlphaNum c || isDigit c || c == '_'))
        identifier = Identifier <$> identifierStr

        binOp1 = string "."
        binOp2 = string "+"
        leafExpr = tok (string "(") *> expr <* tok (string ")") <|> identifier


parseStatement :: Parser (Maybe Statement)
parseStatement = do
    spaces2
    choice (map (Just<$>) [
            parseFunction,
            parseExpr
        ]) <|> (endStmt *> return Nothing)

    where
        params = tok (string "(") *> tok (string ")")

        parseFunction = do
            name <- string "fn" *> skip *> parseLowerIdentifier
            params
            endStmt
            Function name <$> (catMaybes <$> manyTill parseStatement (try end))

        parseExpr = (StmtExpr <$> expr) <* endStmt

main :: IO ()
main = (>>=) getArgs $ \argv -> do
    ps <- parse parseModule (head argv) <$> readFile (head argv)
    case ps of
        Left err -> print err
        Right test -> print test
