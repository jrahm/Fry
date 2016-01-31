{-# LANGUAGE FlexibleContexts #-}
module Main where

import Debug.Trace
import Control.Monad
import Language.Fry.Tokenizer
import Language.Fry.AST
import Language.Fry.ParsecHelp
import Language.Fry.Pretty
import Language.Fry.Interpret.Interpreter

import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.List
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as M

import Text.Parsec

data FryParseState = FryParseState {operator_map :: Map String Op}
addOperator :: Op -> FryParseState -> FryParseState
addOperator op fs = fs {operator_map = M.insert (op_symbol op) op (operator_map fs)}

type Parser = Parsec [Token SourcePos] FryParseState

operators :: Map String Op
operators = M.fromList $ map (\o@(Op s _ _) -> (s, o)) [
                Op "=" 0 RA,
                Op "|" 5 LA, Op "&" 5 LA,
                Op "+" 10 LA, Op "-" 10 LA,
                Op "*" 20 LA, Op "/" 20 LA, Op "%" 20 LA
            ]

parseModule :: Parser Package
parseModule = Package <$>
    (keyword "package" *> identifier <* eos) <*>
     many statementOrInfix <* eof

statementOrInfix :: Parser Statement
statementOrInfix =
    (do
        assoc <- try (keyword "infixl" $> LA) <|> try (keyword "infixr" $> RA)
        op <- operator
        number <- readValue
        eos
        updateState (addOperator (Op op number assoc))
        statementOrInfix
        ) <|> statement

expression :: Parser Expression
expression = try (do
    lhs <- primaryExpression
    op <- operator
    rhs <- primaryExpression

    expression' lhs op rhs) <|> primaryExpression

    where
        primaryExpression' lhs =
            (try openParen *> closeParen *> primaryExpression' (Call lhs))
                <|> return lhs

        primaryExpression = primaryExpression' =<< leafExpression

        leafExpression =
            (openParen *> expression <* closeParen) <|>
            ExprIdentifier <$> identifier <|>
            ExprNumber <$> number


        compop :: String -> String -> Parser Bool
        compop o1 o2 = do
                    FryParseState {operator_map = ops} <- getState

                    op1@(Op _ prec1 _) <- maybeFail ("Unknown operator: " ++ o1) $ M.lookup o1 ops
                    op2@(Op _ prec2 assoc) <- maybeFail ("Unknown operator: " ++ o2) $ M.lookup o2 ops

                    case prec1 `compare` prec2 of
                            GT -> return True
                            LT -> return False
                            EQ -> case assoc of
                                    LA -> return True
                                    RA -> return False

        expression' :: Expression -> String -> Expression -> Parser Expression
        expression' lhs op rhs = (do
                op2 <- operator
                b <- compop op op2
                if b then
                    expression' (BinOp op lhs rhs) op2 =<< primaryExpression
                    else
                    BinOp op lhs <$> (expression' rhs op2 =<< primaryExpression)) <|>
                return (BinOp op lhs rhs)

statement :: Parser Statement
statement = try (do
        (Token _ _ s) <- lookAhead anyToken
        when (s == "end") $ parserFail ""
    ) *> statement'
    where
        statement' = function <|> statementExpr

        function = try (keyword "fn") *>
            (Function <$> identifier <*>
                (tokStrs ["(", ")"] *> eos *> many statement <* eob))

        statementExpr = StmtExpr <$> expression <* eos



main :: IO ()
main = (>>=) getArgs $ \argv -> do
    ps <- tokenize (head argv) <$> Text.readFile (head argv)
    case ps of
        Left err -> print err
        Right tokens -> do
            mapM_ prettyPrint tokens
            let ps' = runParser parseModule (FryParseState operators) (head argv) tokens
            case ps' of
                Left err -> print err
                Right ast -> do
                    prettyPrint ast
                    interpret ast
