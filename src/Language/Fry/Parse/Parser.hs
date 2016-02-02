{-# LANGUAGE FlexibleContexts #-}
module Language.Fry.Parse.Parser where

import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad
import Control.Monad.Trans

import Language.Fry.Tokenizer
import Language.Fry.AST
import Language.Fry.ParsecHelp
import Language.Fry.Pretty

import Text.Parsec
import Text.Parsec.Error

{- The Fry Parser type. This works on tokens rather than
 - straight text. -}
type Parser = ParsecT [Token SourcePos] FryParseState (Either ParseError)

{- The parsing state. This is used when certain statements
 - affect the parsing of the AST. For example, the infix
 - keywords will change what operators are parsed in expression
 - parsing. -}
data FryParseState = FryParseState {operator_map :: Map String Op}

{- Add an operator to the parse state -}
addOperator :: Op -> FryParseState -> FryParseState
addOperator op fs = fs {operator_map = M.insert (op_symbol op) op (operator_map fs)}

{- List of default operators that are allowed. -}
operators :: Map String Op
operators = M.fromList $ map (\o@Op {op_symbol = s} -> (s, o)) [
                Op "=" 0 RA "set",
                Op "|" 5 LA "or", Op "&" 5 LA "and",
                Op "+" 10 LA "add", Op "-" 10 LA "subtract",
                Op "*" 20 LA "multiply", Op "/" 20 LA "divide", Op "%" 20 LA "modulo",
                Op "->" 30 RA "Arrow",
                Op "." 1000 LA "get"
            ]
{- Function that parses a package. It parses statements or
 - the infix macros. -}
parsePackage :: Parser (Package SourcePos)
parsePackage = annotate $
    Package <$>
    (keyword "package" *> identifier <* eos) <*>
     many statementOrInfix <* eof

     where

        statementOrInfix :: Parser (Statement SourcePos)
        statementOrInfix =
            (do
                assoc <- try (keyword "infixl" $> LA) <|> try (keyword "infixr" $> RA)
                op <- operator
                number <- readValue
                alias <- identifier
                eos
                updateState (addOperator (Op op number assoc alias))
                statementOrInfix
                ) <|> statement

parameterList :: Parser [TypedIdentifier SourcePos]
parameterList = option [] $
    liftM2 (:) parameterList' (many $ comma *> parameterList')
    where
        parameterList' =
            annotate $ TypedIdentifier <$> identifier <*> (tokStr ":" *> expression <* many eos)

{- Parse a statement. Many times this means also parsing an end
 - token, like in the case of function parsing. -}
statement :: Parser (Statement SourcePos)
statement = try (do
        (Token _ _ s) <- lookAhead anyToken
        when (s == "end" || s == "else") $ parserFail ""
    ) *> statement'
    where
        statement' = structure <|> function <|> ifstmt <|> returnstmt <|> statementExpr

        idlist :: Parser [String]
        idlist = liftM2 (:) identifier (many $ comma *> identifier)

        structure :: Parser (Statement SourcePos)
        structure = annotate $
            try (keyword "struct") *>
                (Structure <$> identifier <*> option [] (openParen *> idlist <* closeParen) <*> (eos *> parameterList)) <* eob

        parseRettype :: Parser (Maybe (Expression SourcePos))
        parseRettype =
            optionMaybe $ do
                try (tokStr "->")
                expression

        functionContext :: Parser [TypedIdentifier SourcePos]
        functionContext =
            option [] $
            openBracket *> parameterList <* closeBracket

        function = annotate $
            try (keyword "fn") *>
                (Function <$>
                    functionContext <*>
                    identifier <*> (openParen *> parameterList <* closeParen) <*> (parseRettype <* eos) <*>
                    (many statement <* eob))

        ifstmt = annotate $
            try (keyword "if") *>
                (IfStmt <$> expression <*> (eos *> many statement) <*>
                (keyword "else" *> eos *> many statement <|> return []))
                <* eob

        returnstmt = annotate $
            try (keyword "return") *>
                (Return <$> expression <* eos)

        statementExpr = annotate $ StmtExpr <$> expression <* eos


expressionList :: Parser [Expression SourcePos]
expressionList = option [] $
    liftM2 (:) expression (many $ comma *> expression)

{- Parse an expression. This will use the state gathered from before
 - to make sure that the parsing is correct with the infix
 - macros defined before. -}
expression :: Parser (Expression SourcePos)
expression = stmtexpr <|> try (do
    lhs <- primaryExpression
    op <- getOp =<< operator
    rhs <- primaryExpression

    expression' lhs op rhs) <|> primaryExpression

    where
        stmtexpr = lookAhead (keyword "fn") *> annotate (ExprStmt <$> statement)

        primaryExpression' :: Expression SourcePos -> Parser (Expression SourcePos)
        primaryExpression' lhs =
            (try openParen *> do
                args <- expressionList
                closeParen
                primaryExpression' (Call lhs args $ annotation lhs))
                <|> return lhs

        primaryExpression = primaryExpression' =<< leafExpression

        leafExpression =
            (openParen *> expression <* closeParen) <|>
            annotate (ExprIdentifier <$> identifier <|>
                      ExprNumber <$> number <|>
                      StringLiteral <$> stringLiteral <|>
                      ListLiteral <$> (openBracket *> expressionList <* closeBracket))


        getOp :: String -> Parser Op
        getOp s = do
            FryParseState {operator_map = ops} <- getState

            let
                maybeFail' str Nothing = do
                    errmsg <- newErrorMessage <$> pure (Message str) <*> getAnnot
                    traceM "Errors!!!!!"
                    lift (Left errmsg :: Either ParseError ())
                    parserFail str

                maybeFail' str (Just j) = return j

            maybeFail' ("Unknown operator: " ++ s) $ M.lookup s  ops

        compop (Op _ prec1 assoc _) (Op _ prec2 _ _) =
                    case prec1 `compare` prec2 of
                            GT -> True
                            LT -> False
                            EQ -> case assoc of
                                    LA -> True
                                    RA -> False

        operatorToCall :: Expression a -> Expression a -> Op -> Expression a
        operatorToCall lhs rhs (Op _ _ _ alias) =
            let annot = annotation lhs in
            Call (ExprIdentifier alias annot)
                [lhs, rhs] annot

        expression' :: Expression SourcePos -> Op -> Expression SourcePos -> Parser (Expression SourcePos)
        expression' lhs op1 rhs = (do
                op' <- operator
                op2 <- getOp op'
                if compop op1 op2 then
                    expression' (operatorToCall lhs rhs op1) op2 =<< primaryExpression
                    else
                        operatorToCall lhs <$>
                            (expression' rhs op2 =<< primaryExpression) <*> pure op1
                ) <|> return (operatorToCall lhs rhs op1)
