module Language.Fry.AST where

import Language.Fry.Pretty
import Text.Printf

data Package annot =
        Package {package_name :: String, package_statements :: [Statement annot], package_annotation :: annot}
        deriving Show

data Expression annot =
          ExprIdentifier {identifier_name :: String, expr_annotation :: annot}
        | BinOp {binop_operator :: String, binop_lhs :: Expression annot, binop_rhs :: Expression annot, expr_annotation :: annot}
        | ExprNumber {expr_number :: Int, expr_annotation :: annot}
        | Call {call_expr :: Expression annot, expr_annotation :: annot}

        deriving Show

data Statement annot =
         Function {function_name :: String, function_body :: [Statement annot], statement_annotation :: annot}
       | StmtExpr {stmtexpr_expression :: Expression annot, statement_annotation :: annot}

       deriving Show

class Annotated a where
    annotation :: a e -> e

instance Annotated Statement where
    annotation = statement_annotation

instance Annotated Expression where
    annotation = expr_annotation

instance Annotated Package where
    annotation = package_annotation

instance Pretty (Expression annot) where
    pretty (ExprIdentifier id _) = id
    pretty (BinOp op rhs lhs _) = printf "(%s) %s (%s)" (pretty rhs) op (pretty lhs)
    pretty (ExprNumber num _) = show num
    pretty (Call expr _) = printf "(%s)()" (pretty expr)

instance Pretty (Statement a) where
    pretty (Function name body _) = "fn " ++ name ++ "()\n\n"
        ++ indent (concatMap pretty body) ++ "\nend\n"
    pretty (StmtExpr expr _) = pretty expr ++ "\n"

instance Pretty (Package a) where
    pretty (Package name stmts _) = "package " ++ name ++ "\n\n"
        ++ indent (concatMap pretty stmts)

indent :: String -> String
indent str = unlines $ map ("   "++) $ lines str
