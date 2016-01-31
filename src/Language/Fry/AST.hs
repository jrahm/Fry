module Language.Fry.AST where

import Language.Fry.Pretty
import Text.Printf
import Data.List
import Data.Maybe

data Package annot =
        Package {package_name :: String, package_statements :: [Statement annot], package_annotation :: annot}
        deriving Show

data Expression annot =
          ExprIdentifier {identifier_name :: String, expr_annotation :: annot}
        | BinOp {binop_operator :: String, binop_lhs :: Expression annot, binop_rhs :: Expression annot, expr_annotation :: annot}
        | ExprNumber {expr_number :: Int, expr_annotation :: annot}
        | Call {call_expr :: Expression annot, call_args :: [Expression annot], expr_annotation :: annot}
        | ListLiteral {list_exprs :: [Expression annot], expr_annotation :: annot}

        deriving Show

data Statement annot =
         Function {function_name :: String,
                   function_params :: [TypedIdentifier annot],
                   function_rettype :: Maybe (Expression annot),
                   function_body :: [Statement annot],
                   statement_annotation :: annot}

       | IfStmt {if_expression :: Expression annot,
                 if_body :: [Statement annot],
                 if_else :: [Statement annot],
                 statement_annotation :: annot}

       | StmtExpr {stmtexpr_expression :: Expression annot, statement_annotation :: annot}

       deriving Show

data TypedIdentifier annot =
        TypedIdentifier {typedid_name :: String, typedid_type :: Expression annot, typeid_annotation :: annot}

        deriving Show

class Annotated a where
    annotation :: a e -> e

instance Annotated Statement where
    annotation = statement_annotation

instance Annotated Expression where
    annotation = expr_annotation

instance Annotated Package where
    annotation = package_annotation

instance Annotated TypedIdentifier where
    annotation = typeid_annotation

instance Pretty (Expression annot) where
    pretty (ExprIdentifier id _) = id
    pretty (BinOp op rhs lhs _) = printf "(%s) %s (%s)" (pretty rhs) op (pretty lhs)
    pretty (ExprNumber num _) = show num
    pretty (Call expr args _) = printf "(%s)(%s)" (pretty expr) (intercalate ", " $ map pretty args)
    pretty (ListLiteral lst _) = printf "[%s]" (intercalate ", " $ map pretty lst)

instance Pretty (TypedIdentifier annot) where
    pretty (TypedIdentifier name typ _) = name ++ ": " ++ pretty typ

instance Pretty (Statement a) where
    pretty (Function name params rettyp body _) = "fn " ++ name ++ "(" ++ intercalate "," (map pretty params) ++ ")" ++
            maybe "" (\expr -> " -> " ++ pretty expr) rettyp
            ++ "\n\n"
        ++ indent (concatMap pretty body) ++ "\nend\n"

    pretty (IfStmt expr body elsebody _) =
        printf "if %s\n%s\n%s\nend"
            (pretty expr) (indent $ concatMap pretty body)
            (if not (null elsebody) then
                printf "else\n%s" (indent $ concatMap pretty elsebody) else "")

    pretty (StmtExpr expr _) = pretty expr ++ "\n"

instance Pretty (Package a) where
    pretty (Package name stmts _) = "package " ++ name ++ "\n\n"
        ++ indent (concatMap pretty stmts)

indent :: String -> String
indent str = unlines $ map ("   "++) $ lines str
