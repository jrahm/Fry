module Language.Fry.AST where

import Language.Fry.Pretty
import Text.Printf

data Package =
        Package {package_name :: String, package_statements :: [Statement]}
        deriving Show

data Expression =
          ExprIdentifier {identifier_name :: String}
        | BinOp {binop_operator :: String, binop_lhs :: Expression, binop_rhs :: Expression}

        deriving Show

data Statement =
         Function {function_name :: String, function_body :: [Statement]}
       | StmtExpr {stmtexpr_expression :: Expression}

       deriving Show

instance Pretty Expression where
    pretty (ExprIdentifier id) = id
    pretty (BinOp op rhs lhs) = printf "(%s)%s(%s)" (pretty rhs) op (pretty lhs)

instance Pretty Statement where
    pretty (Function name body) = "fn " ++ name ++ "()\n\n"
        ++ indent (concatMap pretty body) ++ "\nend\n"
    pretty (StmtExpr expr) = pretty expr ++ "\n"

instance Pretty Package where
    pretty (Package name stmts) = "package " ++ name ++ "\n\n"
        ++ indent (concatMap pretty stmts)

indent :: String -> String
indent str = unlines $ map ("   "++) $ lines str
