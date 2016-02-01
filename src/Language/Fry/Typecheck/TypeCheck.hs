module Language.Fry.Typecheck where

import Language.Fry.AST
import Control.Monad

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

data DataType =
        DataType
           {datatype_base :: String,
            datatype_vars :: [DataType]}

data TypeCheckState =
        TypeCheckState {
            checkstate_varmap :: Map String DataType
        }

{- Attempt to type-check the AST. If all is successful, a new
 - version of the ast is returned with all the correct functions
 - put into place. -}
typecheckPackage :: Package annot -> Either (String, annot) (Package annot)
typecheckPackage (Package name stmts _) =
    combStatementList stmts (TypeCheckState
    foldM checkStatement stmts

checkStatement :: ([Statement annot], TypeCheckState) ->
                    Statement annot ->
                    Either (String, annot) ([Statement annot], TypeCheckState)
checkStatement (lst, st) stmt = case stmt of
    (Function _ name params rettype body annot) ->
