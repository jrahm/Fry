module Language.Fry.Typecheck.TypeCheck where

import Language.Fry.AST
import Control.Monad

import Data.List
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Monoid
import Data.Maybe

import Language.Fry.Pretty

data DataType =
        DataType
           {datatype_base :: String,
            datatype_vars :: [DataType]} |
            TypeVar String [DataType]

        deriving Show

instance Pretty DataType where
    pretty (DataType base vars) = base ++ if null vars then "" else
                                    "(" ++ intercalate ", " (map pretty vars) ++ ")"

    pretty (TypeVar base vars) = "'" ++ base ++ if null vars then "" else
                                    "(" ++ intercalate ", " (map pretty vars) ++ ")"

data TypeCheckState annot =
        TypeCheckState {
            checkstate_varmap :: Map String DataType
        } |
        Failed String annot

        deriving Show

instance (Show a) => Pretty (TypeCheckState a) where
    pretty (Failed str at) = show at ++ ": TypeCheck failed. " ++ str
    pretty (TypeCheckState mp) = concatMap (\(k, v) -> k ++ " :: " ++ pretty v ++ "\n") $ Map.toList mp

instance Monoid (TypeCheckState annot) where
    mappend (TypeCheckState mp1) (TypeCheckState mp2) = TypeCheckState (mp1 `mappend` mp2)
    mappend t@Failed {} _ = t

    mempty = TypeCheckState mempty

{- This is going to be a 2-phase type-check. The first phase will not
 - descend into functions, but rather will simply record the type of
 - functions. This wil help with type-checking mutually recursive
 - functions of the sorts. -}

collectConstantTypes :: [Statement annot] -> TypeCheckState annot
collectConstantTypes statements =
    (mconcat . flip map statements) $ \stmt ->
        case stmt of
            Function ctx name params ret _ annot ->
                let typevars = Set.fromList $ map (\(TypedIdentifier x _ _) -> x) ctx
                    paramtypes = map (\(TypedIdentifier _ x _) -> x) params
                    ret' = fromMaybe (ExprIdentifier "Unit" annot) ret
                    fntypeexpr = toArrowFn (paramtypes ++ [ret']) typevars
                    in

                TypeCheckState $
                    Map.singleton name fntypeexpr

    where
        toArrowFn :: [Expression annot] -> Set String -> DataType
        toArrowFn [expr] str = toArrowFn' [ExprIdentifier "Unit" $ annotation expr, expr] str
        toArrowFn xs str = toArrowFn' xs str

        toArrowFn' :: [Expression annot] -> Set String -> DataType
        toArrowFn' [expr] str = toDataType str expr
        toArrowFn' (a:as) str = DataType "Arrow" [toDataType str a, toArrowFn' as str]

        toDataType str expr = case expr of
            (ExprIdentifier id' _) ->
                if id' `Set.member` str then
                    TypeVar id' []
                    else
                        DataType id' []

            (Call (ExprIdentifier id' _) args _) ->
                if id' `Set.member` str then
                    TypeVar id' (map (toDataType str) args)
                    else
                        DataType id' (map (toDataType str) args)

            t -> error ("Unallowed expression in type construct: " ++ pretty t)
