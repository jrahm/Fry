{-# LANGUAGE ScopedTypeVariables #-}
module Language.Fry.Typecheck.TypeCheck where

import Language.Fry.AST
import Control.Monad
import Debug.Trace

import Prelude hiding (lookup)
import Data.List hiding (lookup)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Monoid
import Data.Maybe

import Language.Fry.Pretty
import Control.Monad.Writer

import Control.Applicative
import Text.Printf

{- The Kind of a Type. Right now, since there are
 - no dependent types in Fry, this is just an integer
 - telling the number of type variables this Type expects -}
data Kind = Kind Int
                deriving (Show, Eq)

{- A base type is a structure. It is a name with a Kind -}
data BaseType =
        BaseType {basetype_name :: String,
                  basetype_kind :: Kind}
            deriving (Show, Eq)

{- A data type is either an applied base type, or it is
 - a type variable. In either sense, this type is recursive to
 - represent the meaning of having types within types. -}
data DataType =
        DataType {
            datatype_basetype :: BaseType,
            datatype_vars :: [DataType]
        } | TypeVar {
            datatype_varname :: String,
            datatype_vars :: [DataType]
        }
        deriving (Eq, Show)

instance Pretty BaseType where
    pretty (BaseType name _) = name

instance Pretty DataType where
    pretty (DataType base vars) = pretty base ++ if null vars then "" else
                                    "(" ++ intercalate ", " (map pretty vars) ++ ")"

    pretty (TypeVar base vars) = "'" ++ base ++ if null vars then "" else
                                    "(" ++ intercalate ", " (map pretty vars) ++ ")"

data TypeCheckState annot =
        TypeCheckState {
            checkstate_varmap :: Map String DataType
        } |
        Failed String annot

        deriving Show

lookup :: TypeCheckState a -> String -> Maybe DataType
lookup Failed {} _ = Nothing
lookup TypeCheckState {checkstate_varmap = x} s = Map.lookup s x

matchArrow :: [DataType] -> DataType -> Either String DataType
matchArrow [] dt = Right dt
matchArrow (t:ts) dt =
    case dt of
        DataType (BaseType "Arrow" _) [a, b] -> t `compat` a >> matchArrow ts b
        _ -> Left ("Expected arrow, got" ++ show dt ++ " probable cause, function applied to too many arguments.")

compat :: DataType -> DataType -> Either String ()
{- return if t1 is compatible as t2-}
compat (DataType dt vars1) (TypeVar vars vars2) =
    zipWithM_ compat vars1 vars2

compat (DataType dt1 vars1) (DataType dt2 vars2) = do
    unless (dt1 == dt2) $
        Left $ "Could not match type " ++ pretty dt1 ++ " with " ++ pretty dt2
    zipWithM_ compat vars1 vars2

compat (TypeVar base1 vars1) (TypeVar base2 vars2) =
    zipWithM_ compat vars1 vars2

compat _ _ = Left "Unable to match type variable to non type variable"


instance (Show a) => Pretty (TypeCheckState a) where
    pretty (Failed str at) = show at ++ ": TypeCheck failed. " ++ str
    pretty (TypeCheckState mp) = concatMap (\(k, v) -> k ++ " :: " ++ pretty v ++ "\n") $ Map.toList mp

instance Monoid (TypeCheckState annot) where
    mappend (TypeCheckState mp1) (TypeCheckState mp2) = TypeCheckState (mp1 `mappend` mp2)
    mappend t@Failed {} _ = t

    mempty = TypeCheckState mempty

baseDataType :: String -> DataType
baseDataType str = DataType (BaseType str (Kind 0)) []

unitType :: DataType
unitType = baseDataType "Unit"

intType :: DataType
intType = baseDataType "Int"

stringType :: DataType
stringType = baseDataType "String"

listType :: DataType -> DataType
listType typ = DataType (BaseType "List" (Kind 1)) [typ]

arrowType :: DataType -> DataType -> DataType
arrowType a b = DataType (BaseType "Arrow" (Kind 2)) [a, b]

alter :: [a] -> [a] -> [a]
alter [] l2 = l2
alter l1 _ = l1

typecheck :: forall annot.
             (Show annot) => TypeCheckState annot ->
             [Statement annot] -> [(String, annot)] -- return list of error messages
                                                    -- and their position
typecheck st stmts =
    let collect = st `mappend` collectConstantTypes stmts
    in
    trace (pretty collect)
    check collect stmts

    where
        exprType :: TypeCheckState annot -> Expression annot -> Either String DataType
        exprType state expr =
            case expr of
                ExprIdentifier id _ ->
                    case lookup state id of
                        Nothing -> Left ("Undefined reference to " ++ id)
                        Just d -> return d

                Call fn args _ -> do
                    functype <- exprType state fn
                    argsTypes' <- mapM (exprType state) args
                    let argTypes = argsTypes' `alter` [unitType]

                    matchArrow argTypes functype

                ExprNumber _ _ -> return intType

                StringLiteral _ _ -> return stringType

                ListLiteral exprs _ ->
                    if null exprs then
                        return $ listType (TypeVar "A" [])
                    else do
                        typ <- exprType state $ head exprs
                        mapM_ (exprType state >=> flip compat typ) (tail exprs)
                        return $ listType typ
                _ -> undefined


        check :: TypeCheckState annot -> [Statement annot] -> [(String, annot)]
        check state stmts =
            execWriter $ forM stmts $ tell . checkStatement

            where
                checkStatement :: Statement annot -> [(String, annot)]
                checkStatement s = case s of
                    Function _ _ _ _ body _ ->
                        typecheck state body

                    StmtExpr expr annot ->
                        case exprType state expr of
                            Left err -> [(err, annot)]
                            Right _ -> []

                    _ -> trace ("WARN: " ++ show s ++ " unimplemented") []



{- This is going to be a 2-phase type-check. The first phase will not
 - descend into functions, but rather will simply record the type of
 - functions. This wil help with type-checking mutually recursive
 - functions of the sorts. -}
collectConstantTypes :: (Show annot) => [Statement annot] -> TypeCheckState annot
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

            _ -> TypeCheckState Map.empty

    where
        toArrowFn :: (Show annot) => [Expression annot] -> Set String -> DataType
        toArrowFn [expr] str = toArrowFn' [ExprIdentifier "Unit" $ annotation expr, expr] str
        toArrowFn xs str = toArrowFn' xs str

        toArrowFn' :: (Show annot) => [Expression annot] -> Set String -> DataType
        toArrowFn' [expr] str = toDataType str expr
        toArrowFn' (a:as) str = arrowType (toDataType str a) (toArrowFn' as str)

        toDataType :: (Show annot) => Set String -> Expression annot -> DataType
        toDataType str expr = case expr of
            (ExprIdentifier id' _) ->
                if id' `Set.member` str then
                    TypeVar id' []
                    else
                        baseDataType id'

            (Call (ExprIdentifier id' _) args _) ->
                if id' `Set.member` str then
                    TypeVar id' (map (toDataType str) args)
                    else
                        DataType (BaseType id' (Kind $ length args))
                            (map (toDataType str) args)

            t -> error (show (annotation t) ++ ": Unallowed expression in type construct: " ++ pretty t)
