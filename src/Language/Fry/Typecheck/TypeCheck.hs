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

import My.Util

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

{- The type checking state. Will look for potential errors
 - as well as keep track of the types of all functions. -}
data TypeCheckState annot =
        TypeCheckState {
              {- A map of variables and their associated types
               - in this scope. -}
              checkstate_varmap :: Map String DataType

              {- A map of the structures and types and
               - their associated base types. -}
            , checkstate_typemap :: Map String BaseType
        }

        deriving Show

lookup :: TypeCheckState a -> String -> Maybe DataType
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
    pretty (TypeCheckState mp mp1) =
        (concatMap (\(k, v) -> k ++ " :: " ++ pretty v ++ "\n") $ Map.toList mp) ++
        (concatMap (\(k, (BaseType _ (Kind v))) -> k ++ "<" ++ show v ++ ">") $ Map.toList mp1)

instance Monoid (TypeCheckState annot) where
    mappend (TypeCheckState mp1 mp1') (TypeCheckState mp2 mp2') =
        TypeCheckState (mp1 `mappend` mp2) (mp1' `mappend` mp2')

    mempty = TypeCheckState mempty mempty

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

unfoldArrow :: DataType -> [DataType]
unfoldArrow (DataType (BaseType "Arrow" (Kind 2)) [a, b]) = a : unfoldArrow b
unfoldArrow t = return t

alter :: [a] -> [a] -> [a]
alter [] l2 = l2
alter l1 _ = l1

typecheck :: forall annot.
             (Show annot) => TypeCheckState annot ->
             [Statement annot] -> [(String, annot)] -- return list of error messages
                                                    -- and their position
typecheck st stmts =
    let tmp = do
            newstate <- liftM (mappend st) $ collectStructures stmts
            collectConstantTypes newstate stmts
            in
    case tmp of
        Left err -> [err]
        Right st ->
            check st stmts

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
                    Function _ name params _ body annot ->
                        case lookup state name of
                            Nothing -> [("Unable to find type for: " ++ name, annot)]
                            Just t ->
                                let paramTypes = unfoldArrow t in
                                if (length paramTypes - 1 /= length params) &&
                                   not (null params) then
                                    [("Param type discrepency", annot)]
                                    else
                                        let state' = mappend state $
                                             flip mconcatMap (zip paramTypes params) $
                                                \(typ, (TypedIdentifier n _ _)) ->
                                                    TypeCheckState (Map.singleton n typ)
                                                                    Map.empty
                                            in
                                        typecheck state' body

                    StmtExpr expr annot ->
                        case exprType state expr of
                            Left err -> [(err, annot)]
                            Right _ -> []

                    _ -> trace ("WARN: " ++ show s ++ " unimplemented") []



{- This is going to be a 2-phase type-check. The first phase will not
 - descend into functions, but rather will simply record the type of
 - functions. This wil help with type-checking mutually recursive
 - functions of the sorts. -}
collectConstantTypes :: (Show annot) => TypeCheckState annot ->
                                       [Statement annot]     ->
                                       Either (String, annot) (TypeCheckState annot)
collectConstantTypes state statements =
    liftM (mappend state) $
    flip mconcatMapM statements $ \stmt ->
        case stmt of
            Function ctx name params ret _ annot -> do
                {- The type variables for this function. -}
                let typevars = Set.fromList $ map typedid_name ctx
                {- The types of all the parameters of a function -}
                let paramtypes = map typedid_type params
                {- The return type, changed to contain unit if not defined.  -}
                let ret' = fromMaybe (ExprIdentifier "Unit" annot) ret

                {- This is where the checking is done to make sure all
                 - the type values are in fact valid -}
                fntypeexpr <- toArrowFn (paramtypes ++ [ret']) typevars

                return $ TypeCheckState
                            (Map.singleton name fntypeexpr)
                             Map.empty

            _ -> return $ TypeCheckState Map.empty Map.empty

    where
        toArrowFn [expr] str = toArrowFn' [ExprIdentifier "Unit" $ annotation expr, expr] str
        toArrowFn xs str = toArrowFn' xs str

        toArrowFn' [expr] str = toDataType state str expr
        toArrowFn' (a:as) str = arrowType <$> toDataType state str a <*> toArrowFn' as str

lookupStructure :: (Show annot) => TypeCheckState annot -> annot -> String -> Either (String, annot) BaseType
lookupStructure state pos string =
    maybe (Left ("Undeclared type identifier: " ++ string, pos)) return
        (Map.lookup string (checkstate_typemap state))

toDataType :: (Show annot) => TypeCheckState annot -> Set String -> Expression annot -> Either (String, annot) DataType
toDataType state str expr = case expr of
    (ExprIdentifier id' annot) ->
        if id' `Set.member` str then
            return $ TypeVar id' []
            else do
                {- This guy had better have Kind=0, otherwise it shouldn't be
                 - used without it's type arguments. -}
                baseType <- lookupStructure state annot id'
                case baseType of
                    (BaseType _ (Kind 0)) -> return $ DataType baseType []
                    (BaseType _ (Kind k)) ->
                        Left ("0 arguments given to type function " ++
                            id' ++ ", " ++ show k ++ " expected!", annot)

    (Call (ExprIdentifier id' annot) args _) ->
        if id' `Set.member` str then
            {- In the function  we are type checking, this is
             - a type variable. -}
            TypeVar id' <$> mapM (toDataType state str) args
            else
                {- In the function, the data type given is not
                 - a type variable, so we must find it in the state. -}
                let mkDataType bt@(BaseType id' (Kind x)) args = do
                        unless (length args == x) $
                            Left (printf "Type function %s applied to wrong number of arguments %d (expected %d)"
                                    id' (length args) x, annot)

                        return $ DataType bt args
                        in do
                structure <- lookupStructure state annot id'
                typeargs <- mapM (toDataType state str) args
                mkDataType structure typeargs

    t ->
        Left ("Unallowed expression in type construct: " ++ pretty t, annotation t)

collectStructures :: (Show annot) => [Statement annot] -> Either (String, annot) (TypeCheckState annot)
collectStructures statements =
    flip mconcatMapM statements $ \stmt ->
        case stmt of
            Structure name vars _ _ ->
                return $
                    TypeCheckState
                        Map.empty
                        (Map.singleton name $ BaseType name $ Kind $ length vars)
            _ -> return mempty

instance Pretty BaseType where
    pretty (BaseType name _) = name

instance Pretty DataType where
    pretty (DataType base vars) = pretty base ++ if null vars then "" else
                                    "(" ++ intercalate ", " (map pretty vars) ++ ")"

    pretty (TypeVar base vars) = "'" ++ base ++ if null vars then "" else
                                    "(" ++ intercalate ", " (map pretty vars) ++ ")"
