

module Language.Fry.Typecheck.Util where

import Language.Fry.Typecheck.Internal
import qualified Data.Map as Map

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

{-| Test the compatibility of the first data type as
 - the second data type. -}
compat :: DataType -> DataType -> Either String [(String, String)]
{- return if t1 is compatible as t2-}
compat (DataType (BaseType bt _) vars1) (TypeVar name vars2) = do
    discovered <- concat <$> zipWithM compat vars1 vars2
    return ((name, bt):discovered)

compat (DataType dt1 vars1) (DataType dt2 vars2) = do
    unless (dt1 == dt2) $
        Left $ "Could not match type " ++ pretty dt1 ++ " with " ++ pretty dt2
    concat <$> zipWithM compat vars1 vars2

compat (TypeVar base1 vars1) (TypeVar base2 vars2) = do
    unless (base1 == base2) $
        Left $ "Could not match type " ++ base1 ++ " with " ++ base2
    concat <$> zipWithM compat vars1 vars2

compat _ _ = Left "Unable to match type variable to non type variable"

{- Lookup the type of a variable in the type check state.
 - Evaluates to nothing if the variable is not found. -}
lookupIdentifierType :: TypeCheckState a -> String -> Maybe DataType
lookupIdentifierType TypeCheckState {checkstate_varmap = x} s = Map.lookup s x

{- Lookup a type from the state. -}
lookupStructure :: (Show annot) => TypeCheckState annot -> annot -> String -> Either (String, annot) BaseType
lookupStructure state pos string =
    maybe (Left ("Undeclared type identifier: " ++ string, pos)) return
        (Map.lookup string (checkstate_typemap state))

mapDataType :: (Monad m) => (DataType -> m DataType) -> DataType -> m DataType
mapDataType fn dataType = do
    dt <- fn dataType
    case dt of
        (TypeVar a ds) -> TypeVar a <$> mapM (mapDataType fn) ds
        (DataType bt ds) -> DataType bt <$> mapM (mapDataType fn) ds
{- Try to match a list of arguments against an arrow type. If successful,
 - the remaining data type (return type when applied to such arguments)
 - is returned. Otherwise an error message is returned. -}
matchArrow :: [DataType] -> DataType -> Either String DataType
matchArrow [] dt = Right dt
matchArrow (t:ts) dt =
    case dt of
        DataType (BaseType "Arrow" _) [a, b] -> t `compat` a >> matchArrow ts b
        _ -> Left ("Expected arrow, got" ++ show dt ++ " probable cause, function applied to too many arguments.")

{- Given an arrow, unfold it into a list of data types. -}
unfoldArrow :: DataType -> [DataType]
unfoldArrow (DataType (BaseType "Arrow" (Kind 2)) [a, b]) = a : unfoldArrow b
unfoldArrow t = return t


{- Some instances for TypeCheckState. -}
instance (Show a) => Pretty (TypeCheckState a) where
    {- Mostly for debugging purposes. -}
    pretty (TypeCheckState mp mp1) =
        concatMap (\(k, v) -> k ++ " :: " ++ pretty v ++ "\n") (Map.toList mp) ++
        concatMap (\(k, BaseType _ (Kind v)) -> k ++ "<" ++ show v ++ ">") (Map.toList mp1)

{- A type check state is a monoid. The implementation is
 - to simply union  together the maps of the state. -}
instance Monoid (TypeCheckState annot) where
    mappend (TypeCheckState mp1 mp1') (TypeCheckState mp2 mp2') =
        TypeCheckState (mp1 `mappend` mp2) (mp1' `mappend` mp2')
    mempty = TypeCheckState mempty mempty

{- Pretty instances for the types. For better debugging purposes. -}
instance Pretty BaseType where
    pretty (BaseType name _) = name

instance Pretty DataType where
    pretty (DataType base vars) = pretty base ++ if null vars then "" else
                                    "(" ++ intercalate ", " (map pretty vars) ++ ")"

    pretty (TypeVar base vars) = "'" ++ base ++ if null vars then "" else
                                    "(" ++ intercalate ", " (map pretty vars) ++ ")"
