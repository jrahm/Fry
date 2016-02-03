
module Language.Fry.Typecheck.Builtins where

import Language.Fry.Typecheck.Util
import qualified Data.Map as Map

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

initialState :: TypeCheckState annot
initialState = TypeCheckState
                Map.empty $
                Map.fromList [
                      ("Unit",   BaseType "Unit" (Kind 0))
                    , ("Int",    BaseType "Int" (Kind 0))
                    , ("List",   BaseType "List" (Kind 1))
                    , ("String", BaseType "String" (Kind 0))
                    , ("Arrow",  BaseType "Arrow" (Kind 2))
                ]
