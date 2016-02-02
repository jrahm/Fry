{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Language.Fry.Typecheck.TypeCheck where

import Control.Monad.Writer

import Language.Fry.AST
import Language.Fry.Typecheck.Builtins
import Language.Fry.Typecheck.Internal
import Language.Fry.Typecheck.Util

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Char


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
        {- What is teh type of an expression? -}
        exprType state expr =
            case expr of
                {- The type of an identifier should be stored in the state.
                 - If it is not, then there is a problem and the identifier
                 - is undefined. -}
                ExprIdentifier id ann ->
                    toEither ("Undefined reference to " ++ id, ann)
                        (lookupIdentifierType state id)

                {- The type of a call is the return type of
                 - that function. However, the return type may
                 - be a type variable so we must have to deal with
                 - that. -}
                Call fn args ann -> do
                    functype <- exprType state fn
                    argsTypes' <- mapM (exprType state) args
                    let argTypes = argsTypes' `alter` [unitType]

                    mapLeft (,ann) $ matchArrow argTypes functype

                {- A number expression is an Integer. (No floats yet!)-}
                ExprNumber {} -> return intType

                {- A string is a string. -}
                StringLiteral _ _ -> return stringType

                {- A list literal tries to take the type of the first argument. -}
                ListLiteral exprs ann ->
                    if null exprs then
                        return $ listType (TypeVar "A" [])
                    else do
                        typ <- exprType state $ head exprs
                        mapM_ (exprType state >=>
                                mapLeft (,ann) . flip compat typ)
                                (tail exprs)
                        return $ listType typ
                _ -> undefined


        {- Tries to check the type soundness of a list of statements
         - given a current state. -}
        check state stmts =
            execWriter $ forM stmts $ tell . checkStatement
            where

            checkStatement s = case s of
                {- Check the type soundness of a function. -}
                Function _ name params _ body annot ->
                    case lookupIdentifierType state name of
                        Nothing ->
                            {- The identifier with the type could not be found -}
                            [("Unable to find type for: " ++ name, annot)]

                        Just t ->
                            let paramTypes = unfoldArrow t in
                            case params of
                                [] -> typecheck state body

                                _ | length paramTypes - 1 == length params ->
                                    {- Make a new state with the types of the parameters
                                     - given. -}
                                    let state' = mappend state $
                                         flip mconcatMap (zip paramTypes params) $
                                            \(typ, TypedIdentifier n _ _) ->
                                                TypeCheckState (Map.singleton n typ)
                                                                Map.empty
                                        in typecheck state' body

                                _ -> error "Param type discrepency **THIS IS A BUG**"

                StmtExpr expr annot ->
                    case exprType state expr of
                        Left err -> [err]
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
                {- The types of all the parameters of a function -}
                let paramtypes = map typedid_type params
                {- The return type, changed to contain unit if not defined.  -}
                let ret' = fromMaybe (ExprIdentifier "Unit" annot) ret

                {- This is where the checking is done to make sure all
                 - the type values are in fact valid -}
                fntypeexpr <- toArrowFn (paramtypes ++ [ret'])

                return $ TypeCheckState
                            (Map.singleton name fntypeexpr)
                             Map.empty

            _ -> return $ TypeCheckState Map.empty Map.empty

    where
        toArrowFn [expr] = toArrowFn' [ExprIdentifier "Unit" $ annotation expr, expr]
        toArrowFn xs = toArrowFn' xs

        toArrowFn' [expr] = toDataType state expr
        toArrowFn' (a:as) = arrowType <$> toDataType state a <*> toArrowFn' as

toDataType :: (Show annot) => TypeCheckState annot -> Expression annot -> Either (String, annot) DataType
toDataType state expr = case expr of
    (ExprIdentifier id' annot) ->
        if isLower (head id') then
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
        if isLower (head id') then
            {- In the function  we are type checking, this is
             - a type variable. -}
            TypeVar id' <$> mapM (toDataType state) args
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
                typeargs <- mapM (toDataType state) args
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
