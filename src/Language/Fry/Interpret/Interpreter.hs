module Language.Fry.Interpret.Interpreter where

import Language.Fry.AST
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as M

data Value =
        Func ([Value] -> FryState -> IO ())

data FryState = FryState (Map String Value)

singletonMap :: Map String Value
singletonMap = M.fromList [("println", Func (\_ _ -> putStrLn "!!println!!"))]

interpret :: (Show a) => Package a -> IO ()
interpret (Package _ stmts a) = do
    st <- foldM interpretStatement (FryState singletonMap) stmts
    void $ interpretExpr st (Call (ExprIdentifier "main" a) [] a)

    where
        interpretStatement :: (Show a) => FryState -> Statement a -> IO FryState
        interpretStatement state@(FryState mp) stmt = case stmt of
            (Function _ name _ _ body _) ->
                return $ FryState $
                    M.insert name (Func $ \_ st -> foldM_ interpretStatement st body) mp
            (StmtExpr expr _) -> interpretExpr state expr

        interpretExpr :: (Show a) => FryState -> Expression a -> IO FryState
        interpretExpr state@(FryState mp) expr =
            case expr of
                (ExprIdentifier _ _) -> return state
                (ExprNumber _ _) -> return state
                (BinOp ">>" lhs rhs _) -> do
                    state' <- interpretExpr state lhs
                    interpretExpr state' rhs
                (Call (ExprIdentifier x annot) _ _) ->
                    case M.lookup x mp of
                        Nothing -> error (show annot ++ ": " ++ x ++ ": no such function")
                        Just v -> case v of
                                    Func fn -> fn [] state >> return state
                _ -> undefined

