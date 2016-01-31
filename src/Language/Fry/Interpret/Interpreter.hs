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

interpret :: Package -> IO ()
interpret (Package _ stmts) = do
    st <- foldM interpretStatement (FryState singletonMap) stmts
    void $ interpretExpr st (Call (ExprIdentifier "main"))

    where
        interpretStatement :: FryState -> Statement -> IO FryState
        interpretStatement state@(FryState mp) stmt = case stmt of
            (Function name body) ->
                return $ FryState $
                    M.insert name (Func $ \_ st -> foldM_ interpretStatement st body) mp
            (StmtExpr expr) -> interpretExpr state expr

        interpretExpr :: FryState -> Expression -> IO FryState
        interpretExpr state@(FryState mp) expr =
            case expr of
                (ExprIdentifier _) -> return state
                (ExprNumber _) -> return state
                (BinOp ">>" lhs rhs) -> do
                    state' <- interpretExpr state lhs
                    interpretExpr state' rhs
                (Call (ExprIdentifier x)) ->
                    case M.lookup x mp of
                        Nothing -> error (x ++ ": no such function")
                        Just v -> case v of
                                    Func fn -> fn [] state >> return state
                _ -> undefined

