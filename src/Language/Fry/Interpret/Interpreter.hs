module Language.Fry.Interpret.Interpreter where

import Language.Fry.AST
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as M
import Data.List

data Value =
        Func ([Value] -> FryState -> IO Value) |
        LiteralString String |
        List [Value]  |
        Integer Int   |
        Unit

instance Show Value where
    show (Func _) = "fn"
    show (LiteralString str) = str
    show (List vals) = "[" ++ show vals ++ "]"
    show (Integer i) = show i
    show Unit = "unit"

data FryState = FryState (Map String Value)

singletonMap :: Map String Value
singletonMap = M.fromList [("println", Func (\vals _ -> (putStrLn $ unwords $ map show vals) >> return Unit))]

interpret :: (Show a) => Package a -> IO ()
interpret (Package _ stmts a) = do
    st <- foldM interpretStatement (FryState singletonMap) stmts
    void $ evalExpr st (Call (ExprIdentifier "main" a) [] a)

    where
        interpretStatement :: (Show a) => FryState -> Statement a -> IO FryState
        interpretStatement state@(FryState mp) stmt = case stmt of
            (Function _ name idents _ body _) ->
                return $ FryState $
                    M.insert name (Func $ \vals st ->
                            let st' = foldl (\(FryState mp) (n, v) -> FryState $ M.insert n v mp) st $
                                        zipWith (\(TypedIdentifier n _ _) v -> (n, v)) idents vals
                                        in
                            foldM_ interpretStatement st' body >>
                            return Unit
                        ) mp

            (StmtExpr expr _) -> evalExpr state expr >> return state

        evalExpr :: (Show a) => FryState -> Expression a -> IO Value
        evalExpr state@(FryState mp) e = case e of
            StringLiteral str _ -> return $ LiteralString str

            ExprIdentifier id' _ ->
                case M.lookup id' mp of
                    Nothing -> error ("Undefined variable: " ++ id')
                    Just x -> return x

            (Call (ExprIdentifier x annot) args _) ->
                case M.lookup x mp of
                    Nothing -> error (show annot ++ ": " ++ x ++ ": no such function")
                    Just v -> case v of
                                Func fn -> do
                                    args' <- mapM (evalExpr state) args
                                    fn args' state
                                _ -> error(show annot ++ ": " ++ x ++ ": is not a function!")

            _ -> undefined

