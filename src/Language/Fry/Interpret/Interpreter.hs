module Language.Fry.Interpret.Interpreter where

import Language.Fry.AST
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as M
import Data.List

data Value =
        Func ([Value] -> FryState -> IO ()) |
        LiteralString String |
        List [Value]  |
        Integer Int

instance Show Value where
    show (Func _) = "fn"
    show (LiteralString str) = str
    show (List vals) = "[" ++ show vals ++ "]"
    show (Integer i) = show i

data FryState = FryState (Map String Value)

singletonMap :: Map String Value
singletonMap = M.fromList [("println", Func (\vals _ -> putStrLn $ unwords $ map show vals))]

interpret :: (Show a) => Package a -> IO ()
interpret (Package _ stmts a) = do
    st <- foldM interpretStatement (FryState singletonMap) stmts
    void $ interpretExpr st (Call (ExprIdentifier "main" a) [] a)

    where
        interpretStatement :: (Show a) => FryState -> Statement a -> IO FryState
        interpretStatement state@(FryState mp) stmt = case stmt of
            (Function _ name idents _ body _) ->
                return $ FryState $
                    M.insert name (Func $ \vals st ->
                            let st' = foldl (\(FryState mp) (n, v) -> FryState $ M.insert n v mp) st $
                                        zipWith (\(TypedIdentifier n _ _) v -> (n, v)) idents vals
                                        in
                            foldM_ interpretStatement st' body
                        ) mp

            (StmtExpr expr _) -> interpretExpr state expr

        evalExpr :: Expression a -> Value
        evalExpr e = case e of
            StringLiteral str _ -> LiteralString str
            _ -> undefined

        interpretExpr :: (Show a) => FryState -> Expression a -> IO FryState
        interpretExpr state@(FryState mp) expr =
            case expr of
                (ExprIdentifier _ _) -> return state

                (ExprNumber _ _) -> return state

                (BinOp ">>" lhs rhs _) -> do
                    state' <- interpretExpr state lhs
                    interpretExpr state' rhs

                (Call (ExprIdentifier x annot) args _) ->
                    case M.lookup x mp of
                        Nothing -> error (show annot ++ ": " ++ x ++ ": no such function")
                        Just v -> case v of
                                    Func fn -> fn (map evalExpr args) state >> return state
                                    _ -> error(show annot ++ ": " ++ x ++ ": is not a function!")
                _ -> undefined

