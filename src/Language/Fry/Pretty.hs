module Language.Fry.Pretty where

class Pretty a where
    pretty :: a -> String

prettyPrint :: (Pretty a) => a -> IO ()
prettyPrint = putStrLn . pretty
