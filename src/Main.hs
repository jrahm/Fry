{-# LANGUAGE FlexibleContexts #-}
module Main where

import Debug.Trace
import Control.Monad
import Language.Fry.Tokenizer
import Language.Fry.AST
import Language.Fry.ParsecHelp
import Language.Fry.Pretty
import Language.Fry.Interpret.Interpreter

import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.List
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as M

import Text.Parsec
import Language.Fry.Parse.Parser

main :: IO ()
main = (>>=) getArgs $ \argv -> do
    ps <- tokenize (head argv) <$> Text.readFile (head argv)
    case ps of
        Left err -> print err
        Right tokens -> do
            mapM_ print tokens
            let ps' = runParser parsePackage (FryParseState operators) (head argv) tokens
            case ps' of
                Left err -> print err
                Right ast -> do
                    prettyPrint ast
                    interpret ast
