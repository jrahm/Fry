module Main where

import Language.Fry.Tokenizer
import Language.Fry.AST
import Language.Fry.ParsecHelp
import Language.Fry.Pretty

import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Text.Parsec

type Parser = Parsec [Token SourcePos] ()

parseModule :: Parser Package
parseModule = Package <$> (satisfyToken isPackage *> identifier <* eos)
    where isPackage (Token _ _ "package") = True
          isPackage _ = False

main :: IO ()
main = (>>=) getArgs $ \argv -> do
    ps <- tokenize (head argv) <$> Text.readFile (head argv)
    case ps of
        Left err -> print err
        Right tokens -> do
            mapM_ prettyPrint tokens
            let ps' = parse parseModule (head argv) tokens
            case ps' of
                Left err -> print err
                Right ast -> prettyPrint ast
