module Language.Fry.AST where

import Language.Fry.Pretty

data Package =
        Package {package_name :: String}
        deriving Show

instance Pretty Package where

    pretty (Package name) = "package " ++ name
