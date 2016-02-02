module My.Util where

import Control.Monad

mconcatMap :: (Monoid m) => (a -> m) -> [a] -> m
mconcatMap fn lst = mconcat (map fn lst)

mconcatMapM :: (Monad m, Monoid o) => (a -> m o) -> [a] -> m o
mconcatMapM fn lst = mconcat <$> mapM fn lst
