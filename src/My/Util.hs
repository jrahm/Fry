module My.Util where

import Control.Monad

mconcatMap :: (Monoid m) => (a -> m) -> [a] -> m
mconcatMap fn lst = mconcat (map fn lst)

mconcatMapM :: (Monad m, Monoid o) => (a -> m o) -> [a] -> m o
mconcatMapM fn lst = mconcat <$> mapM fn lst

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft fn eth = case eth of
    Right b -> Right b
    Left b -> Left (fn b)

toEither :: b -> Maybe a -> Either b a
toEither l Nothing = Left l
toEither _ (Just j) = Right j
