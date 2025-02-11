{-|
Module      : List_add
Description : List manipulation utility functions.
-}
module List_add where

-- | updates an existing key in an association list
updateList :: Eq k => k -> a -> [(k, a)] -> [(k, a)]
updateList k v ((k', v') : kvs)
  | k' == k = (k, v) : kvs
  | otherwise = (k', v') : updateList k v kvs
updateList _ _ _ = []

-- | removes the first occurrence of a key in an association list
removeAt :: Eq k => k -> [(k, a)] -> [(k, a)]
removeAt k (el : kvs)
  | k == fst el = kvs
  | otherwise = el : removeAt k kvs
removeAt _ [] = []

-- | inserts an element at a specified index
insertAt :: Int -> a -> [a] -> [a]
insertAt i el xs =
  let (hd, tl) = splitAt i xs in hd ++ el : tl

-- | moves an element from one index to another
moveList :: (Eq a, Show a) => Int -> Int -> [a] -> [a]
moveList from to xs
  | from < to
  , let
      (us, vs) = splitAt from xs
      (as, bs) = splitAt (to - from + 1) vs =
      us ++ tail as ++ head as : bs
  | to < from
  , let
      (us, vs) = splitAt to xs
      (as, bs) = splitAt (from - to) vs =
      us ++ head bs : as ++ tail bs
  | otherwise = xs
