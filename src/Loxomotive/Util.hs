module Loxomotive.Util where

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start) . drop start
