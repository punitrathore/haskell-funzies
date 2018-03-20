module Permutations where

import Data.List (delete)

permutation [] = [[]]
permutation xs = [x:ys | x <- xs, ys <- permutation (delete x xs)]
