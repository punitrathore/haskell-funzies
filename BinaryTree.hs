{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module BinaryTree where

data (Eq a, Ord a) => BTree a = BNode a (BTree a) (BTree a) | BNull deriving (Show)

-- empty :: (BTree a) -> Bool
empty BNull = True
empty _ = False

-- inorder :: (BTree a) -> [a]
inorder BNull = []
inorder (BNode a lt rt) =  (inorder lt) ++ [a] ++ (inorder rt)

-- preorder :: (BTree a) -> [a]
preorder BNull = []
preorder (BNode a lt rt) = [a] ++ (preorder lt) ++ (preorder rt)

-- postorder :: (BTree a) -> [a]
postorder BNull = []
postorder (BNode a lt rt) = (postorder lt) ++ (postorder rt) ++ [a]

bt3 = (BNode 4 BNull BNull)
bt4 = (BNode 5 BNull BNull)
bt5 = (BNode 3 BNull BNull)

bt1 = (BNode 2 bt3 bt4)
bt2 = (BNode 3 BNull BNull)
bt = (BNode 1 bt1 bt2)
