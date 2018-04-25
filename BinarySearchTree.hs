module BinarySearchTree where

import BinaryTree

contains:: (Eq a, Ord a) => (BTree a) -> a -> Bool
contains BNull _ = False
contains (BNode a lt rt) searchTerm
  | (searchTerm == a) = True
  | (searchTerm > a) = (contains rt searchTerm)
  | (searchTerm < a) = (contains lt searchTerm)

insert :: (Eq a, Ord a) => (BTree a) -> a -> (BTree a)
insert BNull elem = (BNode elem BNull BNull)
insert bt@(BNode a lt rt) elem
  | (a > elem) = (BNode a (insert lt elem) rt)
  | (a < elem) = (BNode a lt (insert rt elem))
  | (a == elem) =  bt

delete :: (Eq a, Ord a) => (BTree a) -> a -> (BTree a)
delete BNull elem = BNull
delete bt@(BNode a lt rt) elem
  | (a == elem) = deleteRootNode bt
  | (a > elem) = BNode a lt (delete rt elem)
  | (a < elem) = BNode a (delete lt elem) rt

deleteRootNode :: (Eq a, Ord a) => (BTree a) -> (BTree a)
deleteRootNode (BNode a BNull r) = r
deleteRootNode (BNode a l BNull) = l
deleteRootNode (BNode a l r) = BNode leftmost l r
  where leftmost = leftMostElement l


leftMostElement :: (Eq a, Ord a) => (BTree a) -> a
leftMostElement (BNode a BNull _) = a
leftMostElement (BNode a l _) = leftMostElement l



