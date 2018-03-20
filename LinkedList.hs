module LinkedList where

data LList a = LList {getData:: a,
                      getNext:: (LList a)}
              | LNil
              deriving(Eq, Show)

instance Functor LList where
  fmap f LNil = LNil
  fmap f l = LList (f (getData l)) (fmap f (getNext l))

instance Monoid (LList a) where
  mempty = LNil
  mappend l1 LNil = l1
  mappend l1 (LList a ls) = mappend (LList a l1) ls

lHead :: LList a -> Maybe a
lHead LNil = Nothing
lHead (LList a _) = Just a

lTail :: LList a -> Maybe a
lTail LNil = Nothing
lTail (LList a LNil) = Just a
lTail (LList a ls) = lTail ls

lIndex :: (Num a, Eq a) => a -> LList b -> Maybe b
lIndex _ LNil = Nothing
lIndex 0 l = lHead l
lIndex i (LList a ls) = lIndex (i - 1) ls
