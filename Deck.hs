module Deck where

data Suit = Heart | Diamond | Club | Spade deriving (Enum, Show)

data Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Enum, Show)

data Card = CardData Face Suit deriving (Show)

deckOfCards = [CardData f s
              | f <- [Ace ..],
                s <- [Heart ..]]

pairs arr = [(x,y) | x <- arr, y <- (tail arr), (x + y) == 0]
