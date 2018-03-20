-- Implementing the Set Game. Rules are listed here : https://www.setgame.com/sites/default/files/instructions/SET%20INSTRUCTIONS%20-%20ENGLISH.pdf --

module SetGame where

import System.Random
import Control.Applicative
import Data.List (tails)

data Color = Red | Green | Purple deriving (Eq, Show, Enum)
data Pattern = Solid | Striped | Outlined deriving (Eq, Show, Enum)
data Number = One | Two | Three deriving (Eq, Show, Enum)
data Shape = Oval | Squiggle | Diamond deriving (Eq, Show, Enum)

data Card = Card {cardColor :: Color,
                  cardPattern :: Pattern,
                  cardNumber :: Number,
                  cardShape :: Shape}
          deriving (Eq, Show)

isSetProp f c1 c2 c3 =
  let p1 = f c1
      p2 = f c2
      p3 = f c3
  in
  if(((p1 == p2) && (p1 == p3) && (p2 == p3)) || ((p1 /= p2) && (p2 /= p3) && (p1 /= p3)))
    then True
  else False

isSetColor = isSetProp cardColor
isSetShape = isSetProp cardShape
isSetNumber = isSetProp cardNumber
isSetPattern = isSetProp cardPattern

computeSetLogic f g h i c1 c2 c3 = (f c1 c2 c3) && (g c1 c2 c3) && (h c1 c2 c3) && (i c1 c2 c3)

isSetTriplet = computeSetLogic isSetColor isSetPattern isSetShape isSetNumber

allCards :: [Card]
allCards = [Card c p n s
           | c <- [Red ..],
             p <- [Solid ..],
             n <- [One ..],
             s <- [Oval ..]]

-- got this function from here : https://stackoverflow.com/questions/14692059/how-to-shuffle-a-list-in-haskell
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle lst = do
    (e, rest) <- pickElem <$> getIx
    (e:) <$> shuffle rest
    where
    getIx = getStdRandom $ randomR (1, length lst)
    pickElem n = case splitAt n lst of
        ([], s) -> error $ "failed at index " ++ show n -- should never match
        (r, s)  -> (last r, init r ++ s)

findAllSetTriplets :: [Card] -> [(Card, Card, Card)]
findAllSetTriplets cs = [(a, b, c)
                        | (a:as) <- tails cs,
                          (b:bs) <- tails as,
                          c <- bs,
                          (isSetTriplet a b c)]

random12 = (take 12) <$> shuffle allCards

playGame :: IO([Card]) -> IO ([(Card, Card, Card)])
playGame = fmap findAllSetTriplets
