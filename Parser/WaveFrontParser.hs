module WaveFrontParser where

import AParser
import Control.Applicative
import Data.Char
import Data.Foldable
import Data.Maybe (catMaybes)
import Debug.Trace

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (oneOrMore p) <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> (zeroOrMore p)

rd = read :: String -> Double
digit = satisfy isDigit

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

parsePosIntegerStr = oneOrMore digit
parseAnyIntegerStr = parsePosIntegerStr <|> (:) <$> char '-' <*> parsePosIntegerStr

parsePosDoubleStr = (mappend <$> (mappend <$> parsePosIntegerStr <*> charStr '.')
                      <*> parsePosIntegerStr)
                    <|> parsePosIntegerStr

parseAnyDouble = rd <$> (parsePosDoubleStr <|>
                         ((:) <$> char '-' <*> parsePosDoubleStr))

opt = pure Nothing

optionalDouble = (parseAnyDouble <|> pure 0.0 )

maybeDouble = (parseAnyDouble >>= (\a -> return (Just a)))
              <|> opt


------------------------------------------------------------
--  3. Parsing Wavefront OBJ files
------------------------------------------------------------

data FaceElement = FaceElement (Maybe Double) (Maybe Double) (Maybe Double) deriving (Show, Read)

data Element = Vertex Double Double Double Double
             | TC Double Double Double
             | VNormal Double Double Double
             | Face FaceElement FaceElement FaceElement deriving (Show, Read)

parseVertex = Vertex <$> optionalDouble <* spaces
              <*> optionalDouble <* spaces
              <*> optionalDouble <* spaces
              <*> optionalDouble

parseTC = TC <$> optionalDouble <* spaces
          <*> optionalDouble <* spaces
          <*> optionalDouble

parseVNormal = VNormal <$> optionalDouble <* spaces
               <*> optionalDouble <* spaces
               <*> optionalDouble <* spaces

parseFaceElement = FaceElement <$> maybeDouble
                   <*> ((char '/') *> maybeDouble <|> opt)
                   <*> ((char '/') *> maybeDouble <|> opt)

parseFace = Face <$> parseFaceElement <* spaces <*> parseFaceElement <* spaces <*> parseFaceElement

parseVLine = char 'v' *> spaces *> parseVertex
parseTCLine = char 'v' *> char 't' *> spaces *> parseTC
parseVNormalLine = char 'v' *> char 'n' *> spaces *> parseVNormal
parseFaceLine = char 'f' *> spaces *> parseFace


parseLine :: Parser Element
parseLine = parseTCLine
            <|> parseVNormalLine
            <|> parseVLine
            <|> parseFaceLine


printLine :: (Element, b) -> IO ()
printLine = putStrLn . show . fst

getElement :: (Element, b) -> String
getElement = show . fst

elementsToString :: [(Element, a)] -> String
elementsToString = foldr (\e a -> (getElement e) ++ "\n" ++ a) ""

parseFile :: FilePath -> IO ()
parseFile path = do
  ch <- readFile path
  readFile path
  let fLines = lines ch
      pls = catMaybes $ (fmap (\l -> runParser parseLine l) fLines)
  return pls
    >>= return . elementsToString
    >>= writeFile "output.txt"
