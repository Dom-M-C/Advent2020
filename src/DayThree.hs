module DayThree where

import Control.Applicative
import Data.Tuple
import Data.Char

import Data.Map(Map)
import qualified Data.Map as Map

type Position = (Int, Int)
type Velocity = (Int, Int)


sledMove :: Position -> Velocity -> Position
sledMove (pX, pY) (vX, vY) = (pX + vX, pY + vY)

initialMove :: Velocity -> Position
initialMove = sledMove (0, 0)

sledPath :: Position -> Velocity -> [Position]
sledPath pos vel = pos : sledPath (sledMove pos vel) vel

testInput :: [String]
testInput = 
    [   "..##......."
    ,   "#...#...#.."
    ,   ".#....#..#."
    ,   "..#.#...#.#"
    ,   ".#...##..#."
    ,   "..#.##....."
    ,   ".#.#.#....#"
    ,   ".#........#"
    ,   "#.##...#..."
    ,   "#...##....#"
    ,   ".#..#...#.#"
    ]

--treeMap :: [String] -> Map Position 


data JsonValue 
    = JsonNull
    | JsonBool Bool
    -- | JsonNumber Double
    | JsonNumber Int
    | JsonString String
    | JsonArray [JsonValue]
    -- | JsonObject (Map String JsonValue)
    | JsonObject [(String, JsonValue)]
    deriving (Show, Eq)

newtype Parser a = Parser
    {   runParser :: String -> Maybe (String, a)
    }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (mayInput, x) <- p input
        Just (mayInput, f x)

instance Applicative Parser where
    pure parser = Parser $ \input -> Just (input, parser)
    (Parser pOne) <*> (Parser pTwo) = Parser $ \input -> do
        (outputOne, f) <- pOne input 
        (outputTwo, a) <- pTwo outputOne
        Just (outputTwo, f a)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser parserOne) <|> (Parser parserTwo) = Parser $ \input -> do
        parserOne input <|> parserTwo input

parseChar :: Char -> Parser Char
parseChar char = Parser $ \case
    headChar:str | headChar == char -> Just(str, char)
    _ -> Nothing

parseString :: String -> Parser String
parseString = traverse parseChar 

parseSpan :: (Char -> Bool) -> Parser String
parseSpan predicate = Parser $ Just . swap . span predicate

parseFloat' :: Parser Float
parseFloat' = Parser $ \case
    ""      -> Nothing
    '.':xs  -> runParser parseFloat' $ "0." <> xs
    input   ->
        let (ints, deciWithPoint) = span isDigit input
            (point, deciWithRest) = span ('.'==) deciWithPoint
            (deci, rest) = span isDigit deciWithRest
            (resultRest, resultFloat)
                | null deci = (point <> rest, ints)
                | otherwise = (rest, ints <> point <> deci)
        in
            Just (resultRest, read resultFloat)

parseDigit :: Parser String
parseDigit = parseSpan isDigit

parseInt :: Parser Int
parseInt = read <$> parseDigit


-- parseFloat :: Parser Float
-- parseFloat = Parser $ \input -> do
--     parseDecimal <- parseDigit *> parseChar '.' <* parseDigit
--     parseInt <- parseDigit
--     return parseDecimal <|> parseInt

-- f = do 
--     let deciParse = parseDigit <* parseChar '.'
--     let intParse = parseDigit
--     deciParse <|> intParse

-- f' = do 
--     (rest, deciParse) <- parseDigit <* parseChar '.'
--     Parser rest

notNull :: Foldable t => Parser (t a) -> Parser (t a)
notNull (Parser p) = Parser $ \input -> do
    (result, xs) <- p input
    if null xs
        then Nothing
        else Just (result, xs)

stringLiteral :: Parser String
stringLiteral = parseChar '"' *> parseSpan (/= '"') <* parseChar '"'

parseWhitespace :: Parser String
parseWhitespace = parseSpan isSpace 

separatedBy :: Parser a -> Parser b -> Parser [b]
separatedBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonNumber :: Parser JsonValue
jsonNumber = parseResult <$> notNull parseDigit
    where
        parseResult xs = JsonNumber $ read xs


jsonString :: Parser JsonValue
jsonString = JsonString <$> (parseChar '"' *> stringLiteral <* parseChar '"')

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ parseString "null"

jsonBool :: Parser JsonValue
jsonBool = JsonBool <$> (jTrue <|> jFalse)
    where
        jTrue = True <$ parseString "true"
        jFalse = False <$ parseString "false"

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (parseChar '[' *> elements <* parseChar ']')
    where
        commaOrWhitespace = parseWhitespace *> parseChar ',' <* parseWhitespace
        elements = separatedBy commaOrWhitespace jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (parseWhitespace *>  parseChar '{' 
    *> parseWhitespace *> 
    separatedBy (parseWhitespace *> parseChar ',' <* parseWhitespace) kvPairs 
    <* parseWhitespace <* 
    parseChar '}')
    where
        kvPairs = 
            (\key _ value -> (key, value)) <$> stringLiteral 
                <*> (parseWhitespace *> parseChar ':' <* parseWhitespace) 
                <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

--https://www.youtube.com/watch?v=N9RUqGYuGfw -> 1:42:30



