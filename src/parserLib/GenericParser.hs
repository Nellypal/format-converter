module GenericPArser
    (Parser(..), parseChar, parseAnyChar, parseOr, parseAnd,
    parseAndWith, parseMany, parseSome, parseUInt, parseInt,
    parseTruple
    ) where

import Text.Read (readMaybe)

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap fct (Parser p) = Parser $ \input -> case p input of
        Just (x, rest) -> Just (fct x, rest)
        Nothing -> Nothing

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    (Parser pf) <*> (Parser px) = Parser $ \input ->
        case pf input of
            Just (f, rest1) ->
                case px rest1 of
                    Just (x, rest2) -> Just (f x, rest2)
                    Nothing -> Nothing
            Nothing -> Nothing

instance Alternative Parser where
  empty = Parser $ const Nothing
  (<|>) a b =
    Parser $ \input ->
        case runParser a input of
            Just result -> Just result
            Nothing -> runParser b input

instance Monad Parser where
    (Parser p) >>= f =
        Parser $ \input ->
            case p input of
                Just (a, rest) -> runParser (f a) rest
                Nothing -> Nothing

parseChar :: Char -> Parser Char
parseChar c = Parser $ \input -> case input of
    (x:xs) -> if x == c then Just (c, xs) else Nothing
    [] -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar c = Parser $ \input -> case input of
    (x:xs) -> if x `elem` c then Just (x, xs) else Nothing
    [] -> Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr (Parser p1) (Parser p2) = Parser $ \input ->
    case p1 input of
        Just result -> Just result
        Nothing -> p2 input

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd (Parser p1) (Parser p2) = Parser $ \input -> case p1 input of
    Just (result1, rest1) -> case p2 rest1 of
        Just (result2, rest2) -> Just ((result1, result2), rest2)
        Nothing -> Nothing
    Nothing -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f (Parser p1) (Parser p2) = Parser $ \input -> case p1 input of
    Just (result1, rest1) -> case p2 rest1 of
        Just (result2, rest2) -> Just (f result1 result2, rest2)
        Nothing -> Nothing
    Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany (Parser p) = Parser $ \input ->
    let parseManyHelper acc rest = case p rest of
            Just (result, newRest) -> parseManyHelper (result:acc) newRest
            Nothing -> Just (reverse acc, rest)
    in parseManyHelper [] input

parseSome :: Parser a -> Parser [a]
parseSome parser = Parser $ \input ->
    case runParser (parseAnd parser (parseMany parser)) input of
        Just ((first, rest), remaining) -> Just (first : rest, remaining)
        Nothing -> Nothing

parseUInt :: Parser Int
parseUInt = fmap read (parseSome (parseAnyChar ['0'..'9']))

parseInt :: Parser Int
parseInt = fmap readWithSign (parseSome (parseAnyChar ('-':['0'..'9'])))
  where
    readWithSign str = case readMaybe str of
        Just x -> x
        Nothing -> error "Invalid integer format"

parseDouble :: Parser Double
parseDouble = fmap readWithSign (parseSome (parseAnyChar ('-':'.':['0'..'9'])))
  where
    readWithSign str = case readMaybe str of
        Just x -> x
        Nothing -> error "Invalid integer format"

parseTruple :: Parser (Int , Int , Int)
parseTruple = do
    _ <- parseChar '('
    a <- parseInt
    _ <- parseChar ','
    b <- parseInt
    _ <- parseChar ','
    c <- parseInt
    _ <- parseChar ')'
    return (a,b,c)
