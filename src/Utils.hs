module Utils(Parser(..), PandocValue(..), PandocHeader(..), PandocContent(..), PandocFormat(..), PandocSection(..), parseChar, parseString, parseAnyChar, parseMany, parseSpace, parseSome) where

import Control.Applicative (Alternative(..))
import Data.Maybe (isJust)

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

data PandocHeader =
    PandocHeader {
        headerTitle :: String,
        headerAuthor :: Maybe String,
        headerDate :: Maybe String
    }
    deriving (Show)

data PandocFormat = Italic | Bold | Code deriving(Enum, Show)

data PandocSection =
    PandocSection {
        sectionTitle :: Maybe String,
        sectionContent :: [PandocContent]
    }
    deriving (Show)

data PandocContent
    = ContentText String
    | ContentFormatting (PandocFormat, String)
    | ContentLink (String, String) -- url, text
    | ContentImage (String, String) -- url, alt text
    | ContentParagraph [PandocContent]
    | ContentSection PandocSection
    | ContentCodeBlock [PandocContent]
    | ContentList [PandocContent]
    deriving (Show)

data PandocValue =
    PandocValue {
        header :: PandocHeader,
        content :: [PandocContent]
    }
    deriving (Show)

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

parseString :: String -> Parser String
parseString = mapM parseChar

parseAnyChar :: String -> Parser Char
parseAnyChar (x:xs) = Parser f
    where
        f (x':xs') =
            let res = runParser (parseChar x) (x':xs') in
                if isJust res
                    then res
                else runParser (parseAnyChar xs) (x':xs')
        f _ = Nothing
parseAnyChar _ = Parser $ const Nothing

parseMany :: Parser a -> Parser [a]
parseMany = many

parseSpace :: Parser [Char]
parseSpace = parseMany (parseAnyChar " \t\n")

parseSome :: Parser a -> Parser [a]
parseSome = some
