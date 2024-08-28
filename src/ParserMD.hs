module ParserMD(parseMarkdown) where

import Utils
    ( Parser(..),
      PandocValue(..),
      PandocHeader(..),
      PandocContent(..),
      PandocFormat(..),
      PandocSection(..),
      parseChar,
      parseString,
      parseAnyChar,
      parseMany,
      parseSome,
      parseSpace, PandocContent )
import Data.Char
import Data.Maybe (fromMaybe)
import Control.Applicative ( Alternative((<|>), many), optional, some)
import Data.List (isPrefixOf)

-- split on \n for [] in code block

parseDelimiter :: String -> Parser String
parseDelimiter delim = do
    _ <- parseString delim
    _ <- parseMany (parseChar '\n')
    return delim

parseUntil :: Parser String -> Parser String
parseUntil p = Parser $ \input ->
    case runParser p input of
        Just (_, _) -> Just ("", input)
        Nothing -> case input of
            [] -> Nothing
            (x:xs) ->
                let res = runParser (parseUntil p) xs
                in fmap (\(a, rest) -> (x:a, rest)) res

parseKeyValue :: String -> Parser (Maybe String)
parseKeyValue key = do
    found <- optional $ do
        _ <- parseString key
        _ <- parseSpace
        _ <- parseChar ':'
        _ <- parseSpace
        parseUntilNewLine
    return found

parseUntilNewLine :: Parser String
parseUntilNewLine = parseMany (parseNotChar '\n') <* parseChar '\n'

parseNotChar :: Char -> Parser Char
parseNotChar c = Parser $ \input -> case input of
    (x:xs) -> if x /= c then Just (x, xs) else Nothing
    [] -> Nothing

parseHeader :: Parser PandocHeader
parseHeader = do
    _ <- parseDelimiter "---"
    title <- parseKeyValue "title"
    author <- parseKeyValue "author"
    date <- parseKeyValue "date"
    _ <- parseDelimiter "---"
    return $ PandocHeader (fromMaybe "" title) author date

parseCodeBlock :: Parser PandocContent
parseCodeBlock = do
    _ <- parseString "```\n"
    code <- some parseParagraph
    _ <- parseString "\n```"
    return $ ContentCodeBlock code


parseInlineCode :: Parser PandocContent
parseInlineCode = do
    _ <- parseChar '`'
    code <- parseMany (parseNotChar '`')
    _ <- parseChar '`'
    pure $ ContentFormatting(Code, code)

parseAnyContent :: Parser PandocContent
parseAnyContent =
    parseCodeBlock <|> parseInlineCode <|>
    parseBoldOrItalic <|> parseLink <|>
    parseImage <|> parseText

parseListItem :: Parser PandocContent
parseListItem = do
    _ <- parseChar '-'
    _ <- parseSpace
    content <- parseParagraph
    return content

parseList :: Parser PandocContent
parseList = ContentList <$> some (parseListItem <* parseChar '\n')

parseBold :: Parser PandocContent
parseBold = do
    _ <- parseString "**"
    content <- parseSome (parseNotChar '*')
    _ <- parseString "**"
    pure $ ContentFormatting (Bold, content)

parseItalic :: Parser PandocContent
parseItalic = do
    _ <- parseString "*"
    content <- parseSome (parseNotChar '*')
    _ <- parseString "*"
    pure $ ContentFormatting (Italic, content)

parseBoldOrItalic :: Parser PandocContent
parseBoldOrItalic = parseBold <|> parseItalic

parseLink :: Parser PandocContent
parseLink = do
    _ <- parseChar '['
    linkText <- parseSome (parseNotChar ']')
    _ <- parseString "]("
    url <- parseSome (parseNotChar ')')
    _ <- parseChar ')'
    return $ ContentLink (url, linkText)

parseImage :: Parser PandocContent
parseImage = do
    _ <- parseString "!["
    altText <- parseSome (parseNotChar ']')
    _ <- parseString "]("
    url <- parseSome (parseNotChar ')')
    _ <- parseChar ')'
    pure $ ContentImage (url, altText)

collectText :: String -> ([Char], String)
collectText input = collect input []
  where
    stopAt = ["[", "!", "`", "*", "**", "\n"]
    collect [] acc = (reverse acc, [])
    collect xs acc
        | any (`isPrefixOf` xs) stopAt = (reverse acc, xs)
        | otherwise = collect (tail xs) (head xs : acc)

parseText :: Parser PandocContent
parseText = Parser $ \input ->
    let (collectedText, remainingInput) = collectText input
    in if null collectedText
       then Nothing
       else Just (ContentText collectedText, remainingInput)

parseHeaderLevel :: Parser (Int, String)
parseHeaderLevel = do
    hashes <- some (parseChar '#')
    _ <- parseSpace
    title <- parseUntilNewLine
    _ <- parseSpace
    return (length hashes, title)

parseSection :: Int -> Parser PandocContent
parseSection curLevel = do
    (level, title) <- parseHeaderLevel
    content <- parseContent level
    if level - curLevel > 1
        then return (ContentSection (PandocSection (Just "")
            [(ContentSection (PandocSection (Just title) content))]))
    else return (ContentSection (PandocSection (Just title) content))

parseParagraph :: Parser PandocContent
parseParagraph = do
    content <- some parseAnyContent
    return $ ContentParagraph content

parseContent ::Int -> Parser [PandocContent]
parseContent level = many (p <* many (parseChar '\n'))
    where
        p =
            parseSection level <|> parseList <|>
            parseCodeBlock <|> parseParagraph <|> parseInlineCode <|>
            parseBoldOrItalic <|> parseLink <|>
            parseImage <|> parseText

parseMarkdown :: Parser PandocValue
parseMarkdown = do
    header <- parseHeader
    content <- parseContent 0
    return $ PandocValue header content
