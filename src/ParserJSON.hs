module ParserJSON(parseJson) where

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
      parseSpace, PandocContent )
import Control.Applicative ( Alternative((<|>), many), optional )


parseJSONString :: Parser String
parseJSONString = do
    _ <- parseChar '\"'
    s <- parseMany (parseAnyChar (filter (/= '\"') ['\0'..'\255']))
    _ <- parseChar '\"'
    return s

parseObjectValue :: String -> Parser String
parseObjectValue key = do
    _ <- parseString key
    _ <- parseSpace
    _ <- parseChar ':'
    _ <- parseSpace
    res <- parseJSONString
    _ <- parseSpace
    _ <- parseMany (parseChar ',')
    _ <- parseSpace
    return res

parseAuthorAndDate :: Parser (Maybe String, Maybe String)
parseAuthorAndDate =
    ((\author date -> (Just author, Just date)) <$>
        parseObjectValue "\"author\"" <*> parseObjectValue "\"date\"")
    <|> ((\date author -> (Just author, Just date)) <$>
        parseObjectValue "\"date\"" <*> parseObjectValue "\"author\"")
    <|> (parseObjectValue "\"author\"" >>=
        \author -> return (Just author, Nothing))
    <|> (parseObjectValue "\"date\"" >>=
        \date -> return (Nothing, Just date))
    <|> return (Nothing, Nothing)

parseJsonHeaderOptional :: String -> Parser PandocHeader
parseJsonHeaderOptional title = do
    (author, date) <- parseSpace *> parseAuthorAndDate
    return (PandocHeader title author date)

parseJsonHeader :: Parser PandocHeader
parseJsonHeader = do
    _ <- parseString "\"header\"" *> parseSpace *> parseChar ':'
    _ <- parseSpace *> parseChar '{' *> parseSpace
    _ <- parseString "\"title\"" *> parseSpace *> parseChar ':'
    _ <- parseSpace
    title <- parseJSONString <* parseMany (parseChar ',') <* parseSpace
    res <- parseJsonHeaderOptional title
    _ <- parseSpace *> parseChar '}' *> parseSpace *> parseMany (parseChar ',')
    return res

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
    x <- p
    xs <- many (sep *> p)
    _ <- optional sep
    return (x:xs)

parseJsonText :: Parser PandocContent
parseJsonText = ContentText <$> parseJSONString

parseJsonFormatting :: Parser PandocContent
parseJsonFormatting = do
    _ <- parseChar '{' *> parseSpace
    res <- (parseObjectValue "\"bold\"" >>=
            \value -> return (ContentFormatting (Bold, value)))
        <|> (parseObjectValue "\"italic\"" >>=
            \value -> return (ContentFormatting (Italic, value)))
        <|> (parseObjectValue "\"code\"" >>=
            \value -> return (ContentFormatting (Code, value)))
    _ <- parseSpace *> parseChar '}'
    return res

parseJsonUrl :: String -> Parser String
parseJsonUrl k = do
    _ <- parseString k *> parseSpace *> parseChar ':' *> parseSpace *>
        parseChar '{' *> parseSpace
    parseObjectValue "\"url\"" <* parseSpace <*
        parseMany (parseChar ',') <* parseSpace

parseJsonUrlContent :: String -> Parser String
parseJsonUrlContent k = do
    _ <- parseString k *> parseSpace *> parseChar ':' *> parseSpace *>
        parseChar '[' *> parseSpace
    urlcontent <- parseJSONString
    _ <- parseSpace *> parseChar ']' *> parseSpace *> parseChar '}' *>
        parseSpace *> parseMany (parseChar ',') *> parseSpace
    return urlcontent

parseJsonLink :: Parser PandocContent
parseJsonLink = do
    _ <- parseChar '{' *> parseSpace
    url <- parseJsonUrl "\"link\""
    urlcontent <- parseJsonUrlContent "\"content\""
    _ <- parseChar '}'
    return (ContentLink (url, urlcontent))

parseJsonImage :: Parser PandocContent
parseJsonImage = do
    _ <- parseChar '{' *> parseSpace
    url <- parseJsonUrl "\"image\""
    urlcontent <- parseJsonUrlContent "\"alt\""
    _ <- parseChar '}'
    return (ContentImage (url, urlcontent))

parseJsonList :: Parser PandocContent
parseJsonList = do
    _ <- parseSpace *> parseChar '{' *> parseSpace *> parseString "\"list\""
    _ <- parseSpace *> parseChar ':' *> parseSpace
    _ <- parseChar '[' *> parseSpace
    elements <- sepBy parseJsonContent
        (parseSpace *> parseChar ',' <* parseSpace)
    _ <- parseSpace <* parseChar ']'
    _ <- parseSpace <* parseChar '}'
    return (ContentList elements)

parseJsonCodeBlock :: Parser PandocContent
parseJsonCodeBlock = do
    _ <- parseSpace *> parseChar '{' *> parseSpace
    _ <- parseString "\"codeblock\"" *> parseSpace *> parseChar ':'
    _ <- parseSpace *> parseChar '[' *> parseSpace
    elements <- sepBy parseJsonContent
        (parseSpace *> parseChar ',' <* parseSpace)
    _ <- parseSpace <* parseChar ']'
    _ <- parseSpace <* parseChar '}'
    return (ContentCodeBlock elements)


parseJsonContent :: Parser PandocContent
parseJsonContent = parseJsonText
               <|> parseJsonFormatting
               <|> parseJsonLink
               <|> parseJsonImage
               <|> parseJsonParagraph
               <|> parseJsonList
               <|> parseJsonCodeBlock
               <|> parseJsonSection

parseJsonParagraph :: Parser PandocContent
parseJsonParagraph = ContentParagraph <$> (parseChar '[' *> parseSpace *>
    elements <* parseSpace <* parseChar ']')
    where
        elements = sepBy parseJsonContent
            (parseSpace *> parseChar ',' <* parseSpace)

parseJsonSectionTittle :: Parser (Maybe String)
parseJsonSectionTittle =
    ((\title -> Just title) <$>
        parseObjectValue "\"title\"")
    <|> return Nothing

parseJsonSectionContent :: Parser [PandocContent]
parseJsonSectionContent = do
    _ <- parseSpace *> parseString "\"content\"" *> parseSpace *> parseChar ':'
    _ <- parseSpace *> parseChar '[' *> parseSpace
    elements <- sepBy parseJsonContent
        (parseSpace *> parseChar ',' <* parseSpace)
    _ <- parseSpace <* parseChar ']'
    return elements

parseJsonSection :: Parser PandocContent
parseJsonSection = do
    _ <- parseChar '{' *> parseSpace *> parseString "\"section\"" *> parseSpace
    _ <- parseChar ':' *> parseSpace *> parseChar '{' *> parseSpace
    title <- parseJsonSectionTittle
    contents <- parseJsonSectionContent
    _ <- parseSpace <* parseChar '}'
    _ <- parseSpace <* parseChar '}' <* parseSpace
    return (ContentSection (PandocSection title contents))

parseJsonBody :: Parser [PandocContent]
parseJsonBody = do
    _ <- parseSpace *> parseString "\"body\"" *> parseSpace *> parseChar ':'
    _ <- parseSpace *> parseChar '[' *> parseSpace
    elements <- sepBy parseJsonContent
        (parseSpace *> parseChar ',' <* parseSpace)
    _ <- parseSpace <* parseChar ']'
    return elements

parseJson :: Parser PandocValue
parseJson = do
    _ <- parseChar '{'
    _ <- parseSpace
    jsonHeader <- parseJsonHeader <* parseSpace
    jsonBody <- parseJsonBody <* parseSpace
    _ <- parseChar '}'
    return (PandocValue jsonHeader jsonBody)
