module ParserXML(parseXml) where

import Utils
    ( Parser(..),
      PandocValue(..),
      PandocHeader(..),
      PandocContent(..),
      PandocFormat(..),
      PandocSection(..),
      parseString,
      parseAnyChar,
      parseSpace, PandocContent )
import Control.Applicative ( Alternative((<|>), many, some) )


parseXmlString :: Parser String
parseXmlString = do
    some (parseAnyChar (filter (/= '<') ['\0'..'\255']))

parsetTitle :: Parser String
parsetTitle = many (parseAnyChar (filter (/= '\"') ['\0'..'\255']))

parseBalise :: String -> String -> Parser a -> Parser (String, a)
parseBalise name key parser = do
    _ <- parseSpace
    _ <- parseString ("<" ++ name)
    _ <- parseSpace
    _ <- parseString (key ++ "=\"")
    res <- parsetTitle
    _ <- parseString "\">"
    cont <- parser
    _ <- parseString ("</" ++ name ++ ">")
    return (res, cont)

parseObjectValue :: String -> Parser String
parseObjectValue key = do
    _ <- parseSpace
    _ <- parseString ("<" ++ key ++">")
    _ <- parseSpace
    res <- parseXmlString
    _ <- parseSpace
    _ <- parseString ("</" ++ key ++">")
    return res

parseAuthorAndDate :: Parser (Maybe String, Maybe String)
parseAuthorAndDate =
    ((\author date -> (Just author, Just date)) <$>
        parseObjectValue "author" <*> parseObjectValue "date")
    <|> ((\date author -> (Just author, Just date)) <$>
        parseObjectValue "date" <*> parseObjectValue "author")
    <|> (parseObjectValue "author" >>=
        \author -> return (Just author, Nothing))
    <|> (parseObjectValue "date" >>=
        \date -> return (Nothing, Just date))
    <|> return (Nothing, Nothing)

parseXmlHeader :: Parser PandocHeader
parseXmlHeader = do
    (title, (author, date)) <- parseBalise "header" "title" parseAuthorAndDate
    return (PandocHeader title author date)

parseXmlText :: Parser PandocContent
parseXmlText = ContentText <$> parseXmlString

parseXmlFormatting :: Parser PandocContent
parseXmlFormatting = do
    _ <- parseSpace
    (parseObjectValue "bold" >>=
            \value -> return (ContentFormatting (Bold, value)))
        <|> (parseObjectValue "italic" >>=
            \value -> return (ContentFormatting (Italic, value)))
        <|> (parseObjectValue "code" >>=
            \value -> return (ContentFormatting (Code, value)))

parseXmlLink :: Parser PandocContent
parseXmlLink = do
    _ <- parseSpace
    (url, urlcontent) <- parseBalise "link" "url" parseXmlString
    return (ContentLink (url, urlcontent))

parseXmlImage :: Parser PandocContent
parseXmlImage = do
    _ <- parseSpace
    (url, urlcontent) <- parseBalise "image" "url" parseXmlString
    return (ContentImage (url, urlcontent))

parseXmlList :: Parser PandocContent
parseXmlList = do
    _ <- parseSpace
    _ <- parseString "<list>"
    _ <- parseSpace
    res <- many parseXmlContent
    _ <- parseSpace
    _ <- parseString "</list>"
    return (ContentList res)

parseXmlCodeBlock :: Parser PandocContent
parseXmlCodeBlock = do
    _ <- parseSpace
    _ <- parseString "<codeblock>"
    _ <- parseSpace
    res <- many parseXmlContent
    _ <- parseSpace
    _ <- parseString "</codeblock>"
    return (ContentCodeBlock res)

parseXmlContent :: Parser PandocContent
parseXmlContent = parseXmlFormatting
               <|> parseXmlLink
               <|> parseXmlImage
               <|> parseXmlParagraph
               <|> parseXmlList
               <|> parseXmlCodeBlock
               <|> parseXmlSection
               <|> parseXmlText

parseXmlParagraph :: Parser PandocContent
parseXmlParagraph = do
    _ <- parseSpace
    _ <- parseString "<paragraph>"
    res <- many parseXmlContent
    _ <- parseSpace
    _ <- parseString "</paragraph>"
    return (ContentParagraph res)

parseXmlSection :: Parser PandocContent
parseXmlSection = do
    _ <- parseSpace
    (title, contents) <- parseBalise "section" "title" (many parseXmlContent)
    return (ContentSection (PandocSection (checkTitle title) contents))
        where checkTitle titl = case titl of
                [] -> Nothing
                oooo -> Just oooo

parseXmlBody :: Parser [PandocContent]
parseXmlBody = do
    _ <- parseSpace
    _ <- parseString "<body>"
    _ <- parseSpace
    elements <- many parseXmlContent
    _ <- parseSpace
    _ <- parseString "</body>"
    return elements

parseXml :: Parser PandocValue
parseXml = do
    _ <- parseString "<document>"
    _ <- parseSpace
    xmlHeader <- parseXmlHeader
    _ <- parseSpace
    xmlBody <- parseXmlBody
    _ <- parseSpace
    _ <- parseString "</document>"
    return (PandocValue xmlHeader xmlBody)
