module OutputJSON(displayJson, showParser) where

import Utils
    ( PandocValue(..),
      PandocHeader(..),
      PandocContent(..),
      PandocFormat(..),
      PandocSection(..),
      PandocContent(..) )

showParser :: Maybe (PandocValue, String) -> IO ()
showParser Nothing = putStrLn "Invalid format"
showParser (Just res) = putStrLn $ displayJson res

displayJsonHead :: PandocHeader -> String
displayJsonHead (PandocHeader title (Just author) (Just date)) =
    "\"header\":{" ++ "\"title\":\"" ++ title ++ "\",\"author\":\""
        ++ author ++ "\",\"date\":\"" ++ date ++ "\"}"
displayJsonHead (PandocHeader title (Just author) Nothing) =
    "\"header\":{" ++ "\"title\":\"" ++ title ++ "\",\"author\":\""
        ++ author ++ "\"}"
displayJsonHead (PandocHeader title Nothing (Just date)) =
    "\"header\":{" ++ "\"title\":\"" ++ title ++ "\",\"date\":\""
        ++ date ++ "\"}"
displayJsonHead (PandocHeader title Nothing Nothing) =
    "\"header\":{" ++ "\"title\":\"" ++ title ++ "\"}"

displayJsonFormatting :: PandocContent -> String
displayJsonFormatting (ContentFormatting (Italic, t)) =
    "{\"italic\":\"" ++ t ++ "\"}"
displayJsonFormatting (ContentFormatting (Bold, t)) =
    "{\"bold\":\"" ++ t ++ "\"}"
displayJsonFormatting (ContentFormatting (Code, t)) =
    "{\"code\":\"" ++ t ++ "\"}"
displayJsonFormatting _ = ""

displayJsonLink :: PandocContent -> String
displayJsonLink (ContentLink (link, link_content)) =
    "{\"link\":{\"url\":\"" ++ link ++
        "\",\"content\":[\"" ++ link_content ++ "\"]}}"
displayJsonLink _ = ""

displayJsonImage :: PandocContent -> String
displayJsonImage (ContentImage (link, alt)) =
    "{\"image\":{\"url\":\"" ++ link ++
        "\",\"alt\":[\"" ++ alt ++ "\"]}}"
displayJsonImage _ = ""

displayJsonParagraph :: PandocContent -> String
displayJsonParagraph (ContentParagraph paragraph_content) =
    "[" ++ displayJsonBodyContent paragraph_content "" ++ "]"
displayJsonParagraph _ = ""

displayJsonSection :: PandocSection -> String
displayJsonSection (PandocSection (Just title) section_content) =
    "{\"section\":{\"title\":\"" ++ title ++ "\",\"content\":[" ++
        displayJsonBodyContent section_content "" ++ "]}}"
displayJsonSection (PandocSection Nothing section_content) =
    "{\"section\":{\"content\":[" ++
        displayJsonBodyContent section_content "" ++ "]}}"

displayJsonCodeBlock :: PandocContent -> String
displayJsonCodeBlock (ContentCodeBlock code) =
    "{\"codeblock\":[" ++ displayJsonBodyContent code "" ++ "]}"
displayJsonCodeBlock _ = ""

displayJsonList :: PandocContent -> String
displayJsonList (ContentList list) =
    "{\"list\":[" ++ displayJsonBodyContent list "" ++ "]}"
displayJsonList _ = ""

displayJsonBodyContent :: [PandocContent] -> String -> String
displayJsonBodyContent [] res = res
displayJsonBodyContent ((ContentText text):xs) s =
    displayJsonBodyContent xs
        (s ++ "\"" ++ text ++ "\"" ++ commaIfNotEmpty xs)
displayJsonBodyContent ((ContentFormatting format):xs) s =
    displayJsonBodyContent xs
        (s ++ displayJsonFormatting (ContentFormatting format) ++
            commaIfNotEmpty xs)
displayJsonBodyContent ((ContentLink link):xs) s =
    displayJsonBodyContent xs
        (s ++ displayJsonLink (ContentLink link) ++ commaIfNotEmpty xs)
displayJsonBodyContent ((ContentImage image):xs) s =
    displayJsonBodyContent xs
    (s ++ displayJsonImage (ContentImage image) ++ commaIfNotEmpty xs)
displayJsonBodyContent ((ContentParagraph paragraph):xs) s =
    displayJsonBodyContent xs
        (s ++ displayJsonParagraph (ContentParagraph paragraph) ++
            commaIfNotEmpty xs)
displayJsonBodyContent ((ContentSection section):xs) s =
    displayJsonBodyContent xs
        (s ++ displayJsonSection section ++ commaIfNotEmpty xs)
displayJsonBodyContent ((ContentCodeBlock code):xs) s =
    displayJsonBodyContent xs
        (s ++ displayJsonCodeBlock (ContentCodeBlock code) ++
            commaIfNotEmpty xs)
displayJsonBodyContent ((ContentList list):xs) s =
    displayJsonBodyContent xs
        (s ++ displayJsonList (ContentList list) ++ commaIfNotEmpty xs)

displayJsonBody :: [PandocContent] -> String
displayJsonBody body_content = "\"body\":[" ++
    displayJsonBodyContent body_content "" ++ "]"

displayJson :: (PandocValue, String) -> String
displayJson (PandocValue pandocHead pandocBody, _) =
    "{" ++ displayJsonHead pandocHead ++ "," ++
        displayJsonBody pandocBody ++ "}"

commaIfNotEmpty :: [a] -> String
commaIfNotEmpty xs
    | null xs = ""
    | otherwise = ","
