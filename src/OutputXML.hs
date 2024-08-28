module OutputXML
    (
        displayXML,
    ) where

import Utils
    ( PandocValue(..),
      PandocHeader(..),
      PandocContent(..),
      PandocFormat(..),
      PandocSection(..),
      PandocContent(..) )

displayXMLHead :: PandocHeader -> String
displayXMLHead (PandocHeader title (Just author) (Just date)) =
    "<header title=\"" ++ title ++"\">" ++
    "<author>" ++ author ++ "</author>" ++
    "<date>" ++ date ++ "</date>" ++
    "</header>"
displayXMLHead (PandocHeader title (Just author) Nothing) =
    "<header title=\"" ++ title ++"\">" ++
    "<author>" ++ author ++ "</author>" ++
    "</header>"
displayXMLHead (PandocHeader title Nothing (Just date)) =
    "<header title=\"" ++ title ++"\">" ++
    "<date>" ++ date ++ "</date>" ++
    "</header>"
displayXMLHead (PandocHeader title Nothing Nothing) =
    "<header title=\"" ++ title ++"\">" ++ "</header>"

displayXMLFormatting :: PandocContent -> String
displayXMLFormatting (ContentFormatting (Italic, t)) =
    "<italic>" ++ t ++ "</italic>"
displayXMLFormatting (ContentFormatting (Bold, t)) =
    "<bold>" ++ t ++ "</bold>"
displayXMLFormatting (ContentFormatting (Code, t)) =
    "<code>" ++ t ++ "</code>"
displayXMLFormatting _ = ""

displayXMLLink :: PandocContent -> String
displayXMLLink (ContentLink (link, link_content)) =
    "<link url=\"" ++ link ++
        "\">" ++ link_content ++ "</link>"
displayXMLLink _ = ""

displayXMLImage :: PandocContent -> String
displayXMLImage (ContentImage (link, alt)) =
    "<image url=\"" ++ link ++
        "\">" ++ alt ++ "</image>"
displayXMLImage _ = ""

displayXMLParagraph :: PandocContent -> String
displayXMLParagraph (ContentParagraph paragraph_content) =
    "<paragraph>" ++ displayXMLBodyContent paragraph_content "" ++
    "</paragraph>"
displayXMLParagraph _ = ""

displayXMLSection :: PandocSection -> String
displayXMLSection (PandocSection (Just title) section_content) =
    "<section title=\"" ++ title ++ "\">" ++
        displayXMLBodyContent section_content "" ++ "</section>"
displayXMLSection (PandocSection Nothing section_content) =
    "<section title=\"\">" ++
        displayXMLBodyContent section_content "" ++ "</section>"

displayXMLCodeBlock :: PandocContent -> String
displayXMLCodeBlock (ContentCodeBlock code) =
    "<codeblock>" ++ displayXMLBodyContent code "" ++ "</codeblock>"
displayXMLCodeBlock _ = ""

displayXMLList :: PandocContent -> String
displayXMLList (ContentList list) =
    "<list>" ++ displayXMLBodyContent list "" ++ "</list>"
displayXMLList _ = ""

displayXMLBodyContent :: [PandocContent] -> String -> String
displayXMLBodyContent [] res = res
displayXMLBodyContent ((ContentText text):xs) s =
    displayXMLBodyContent xs (s ++ text)
displayXMLBodyContent ((ContentFormatting format):xs) s =
    displayXMLBodyContent xs
        (s ++ displayXMLFormatting (ContentFormatting format))
displayXMLBodyContent ((ContentLink link):xs) s =
    displayXMLBodyContent xs
        (s ++ displayXMLLink (ContentLink link))
displayXMLBodyContent ((ContentImage image):xs) s =
    displayXMLBodyContent xs
    (s ++ displayXMLImage (ContentImage image))
displayXMLBodyContent ((ContentParagraph paragraph):xs) s =
    displayXMLBodyContent xs
        (s ++ displayXMLParagraph (ContentParagraph paragraph))
displayXMLBodyContent ((ContentSection section):xs) s =
    displayXMLBodyContent xs
        (s ++ displayXMLSection section)
displayXMLBodyContent ((ContentCodeBlock code):xs) s =
    displayXMLBodyContent xs
        (s ++ displayXMLCodeBlock (ContentCodeBlock code))
displayXMLBodyContent ((ContentList list):xs) s =
    displayXMLBodyContent xs
        (s ++ displayXMLList (ContentList list))

displayXMLBody :: [PandocContent] -> String
displayXMLBody body_content = "<body>" ++
    displayXMLBodyContent body_content "" ++ "</body>"

displayXML :: (PandocValue, String) -> String
displayXML (PandocValue pandocHead pandocBody, _) =
    "<document>" ++ displayXMLHead pandocHead ++
        displayXMLBody pandocBody ++ "</document>"
