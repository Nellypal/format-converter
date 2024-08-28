module OutputMarkdown
    (
        displayMarkdown,
    ) where

import Utils

dispHeader :: PandocHeader -> String
dispHeader (PandocHeader title Nothing Nothing) =
    "---\ntitle: " ++ title ++ "\n---\n\n"
dispHeader (PandocHeader title (Just author) Nothing) =
    "---\ntitle: " ++ title ++
    "\nauthor: " ++ author ++ "\n---\n\n"
dispHeader (PandocHeader title Nothing (Just date)) =
    "---\ntitle: " ++ title ++
    "\ndate: " ++ date ++ "\n---\n\n"
dispHeader (PandocHeader title (Just author) (Just date)) =
    "---\ntitle: " ++ title ++
    "\nauthor: " ++ author ++
    "\ndate: " ++ date ++ "\n---\n\n"

dispContent :: Int -> Int -> [PandocContent] -> String
dispContent _ _ [] = ""
dispContent li pr ((ContentText text):rest) =
    text ++ (dispContent li pr rest)
dispContent li pr ((ContentFormatting format):rest) =
    (dispFormat format) ++ (dispContent li pr rest)
dispContent li pr ((ContentLink link):rest) =
    (dispLink link) ++ (dispContent li pr rest)
dispContent li pr ((ContentImage img):rest) =
    (dispImg img) ++ (dispContent li pr rest)
dispContent li pr ((ContentParagraph paragraph):rest) =
    (dispParagraph li pr paragraph) ++ (dispContent li pr rest)
dispContent li pr ((ContentCodeBlock code):rest) =
    (dispCode pr code) ++ (dispContent li pr rest)
dispContent li pr ((ContentSection section):rest) =
    (dispSection pr section) ++ (dispContent li pr rest)
dispContent li pr ((ContentList list):rest) =
    (dispList pr list) ++ (dispContent li pr rest)

dispFormat :: (PandocFormat, String) -> String
dispFormat (Italic, text) =
    "*" ++ text ++ "*"
dispFormat (Bold, text) =
    "**" ++ text ++ "**"
dispFormat (Code, text) =
    "`" ++ text ++ "`"

dispLink :: (String, String) -> String
dispLink (link, text) =
    "[" ++ text ++ "](" ++ link ++ ")"

dispImg :: (String, String) -> String
dispImg (img, text) =
    "![" ++ text ++ "](" ++ img ++ ")"

dispCode :: Int -> [PandocContent] -> String
dispCode pr code =
    "```\n" ++ (dispContent 1 pr code) ++ "```\n"

dispParagraph :: Int -> Int -> [PandocContent] -> String
dispParagraph 0 pr paragraph =
    (dispContent 0 pr paragraph) ++ "\n\n"
dispParagraph _ pr paragraph =
    (dispContent 1 pr paragraph) ++ "\n"

dispTitle :: Int -> String
dispTitle 0 = " "
dispTitle pr = "#" ++ (dispTitle (pr - 1))

dispSection :: Int -> PandocSection -> String
dispSection pr (PandocSection Nothing text) =
    (dispContent 0 (pr + 1) text)
dispSection pr (PandocSection (Just "") text) =
    (dispContent 0 (pr + 1) text)
dispSection pr (PandocSection (Just title) text) =
    (dispTitle pr) ++ title ++ "\n\n" ++ (dispContent 0 (pr + 1) text)

dispList :: Int -> [PandocContent] -> String
dispList _ [] = "\n"
dispList pr (paragraph:rest) =
    "- " ++ (dispContent 1 pr [paragraph]) ++
    (dispList pr rest)


displayMarkdown :: (PandocValue, String) -> String
displayMarkdown ((PandocValue headval bodyval), _) =
    (dispHeader headval) ++ (dispContent 0 1 bodyval)
