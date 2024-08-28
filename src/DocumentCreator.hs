module DocumentCreator
    (
        writePandocValue
    ) where

import Utils ( Parser(..), PandocValue )
import DocumentParser
import ParserJSON
import ParserXML
import ParserMD
import OutputJSON
import OutputMarkdown
import OutputXML
import System.Exit (exitWith, ExitCode(ExitFailure))
import Control.Exception

testOthers :: String -> Maybe (PandocValue, String)
testOthers content = case runParser parseXml content of
    Nothing -> runParser parseMarkdown content
    _ -> runParser parseXml content

testParsers :: String -> Maybe (PandocValue, String)
testParsers content = case runParser parseJson content of
    Nothing -> testOthers content
    _ -> runParser parseJson content

getParsed :: Args -> String -> Maybe (PandocValue, String)
getParsed (Args _ _ _ Nothing) content = testParsers content
getParsed (Args _ _ _ (Just "xml")) content = runParser parseXml content
getParsed (Args _ _ _ (Just "json")) content = runParser parseJson content
getParsed (Args _ _ _ (Just "markdown")) content = runParser parseMarkdown content
getParsed _ _ = Nothing

dispContent :: Args -> Maybe (PandocValue, String) -> String
dispContent (Args _ (Just "xml") _ _) (Just parsed) =
   displayXML parsed
dispContent (Args _ (Just "json") _ _) (Just parsed) =
    displayJson parsed
dispContent (Args _ (Just "markdown") _ _) (Just parsed) =
    displayMarkdown parsed
dispContent _ _ = ""

writeContent :: Args -> String -> IO ()
writeContent (Args _ _ Nothing _) content = putStrLn content
writeContent (Args _ _ (Just "") _) _ = exitWith (ExitFailure 84)
writeContent (Args _ _ (Just name) _) content = writeFile name content

callDisplay :: Args -> String -> String
callDisplay args content = dispContent args (getParsed args content)

readFileSafe :: FilePath -> IO (Either IOException String)
readFileSafe path = catch (Right <$> readFile path) (\end -> return $ Left end)

writePandocValue :: Args -> IO ()
writePandocValue (Args (Just inp) fout out fin) = do
    file <- readFileSafe inp
    case file of
        Left _ -> putStrLn "Can't open file: doesn't exist" >>
            exitWith (ExitFailure 84)
        Right content ->
            writeContent (Args (Just inp) fout out fin) toWrite
            where
                toWrite = callDisplay (Args (Just inp) fout out fin) content
writePandocValue _ = putStrLn "Error"
