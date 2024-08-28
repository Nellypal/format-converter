module Main (main) where

import DocumentParser
import DocumentCreator
import System.Environment(getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))

dispHelp :: IO()
dispHelp = putStrLn "USAGE: " >>
    putStrLn "    ./format-converter -i ifile -f oformat [-o ofile] [-e iformat]" >>
    putStrLn "" >>
    putStrLn "DESCRIPTION:  " >>
    putStrLn "  ifile  path to the file to convert" >>
    putStrLn "  oformat  output format (xml, json, markdown)" >>
    putStrLn "  ofile  path to the output file" >>
    putStrLn "  iformat  input format (xml, json, markdown)"

dispListRes :: Maybe Args -> IO()
dispListRes Nothing = putStrLn ("Wrong Arguments passed: try "
    ++ "-h or --help for more information") >>
    exitWith (ExitFailure 84)
dispListRes (Just args) = writePandocValue args

main :: IO ()
main = do
    args <- getArgs
    if args == ["-h"] || args == ["--help"] then dispHelp
    else dispListRes (getOpts defaultArgs args)
