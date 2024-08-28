module DocumentParser
    (getOpts, defaultArgs, Args(..)
    ) where

data Args =
    Args {
        i :: Maybe String,
        f :: Maybe String,
        o :: Maybe String,
        e :: Maybe String
    }

defaultArgs :: Args
defaultArgs = Args {
        i = Nothing,
        f = Nothing,
        o = Nothing,
        e = Nothing
}

getOpts :: Args -> [String] -> Maybe Args
getOpts (Args Nothing _ _ _) [] = Nothing
getOpts (Args _ Nothing _ _) [] = Nothing
getOpts args [] = Just args
getOpts (Args _ formatout output formatin) ("-i":current:rest) =
    getOpts (Args (Just current) formatout output formatin) rest
getOpts (Args input _ output formatin) ("-f":current:rest) =
    getOpts (Args input (Just current) output formatin) rest
getOpts (Args input formatout _ formatin) ("-o":current:rest) =
    getOpts (Args input formatout (Just current) formatin) rest
getOpts (Args input formatout output _) ("-e":current:rest) =
    getOpts (Args input formatout output (Just current)) rest
getOpts _ _ = Nothing
