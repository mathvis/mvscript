module Main where

import Parser
import System.Environment
import ParserTypes
import Text.Megaparsec

parseFile :: String -> String -> [TopLevel]
parseFile filename file =
    case runParser (many topLevel <* eof) filename file of
        Left e -> error ("Error while parsing: " ++ errorBundlePretty e)
        Right parsed -> parsed
main :: IO ()
main =
    do
        (filename : _) <- getArgs
        fileContents <- readFile filename
        let parsed = parseFile filename fileContents
        mapM_ print $ parsed
            
