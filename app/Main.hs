module Main where
import System.Environment
import Parser
import Text.ParserCombinators.Parsec
import Types
import System.Exit

parseFile :: String -> [Statement]
parseFile file =
    do
        case parse (many parseStatement) "MVScript" file of
            Left e -> error ("Error while parsing: " ++ show e)
            Right parsed -> parsed



main :: IO ()
main =
    do
        (filename:flags) <- getArgs
        fileContents <- readFile filename
        mapM_ print $ parseFile fileContents
        
