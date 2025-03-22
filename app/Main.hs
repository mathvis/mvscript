module Main where
import System.Environment
import Parser
import Text.ParserCombinators.Parsec
import Types
import System.Exit

parseFile :: [String] -> [Statement]
parseFile [] = []
parseFile (line:rest) =
    do
        case parse parseStatement "MVScript" line of
            Left e -> error ("Error while parsing: " ++ show e)
            Right parsed -> parsed:parseFile rest



main :: IO ()
main =
    do
        (filename:flags) <- getArgs
        fileContents <- lines <$> readFile filename
        mapM_ print $ parseFile fileContents
        
