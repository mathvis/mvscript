module Main where

import Parser
import System.Environment
import ParserTypes
import Text.Megaparsec
import SemanticAnalysisTypes (TypedTopLevel, SemanticError, globalEnv)
import SemanticAnalysis (checkTopLevel)
import Control.Monad.Writer (runWriter)
import Control.Monad.Reader (runReaderT)

parseFile :: String -> String -> [TopLevel]
parseFile filename file =
    case runParser (many topLevel <* eof) filename file of
        Left e -> error ("Error while parsing: " ++ errorBundlePretty e)
        Right parsed -> parsed

runCheck :: [TopLevel] -> ([TypedTopLevel], [SemanticError])
runCheck program = runWriter (runReaderT (traverse checkTopLevel program) globalEnv)

main :: IO ()
main =
    do
        (filename : _) <- getArgs
        fileContents <- readFile filename
        let parsed = parseFile filename fileContents
        let checked = runCheck parsed
        mapM_ print (fst checked)
            
