module Main where

import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriter)
import Misc (sc)
import Parser
import ParserTypes
import SemanticAnalysis (checkTopLevel)
import SemanticAnalysisTypes (SemanticError, TypedTopLevel, globalEnv)
import System.Environment
import Text.Megaparsec

parseFile :: String -> String -> [TopLevel]
parseFile filename file =
    case runParser (sc *> many topLevel <* eof) filename file of
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
