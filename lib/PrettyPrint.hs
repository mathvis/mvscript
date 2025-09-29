{-# LANGUAGE FlexibleInstances #-}
module PrettyPrint (module PrettyPrint) where
import Types
import qualified Data.Text as T
import qualified Data.Map as Map
import Error
import Misc
import Data.Maybe

class PrettyPrint a where
    pprint :: a -> ParserState -> IO ()

printSymbolTableEntry :: ParserState -> (T.Text, VariableData) -> IO ()
printSymbolTableEntry state (name, vData) = do
    putStrLn $ blue state ("\tVariable name: " ++ T.unpack name)
    putStrLn $ yellow state ("\t\tType: " ++ show vType)
    putStrLn $ yellow state ("\t\tIn scope: " ++ show (inScope vData))
    putStrLn $ yellow state ("\t\tIs initialized: " ++ show (isInitialized vData))
    putStrLn $ yellow state ("\t\tIs constant: " ++ show (isConstant vData))
    where
        vType = fromMaybe VoidT (variableType vData) 

printArgument :: ParserState -> (T.Text, TypeName) -> IO ()
printArgument state (name, typename) =
    putStrLn $ yellow state ("\t\t\t" ++ T.unpack name ++ ": " ++ show typename)
        
printFunctionSymbolTableEntry :: ParserState -> (T.Text, FunctionData) -> IO ()
printFunctionSymbolTableEntry state (name, fData) = do
    putStrLn $ blue state ("\tFunction name: " ++ T.unpack name)
    putStrLn $ yellow state ("\t\tReturn type: " ++ show (returnType fData))
    putStrLn $ yellow state "\t\tArguments: "
    mapM_ (printArgument state) (arguments fData)
    putStrLn $ yellow state ("\t\tHas body: " ++ show (hasBody fData))


instance PrettyPrint SymbolTable where
    pprint st state = do
        putStrLn $ (bold state . darkerBlue state) "Symbol table:"
        mapM_ (printSymbolTableEntry state) (Map.toList st)

instance PrettyPrint FunctionSymbolTable where
    pprint fst state = do
        putStrLn $ (bold state . darkerBlue state) "Function symbol table:"
        mapM_ (printFunctionSymbolTableEntry state) (Map.toList fst)

instance PrettyPrint ContextStack where
    pprint ctx state = do
        putStrLn $ (bold state . darkerBlue state) ("Context: " ++ intercalateStr "::" (map (reverse . show) ctx))

    

