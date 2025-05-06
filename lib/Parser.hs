{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use infix" #-}
module Parser where

import Control.Applicative (liftA2)
import Control.Monad
import Data.Char
import Data.Text as T (unpack, pack)
import TypeCheck
import Types
import Eval
import Misc
import Text.Parsec

-- MAIN TYPE PARSERS
parseStatement :: MVParser Statement
parseStatement = ((Decl <$> try parseDeclaration) <|> (Expr <$> try parseExpr) <|> (Comment <$> parseComment) <|> parseBlock NoType)

-- START OF THE CASCADING OPERATION PARSER
parseExpr :: MVParser Expression 
parseExpr = parseOr

parseType :: MVParser Expression 
parseType = parseNum <|> parseBool <|> parseArray <|> parseString <|> parseVector <|> parsePoint <|> parseMatrix

parseAtom :: MVParser Expression
parseAtom = try parseLambda <|> try parseLambdaApplication <|> try parseFunctionCall <|> try parseType <|> try parseParens <|> parseVarIdentifier

parseTerm :: MVParser Expression
parseTerm = try parseUnary <|> try parseAtom

parseDeclaration :: MVParser Declaration
parseDeclaration = try parseVarInitialization <|> try parseVarDeclaration <|> try parseAssign <|> try parseFunctionForwardDeclaration <|> parseFunctionDeclaration <|> parseIf

-- DATA TYPE PARSERS
parseNum :: MVParser Expression
parseNum = Type <$> (try parseFloat <|> parseInt)

parseString :: MVParser Expression
parseString = Type . String . T.pack <$> (char '"' *> many (noneOf "\"") <* lexeme (char '"'))

parseInt :: MVParser Type
parseInt = Int . read <$> lexeme (many1 digit)

parseBool :: MVParser Expression
parseBool = Type . Bool <$> (parseTrue <|> parseFalse)
  where
    parseTrue = True <$ rword "true"
    parseFalse = False <$ rword "false"

parseFloat :: MVParser Type
parseFloat = Float . read <$> lexeme (liftA2 (++) (many1 digit) ((:) <$> char '.' <*> many digit))

parseArray :: MVParser Expression
parseArray = Type . Array <$> lexeme (char '[' *> (sepBy parseExpr (lexeme (string ","))) <* char ']')

parseVector :: MVParser Expression
parseVector = Type . Vector <$> (rword "Vector" *> char '(' *> (sepBy parseExpr (lexeme (string ", "))) <* char ')')

parsePoint :: MVParser Expression
parsePoint = Type . Point <$> (rword "Point" *> char '(' *> (sepBy parseExpr (lexeme (string ", "))) <* char ')')

parseMatrix :: MVParser Expression
parseMatrix = Type . Matrix <$> (rword "Matrix" *> (char '(') *> (sepBy parseArray (lexeme (string ", "))) <* char ')')

-- VARIABLE PARSERS
parseVarIdentifier :: MVParser Expression
parseVarIdentifier = VarIdentifier <$> identifier
  where
    identifier =
        (lexeme . try) $ do
            name <- (:) <$> firstChar <*> many nonFirstChar
            if Prelude.elem name reservedKeywords
                then fail $ "Cannot use reserved keyword '" ++ name ++ "' as an identifier"
                else return name
    firstChar = letter <|> char '_'
    nonFirstChar = firstChar <|> digit

parseVarDeclaration :: MVParser Declaration
parseVarDeclaration =
    Variable
        <$> ((rword "let") *> parseVarIdentifier)
        <*> optionMaybe (lexeme (char ':') *> parseTypeName)
        <*> (lexeme (char ';') *> pure Nothing)

parseTypeName :: MVParser TypeName
parseTypeName = parseIntTName <|> parseStringTName <|> parseFloatTName <|> parseBoolTName <|> parseVectorTName <|> parseMatrixTName <|> parsePointTName <|> parseArrayTName 
    where
        parseIntTName = lexeme $ IntT <$ string "int"
        parseFloatTName = lexeme $ FloatT <$ string "float"
        parseBoolTName = lexeme $ BoolT <$ string "bool"
        parseStringTName = lexeme $ StringT <$ string "string"
        parsePointTName = lexeme $ PointT <$ string "point"
        parseVectorTName = lexeme $ VectorT <$ string "vector"
        parseMatrixTName = lexeme $ MatrixT <$ string "matrix"
        parseArrayTName = lexeme $ ArrayT <$> (char '[' *> parseTypeName <* char ']')

parseVarInitialization :: MVParser Declaration
parseVarInitialization =
    (checkType . inferVariableType)
        <$> ( Variable
                <$> ((rword "let") *> parseVarIdentifier)
                <*> optionMaybe (lexeme (char ':') *> parseTypeName)
                <*> (lexeme (char '=') *> parseExpr <* lexeme (char ';') >>= \expr -> return (Just expr))
            )

-- OPERATION RELATED PARSERS
parseParens :: MVParser Expression
parseParens = Parentheses <$> betweenParentheses parseExpr

parseUnary :: MVParser Expression
parseUnary =
    ( lexeme $
        (char '-' *> pure (Operation . Negation))
            <|> (char '!' *> pure (Operation . Not))
            <|> (char '~' *> pure (Operation . BitwiseNot))
    )
        <*> parseTerm

parseAssign :: MVParser Declaration
parseAssign =
 parseVarIdentifier >>= \leftTerm ->
     endLine
         ( ( lexeme
                 ( try (string "//=" *> pure (\lhs rhs -> Assignment (IntDivAssign lhs rhs)))
                     <|> try (string "+=" *> pure (\lhs rhs -> Assignment (AddAssign lhs rhs)))
                     <|> try (string "-=" *> pure (\lhs rhs -> Assignment (SubAssign lhs rhs)))
                     <|> try (string "*=" *> pure (\lhs rhs -> Assignment (MulAssign lhs rhs)))
                     <|> try (string "/=" *> pure (\lhs rhs -> Assignment (DivAssign lhs rhs)))
                     <|> try (string "%=" *> pure (\lhs rhs -> Assignment (ModAssign lhs rhs)))
                     <|> try (string "|=" *> pure (\lhs rhs -> Assignment (BitwiseOrAssign lhs rhs)))
                     <|> try (string "&=" *> pure (\lhs rhs -> Assignment (BitwiseAndAssign lhs rhs)))
                     <|> try (string "^=" *> pure (\lhs rhs -> Assignment (BitwiseXorAssign lhs rhs)))
                     <|> try (string "=" *> pure (\lhs rhs -> Assignment (Assign lhs rhs)))
                 )
           )
             <*> pure leftTerm
             <*> parseExpr
         )

-- FUNCTION RELATED PARSERS
parseFunctionIdentifier :: MVParser Expression
parseFunctionIdentifier = FunctionIdentifier <$> identifier
  where
    identifier =
        (lexeme . try) $ do
            name <- (:) <$> firstChar <*> many nonFirstChar
            if Prelude.elem name reservedKeywords
                then fail $ "Cannot use reserved keyword '" ++ name ++ "' as a function identifier"
                else return name
    firstChar = letter <|> char '_'
    nonFirstChar = firstChar <|> digit

parseFunctionArguments :: MVParser [(Expression, TypeName)]
parseFunctionArguments = sepBy parseFunctionArgument (lexeme $ char ',')
    where
        parseFunctionArgument = (,) <$> lexeme (parseVarIdentifier <* char ':') <*> parseTypeName

parseFunctionReturnType :: MVParser TypeName
parseFunctionReturnType = parseTypeName <|> parseVoid
    where
        parseVoid = lexeme $ VoidT <$ string ""

parseFunctionForwardDeclaration :: MVParser Declaration
parseFunctionForwardDeclaration = (endLine . lexeme) $ FunctionDef <$> ((rword "func") *> parseFunctionIdentifier) <*> (lexeme (char '(' *> parseFunctionArguments <* char ')')) <*> (parseFunctionReturnType) <*> pure Nothing

parseFunctionDeclaration :: MVParser Declaration
parseFunctionDeclaration = lexeme $ FunctionDef <$> ((rword "func") *> parseFunctionIdentifier) <*> (lexeme (char '(' *> parseFunctionArguments <* char ')')) <*> (parseFunctionReturnType) <*> (Just <$> parseBlock FunctionBlock)

parseFunctionCallArguments :: MVParser [Expression]
parseFunctionCallArguments = lexeme (sepBy parseExpr (lexeme $ char ','))

parseFunctionCall :: MVParser Expression
parseFunctionCall = lexeme $ FunctionCall <$> parseFunctionIdentifier <*> between (lexeme $ char '(') (lexeme $ char ')') parseFunctionCallArguments

parseLambda :: MVParser Expression
parseLambda = LambdaFunc <$> betweenParentheses parseFunctionArguments <*> (lexeme (char ':') *> (Just <$> (parseBlock FunctionBlock <|> parseStatement))) 

parseLambdaApplication :: MVParser Expression
parseLambdaApplication =
    LambdaApplication
        <$> betweenParentheses parseLambda
        <*> (betweenParentheses parseExpr)

parseBlock :: BlockType -> MVParser Statement
parseBlock blocktype = Block blocktype <$> ((newLine . lexeme) (char '{') *> many ((newLine . lexeme) parseStatement) <* lexeme (char '}'))

-- COMMENT PARSERS
parseComment :: MVParser String
parseComment = (lexeme $ string "#" *> many anyChar)

-- CONTROL FLOW PARSERS

parseElse :: MVParser Declaration
parseElse = lexeme $ ElseBlock <$> (rword "else" *> (parseBlock Else <|> parseStatement))

parseIf :: MVParser Declaration
parseIf =  (lexeme $ IfBlock <$> (rword "if" *> betweenParentheses parseExpr) <*> (parseBlock If <|> (endLine parseStatement)) <*> optionMaybe parseElse) >>= evaluateControlFlow (collapseControlFlow <$> getConfig)
-- OPERATION PARSERS
parseOr :: MVParser Expression
parseOr = chainl1 parseAnd parseOrOp >>= evaluateOperations (collapseOperations <$> getConfig)
    where    
        parseOrOp = lexeme $ try $ string "||" *> pure (\lhs rhs -> Operation (Or lhs rhs))

parseAnd :: MVParser Expression
parseAnd =  chainl1 parseComparison parseAndOp >>= evaluateOperations (collapseOperations <$> getConfig)
    where
        parseAndOp = lexeme $ try $ string "&&" *> pure (\lhs rhs -> Operation (And lhs rhs))

parseComparison :: MVParser Expression
parseComparison = chainl1 parseBitwiseOr parseComparisonOp >>= evaluateOperations (collapseOperations <$> getConfig)
    where
        parseComparisonOp = lexeme $
            try (string ">=" *> pure (\lhs rhs -> Operation (GreaterThanEq lhs rhs)))
            <|> try (string "<=" *> pure (\lhs rhs -> Operation (LessThanEq lhs rhs)))
            <|> try (string "==" *> pure (\lhs rhs -> Operation (Equals lhs rhs)))
            <|> try (string "!=" *> pure (\lhs rhs -> Operation (NotEquals lhs rhs)))
            <|> try (char '>' *> pure (\lhs rhs -> Operation (GreaterThan lhs rhs)))
            <|> try (char '<' *> pure (\lhs rhs -> Operation (LessThan lhs rhs)))


parseBitwiseOr :: MVParser Expression
parseBitwiseOr =  chainl1 parseBitwiseXor parseBitwiseOrOp >>= evaluateOperations (collapseOperations <$> getConfig)
    where
        parseBitwiseOrOp = lexeme $ try $ string "b|" *> pure (\lhs rhs -> Operation (BitwiseOr lhs rhs))

parseBitwiseXor :: MVParser Expression
parseBitwiseXor = chainl1 parseBitwiseAnd parseBitwiseXorOp >>= evaluateOperations (collapseOperations <$> getConfig)
    where
        parseBitwiseXorOp = lexeme $ try $ char '^' *> pure (\lhs rhs -> Operation (BitwiseXor lhs rhs))

parseBitwiseAnd :: MVParser Expression
parseBitwiseAnd = chainl1 parseAddSub parseBitwiseAndOp >>= evaluateOperations (collapseOperations <$> getConfig)
    where
        parseBitwiseAndOp = lexeme $ try $ string "b&" *> pure (\lhs rhs -> Operation (BitwiseAnd lhs rhs))

parseAddSub :: MVParser Expression
parseAddSub = chainl1 parseMulDivMod parseAddSubOp >>= evaluateOperations (collapseOperations <$> getConfig)
    where
        parseAddSubOp = lexeme $ 
            try (char '+' *> pure (\lhs rhs -> Operation (Add lhs rhs)))
            <|> try (char '-' *> pure (\lhs rhs -> Operation (Subtract lhs rhs)))

parseMulDivMod :: MVParser Expression
parseMulDivMod = chainl1 parseTerm parseMulDivModOp >>= evaluateOperations (collapseOperations <$> getConfig) 
    where
        parseMulDivModOp = lexeme $       
            try (char '*' *> pure (\lhs rhs -> Operation (Multiply lhs rhs)))
            <|> try (string "//" *> pure (\lhs rhs -> Operation (IntDivide lhs rhs)))
            <|> try (char '/' *> pure (\lhs rhs -> Operation (Divide lhs rhs)))
            <|> try (char '%' *> pure (\lhs rhs -> Operation (Modulo lhs rhs)))
