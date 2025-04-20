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
import Text.ParserCombinators.Parsec
import TypeCheck
import Types
import Eval
import Misc

-- MAIN TYPE PARSERS
parseStatement :: Parser Statement
parseStatement = ((Decl <$> try parseDeclaration) <|> (Expr <$> try parseExpr) <|> (Comment <$> parseComment) <|> parseBlock NoType)

-- START OF THE CASCADING OPERATION PARSER
parseExpr :: Parser Expression
parseExpr = parseOr

parseType :: Parser Expression
parseType = parseNum <|> parseBool <|> parseArray <|> parseString <|> parseVector <|> parsePoint <|> parseMatrix

parseAtom :: Parser Expression
parseAtom = try parseLambda <|> try parseLambdaApplication <|> try parseFunctionCall <|> try parseType <|> try parseParens <|> parseVarIdentifier

parseTerm :: Parser Expression
parseTerm = try parseUnary <|> try parseAtom

parseDeclaration :: Parser Declaration
parseDeclaration = try parseVarInitialization <|> try parseVarDeclaration <|> try parseAssign <|> try parseFunctionForwardDeclaration <|> parseFunctionDeclaration <|> parseIf

-- DATA TYPE PARSERS
parseNum :: Parser Expression
parseNum = Type <$> (try parseFloat <|> parseInt)

parseString :: Parser Expression
parseString = Type . String . T.pack <$> (char '"' *> many (noneOf "\"") <* lexeme (char '"'))

parseInt :: Parser Type
parseInt = Int . read <$> lexeme (many1 digit)

parseBool :: Parser Expression
parseBool = Type . Bool <$> (parseTrue <|> parseFalse)
  where
    parseTrue = True <$ rword "true"
    parseFalse = False <$ rword "false"

parseFloat :: Parser Type
parseFloat = Float . read <$> lexeme (liftA2 (++) (many1 digit) ((:) <$> char '.' <*> many digit))

parseArray :: Parser Expression
parseArray = Type . Array <$> lexeme (char '[' *> (sepBy parseExpr (lexeme (string ","))) <* char ']')

parseVector :: Parser Expression
parseVector = Type . Vector <$> (rword "Vector" *> char '(' *> (sepBy parseExpr (lexeme (string ", "))) <* char ')')

parsePoint :: Parser Expression
parsePoint = Type . Point <$> (rword "Point" *> char '(' *> (sepBy parseExpr (lexeme (string ", "))) <* char ')')

parseMatrix :: Parser Expression
parseMatrix = Type . Matrix <$> (rword "Matrix" *> (char '(') *> (sepBy parseArray (lexeme (string ", "))) <* char ')')

-- VARIABLE PARSERS
parseVarIdentifier :: Parser Expression
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

parseVarDeclaration :: Parser Declaration
parseVarDeclaration =
    Variable
        <$> ((rword "let") *> parseVarIdentifier)
        <*> optionMaybe (lexeme (char ':') *> parseTypeName)
        <*> (lexeme (char ';') *> pure Nothing)

parseTypeName :: Parser TypeName
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

parseVarInitialization :: Parser Declaration
parseVarInitialization =
    (checkType . inferVariableType)
        <$> ( Variable
                <$> ((rword "let") *> parseVarIdentifier)
                <*> optionMaybe (lexeme (char ':') *> parseTypeName)
                <*> (lexeme (char '=') *> parseExpr <* lexeme (char ';') >>= \expr -> return (Just expr))
            )

-- OPERATION RELATED PARSERS
parseParens :: Parser Expression
parseParens = Parentheses <$> betweenParentheses parseExpr
parseUnary :: Parser Expression
parseUnary =
    ( lexeme $
        (char '-' *> pure (Operation . Negation))
            <|> (char '!' *> pure (Operation . Not))
            <|> (char '~' *> pure (Operation . BitwiseNot))
    )
        <*> parseTerm

parseAssign :: Parser Declaration
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
parseFunctionIdentifier :: Parser Expression
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

parseFunctionArguments :: Parser [(Expression, TypeName)]
parseFunctionArguments = sepBy parseFunctionArgument (lexeme $ char ',')
    where
        parseFunctionArgument = (,) <$> lexeme (parseVarIdentifier <* char ':') <*> parseTypeName

parseFunctionReturnType :: Parser TypeName
parseFunctionReturnType = parseTypeName <|> parseVoid
    where
        parseVoid = lexeme $ VoidT <$ string ""

parseFunctionForwardDeclaration :: Parser Declaration
parseFunctionForwardDeclaration = (endLine . lexeme) $ FunctionDef <$> ((rword "func") *> parseFunctionIdentifier) <*> (lexeme (char '(' *> parseFunctionArguments <* char ')')) <*> (parseFunctionReturnType) <*> pure Nothing

parseFunctionDeclaration :: Parser Declaration
parseFunctionDeclaration = lexeme $ FunctionDef <$> ((rword "func") *> parseFunctionIdentifier) <*> (lexeme (char '(' *> parseFunctionArguments <* char ')')) <*> (parseFunctionReturnType) <*> (Just <$> parseBlock FunctionBlock)

parseFunctionCallArguments :: Parser [Expression]
parseFunctionCallArguments = lexeme (sepBy parseExpr (lexeme $ char ','))

parseFunctionCall :: Parser Expression
parseFunctionCall = lexeme $ FunctionCall <$> parseFunctionIdentifier <*> between (lexeme $ char '(') (lexeme $ char ')') parseFunctionCallArguments

parseLambda :: Parser Expression
parseLambda = LambdaFunc <$> betweenParentheses parseFunctionArguments <*> (lexeme (char ':') *> (Just <$> (parseBlock FunctionBlock <|> parseStatement))) 

parseLambdaApplication :: Parser Expression
parseLambdaApplication =
    LambdaApplication
        <$> betweenParentheses parseLambda
        <*> (betweenParentheses parseExpr)

parseBlock :: BlockType -> Parser Statement
parseBlock blocktype = Block blocktype <$> ((newLine . lexeme) (char '{') *> many ((newLine . lexeme) parseStatement) <* lexeme (char '}'))

-- COMMENT PARSERS
parseComment :: Parser String
parseComment = (lexeme $ string "#" *> many anyChar)

-- CONTROL FLOW PARSERS

parseElse :: Parser Declaration
parseElse = lexeme $ ElseBlock <$> (rword "else" *> (parseBlock Else <|> parseStatement))

parseIf :: Parser Declaration
parseIf = collapseControlFlow <$> (lexeme $ IfBlock <$> (rword "if" *> betweenParentheses parseExpr) <*> (parseBlock If <|> (endLine parseStatement)) <*> optionMaybe parseElse)

-- OPERATION PARSERS
parseOr :: Parser Expression

parseOr = chainl1 parseAnd parseOrOp
    where    
        parseOrOp = lexeme $ try $ string "||" *> pure (\lhs rhs -> Operation (Or lhs rhs))

parseAnd :: Parser Expression
parseAnd = chainl1 parseComparison parseAndOp
    where
        parseAndOp = lexeme $ try $ string "&&" *> pure (\lhs rhs -> Operation (And lhs rhs))

parseComparison :: Parser Expression
parseComparison = chainl1 parseBitwiseOr parseComparisonOp
    where
        parseComparisonOp = lexeme $
            try (string ">=" *> pure (\lhs rhs -> Operation (GreaterThanEq lhs rhs)))
            <|> try (string "<=" *> pure (\lhs rhs -> Operation (LessThanEq lhs rhs)))
            <|> try (string "==" *> pure (\lhs rhs -> Operation (Equals lhs rhs)))
            <|> try (string "!=" *> pure (\lhs rhs -> Operation (NotEquals lhs rhs)))
            <|> try (char '>' *> pure (\lhs rhs -> Operation (GreaterThan lhs rhs)))
            <|> try (char '<' *> pure (\lhs rhs -> Operation (LessThan lhs rhs)))


parseBitwiseOr :: Parser Expression
parseBitwiseOr = chainl1 parseBitwiseXor parseBitwiseOrOp
    where
        parseBitwiseOrOp = lexeme $ try $ string "b|" *> pure (\lhs rhs -> Operation (BitwiseOr lhs rhs))

parseBitwiseXor :: Parser Expression
parseBitwiseXor = chainl1 parseBitwiseAnd parseBitwiseXorOp
    where
        parseBitwiseXorOp = lexeme $ try $ char '^' *> pure (\lhs rhs -> Operation (BitwiseXor lhs rhs))

parseBitwiseAnd :: Parser Expression
parseBitwiseAnd = chainl1 parseAddSub parseBitwiseAndOp
    where
        parseBitwiseAndOp = lexeme $ try $ string "b&" *> pure (\lhs rhs -> Operation (BitwiseAnd lhs rhs))

parseAddSub :: Parser Expression
parseAddSub = chainl1 parseMulDivMod parseAddSubOp
    where
        parseAddSubOp = lexeme $ 
            try (char '+' *> pure (\lhs rhs -> Operation (Add lhs rhs)))
            <|> try (char '-' *> pure (\lhs rhs -> Operation (Subtract lhs rhs)))

parseMulDivMod :: Parser Expression
parseMulDivMod = chainl1 parseTerm parseMulDivModOp -- parseTerm parses unary negation, giving it the highest priority
    where
        parseMulDivModOp = lexeme $       
            try (char '*' *> pure (\lhs rhs -> Operation (Multiply lhs rhs)))
            <|> try (string "//" *> pure (\lhs rhs -> Operation (IntDivide lhs rhs)))
            <|> try (char '/' *> pure (\lhs rhs -> Operation (Divide lhs rhs)))
            <|> try (char '%' *> pure (\lhs rhs -> Operation (Modulo lhs rhs)))
