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

-- AUXILIARY PARSERS
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

endLine :: Parser a -> Parser a
endLine p = p <* char ';'

newLine :: Parser a -> Parser a
newLine p = p <* optional (char '\n')

betweenParentheses :: Parser a -> Parser a
betweenParentheses = between (lexeme $ char '(') (lexeme $ char ')') 

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy (letter <|> digit))

-- MAIN TYPE PARSERS
parseStatement :: Parser Statement
parseStatement = ((Expr <$> try parseExpr) <|> (Decl <$> parseDeclaration) <|> (Comment <$> parseComment) <|> parseBlock NoType)

parseExpr :: Parser Expression
parseExpr = chainl1 parseTerm parseBinaryOperator

parseType :: Parser Expression
parseType = parseNum <|> parseBool <|> parseArray <|> parseString <|> parseVector <|> parsePoint <|> parseMatrix

parseAtom :: Parser Expression
parseAtom = try parseLambda <|> parseLambdaApplication <|> try parseFunctionCall <|> try parseType <|> parseParens <|> parseVarIdentifier

parseTerm :: Parser Expression
parseTerm = parseAtom <|> parseUnary

parseDeclaration :: Parser Declaration
parseDeclaration = try parseVarInitialization <|> try parseVarDeclaration <|> parseAssign <|> try parseFunctionForwardDeclaration <|> parseFunctionDeclaration <|> parseIf

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
parseParens = Parentheses <$> (lexeme (char '(') *> parseExpr <* lexeme (char ')'))

parseBinaryOperator :: Parser (Expression -> Expression -> Expression)
parseBinaryOperator =
    lexeme $
        (char '+' *> pure (\lhs rhs -> Operation (Add lhs rhs)))
            <|> (char '-' *> pure (\lhs rhs -> Operation (Subtract lhs rhs)))
            <|> (char '*' *> pure (\lhs rhs -> Operation (Multiply lhs rhs)))
            <|> (char '/' *> pure (\lhs rhs -> Operation (Divide lhs rhs)))
            <|> (string "//" *> pure (\lhs rhs -> Operation (IntDivide lhs rhs)))
            <|> (char '%' *> pure (\lhs rhs -> Operation (Modulo lhs rhs)))
            <|> (char '>' *> pure (\lhs rhs -> Operation (GreaterThan lhs rhs)))
            <|> (char '<' *> pure (\lhs rhs -> Operation (LessThan lhs rhs)))
            <|> (string "==" *> pure (\lhs rhs -> Operation (Equals lhs rhs)))
            <|> (string "!=" *> pure (\lhs rhs -> Operation (NotEquals lhs rhs)))
            <|> (string ">=" *> pure (\lhs rhs -> Operation (GreaterThanEq lhs rhs)))
            <|> (string "<=" *> pure (\lhs rhs -> Operation (LessThanEq lhs rhs)))
            <|> (string "&&" *> pure (\lhs rhs -> Operation (And lhs rhs)))
            <|> (string "||" *> pure (\lhs rhs -> Operation (Or lhs rhs)))
            <|> (string "&" *> pure (\lhs rhs -> Operation (BitwiseAnd lhs rhs)))
            <|> (string "|" *> pure (\lhs rhs -> Operation (BitwiseOr lhs rhs)))
            <|> (string "^" *> pure (\lhs rhs -> Operation (BitwiseXor lhs rhs)))

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
                        <|> (string "+=" *> pure (\lhs rhs -> Assignment (AddAssign lhs rhs)))
                        <|> (string "-=" *> pure (\lhs rhs -> Assignment (SubAssign lhs rhs)))
                        <|> (string "*=" *> pure (\lhs rhs -> Assignment (MulAssign lhs rhs)))
                        <|> (string "/=" *> pure (\lhs rhs -> Assignment (DivAssign lhs rhs)))
                        <|> (string "%=" *> pure (\lhs rhs -> Assignment (ModAssign lhs rhs)))
                        <|> (string "|=" *> pure (\lhs rhs -> Assignment (BitwiseOrAssign lhs rhs)))
                        <|> (string "&=" *> pure (\lhs rhs -> Assignment (BitwiseAndAssign lhs rhs)))
                        <|> (string "^=" *> pure (\lhs rhs -> Assignment (BitwiseXorAssign lhs rhs)))
                        <|> (string "=" *> pure (\lhs rhs -> Assignment (Assign lhs rhs)))
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
