{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser where

import Control.Applicative (liftA2)
import Control.Monad
import Data.Char
import Data.Functor
import Data.Text as T (pack, unpack)
import Eval
import Misc
import Context
import Text.Parsec
import TypeCheck
import Types
import VariableStorage
import FunctionStorage
import Debug.Trace

-- MAIN TYPE PARSERS
parseStatement :: MVParser Statement
parseStatement = (Decl <$> try parseDeclaration) <|> (Expr <$> (try parseReturn <|> try parseExpr)) <|> (Comment <$> parseComment) <|> parseBlock NoType

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
parseArray = Type . Array <$> lexeme (char '[' *> sepBy parseExpr (lexeme (string ",")) <* char ']')

parseVector :: MVParser Expression
parseVector = Type . Vector <$> (rword "Vector" *> char '(' *> sepBy parseExpr (lexeme (string ", ")) <* char ')')

parsePoint :: MVParser Expression
parsePoint = Type . Point <$> (rword "Point" *> char '(' *> sepBy parseExpr (lexeme (string ", ")) <* char ')')

parseMatrix :: MVParser Expression
parseMatrix = Type . Matrix <$> (rword "Matrix" *> char '(' *> sepBy parseArray (lexeme (string ", ")) <* char ')')

-- VARIABLE PARSERS
parseVarIdentifier :: MVParser Expression
parseVarIdentifier = getPosition >>= \pos -> identifier >>= (\expr -> modifyState (checkScope pos expr) >> return expr) . VarIdentifier . T.pack
  where
    identifier =
        (lexeme . try) $ do
            name <- (:) <$> firstChar <*> many nonFirstChar
            if name `elem` reservedKeywords
                then fail $ "Cannot use reserved keyword '" ++ name ++ "' as an identifier"
                else return name
    firstChar = letter <|> char '_'
    nonFirstChar = firstChar <|> digit

parseArgIdentifier :: MVParser Expression
parseArgIdentifier = VarIdentifier . T.pack <$> identifier
  where
    identifier =
        (lexeme . try) $ do
            name <- (:) <$> firstChar <*> many nonFirstChar
            if name `elem` reservedKeywords
                then fail $ "Cannot use reserved keyword '" ++ name ++ "' as an identifier"
                else return name
    firstChar = letter <|> char '_'
    nonFirstChar = firstChar <|> digit

parseVarIdentifierDecl :: MVParser Expression
parseVarIdentifierDecl = VarIdentifier . T.pack <$> identifier
  where
    identifier =
        (lexeme . try) $ do
            name <- (:) <$> firstChar <*> many nonFirstChar
            if name `elem` reservedKeywords
                then fail $ "Cannot use reserved keyword '" ++ name ++ "' as an identifier"
                else return name
    firstChar = letter <|> char '_'
    nonFirstChar = firstChar <|> digit

parseVarDeclaration :: MVParser Declaration
parseVarDeclaration =
    Variable
        <$> (rword "let" *> parseVarIdentifierDecl)
        <*> optionMaybe (lexeme (char ':') *> parseTypeName)
        <*> (lexeme (char ';') Data.Functor.$> Nothing)
        >>= \decl -> modifyState (addVariableToTable decl) >> return decl

parseVoidTypeName :: MVParser TypeName
parseVoidTypeName = lexeme $ VoidT <$ string ""

parseTypeName :: MVParser TypeName
parseTypeName = parseIntTName <|> parseStringTName <|> parseFloatTName <|> parseBoolTName <|> parseVectorTName <|> parseMatrixTName <|> parsePointTName <|> parseArrayTName <|> parseLambdaTName
  where
    parseIntTName = lexeme $ IntT <$ string "int"
    parseFloatTName = lexeme $ FloatT <$ string "float"
    parseBoolTName = lexeme $ BoolT <$ string "bool"
    parseStringTName = lexeme $ StringT <$ string "string"
    parsePointTName = lexeme $ PointT <$ string "point"
    parseVectorTName = lexeme $ VectorT <$ string "vector"
    parseMatrixTName = lexeme $ MatrixT <$ string "matrix"
    parseArrayTName = lexeme $ ArrayT <$> (char '[' *> parseTypeName <* char ']')
    parseLambdaTName = lexeme $ LambdaT <$> (string "lambda[" *> sepBy parseTypeName (lexeme $ char ',') <* char ']') <*> (parseTypeName <|> parseVoidTypeName)

parseVarInitialization :: MVParser Declaration
parseVarInitialization =
    getPosition >>= \pos ->
        getState >>= \state ->
            ( Variable
                <$> (rword "let" *> parseVarIdentifierDecl)
                <*> optionMaybe (lexeme (char ':') *> parseTypeName)
                <*> (lexeme (char '=') *> parseExpr <* lexeme (char ';') >>= \expr -> return (Just expr))
            )
                >>= (\decl -> modifyState (addVariableToTable decl) >> return decl) . checkType pos state . inferVariableType pos state

-- OPERATION RELATED PARSERS
parseParens :: MVParser Expression
parseParens = Parentheses <$> betweenParentheses parseExpr

parseUnary :: MVParser Expression
parseUnary =
    lexeme
        ( (char '-' $> (Operation . Negation))
            <|> (char '!' $> (Operation . Not))
            <|> (char '~' $> (Operation . BitwiseNot))
        )
        <*> parseTerm

parseAssign :: MVParser Declaration
parseAssign =
    getPosition >>= \pos ->
        parseVarIdentifierDecl >>= \leftTerm ->
            endLine
                ( lexeme
                    ( try (string "//=" $> (\lhs rhs -> Assignment (IntDivAssign lhs rhs)))
                        <|> try (string "+=" $> (\lhs rhs -> Assignment (AddAssign lhs rhs)))
                        <|> try (string "-=" $> (\lhs rhs -> Assignment (SubAssign lhs rhs)))
                        <|> try (string "*=" $> (\lhs rhs -> Assignment (MulAssign lhs rhs)))
                        <|> try (string "/=" $> (\lhs rhs -> Assignment (DivAssign lhs rhs)))
                        <|> try (string "%=" $> (\lhs rhs -> Assignment (ModAssign lhs rhs)))
                        <|> try (string "|=" $> (\lhs rhs -> Assignment (BitwiseOrAssign lhs rhs)))
                        <|> try (string "&=" $> (\lhs rhs -> Assignment (BitwiseAndAssign lhs rhs)))
                        <|> try (string "^=" $> (\lhs rhs -> Assignment (BitwiseXorAssign lhs rhs)))
                        <|> try (string "=" $> (\lhs rhs -> Assignment (Assign lhs rhs)))
                    )
                    <*> (\term -> modifyState (checkScope pos term) >> pure term) leftTerm
                    <*> parseExpr
                )
                        >>= \decl -> modifyState (addFunctionToTable decl True >>  updateVariableUninitialized pos decl) >> return decl

-- FUNCTION RELATED PARSERS
parseFunctionIdentifier :: MVParser Expression
parseFunctionIdentifier = FunctionIdentifier . T.pack <$> identifier
  where
    identifier =
        try $ do
            name <- (:) <$> firstChar <*> many nonFirstChar
            if name `elem` reservedKeywords
                then fail $ "Cannot use reserved keyword '" ++ name ++ "' as a function identifier"
                else return name
    firstChar = letter <|> char '_'
    nonFirstChar = firstChar <|> digit

parseFunctionArguments :: MVParser [(Expression, TypeName)]
parseFunctionArguments = sepBy parseFunctionArgument (lexeme $ char ',') >>= \args -> modifyState (addArgumentsToTable args) >> return args
  where
    parseFunctionArgument = (,) <$> lexeme (parseArgIdentifier <* char ':') <*> parseTypeName

parseFunctionReturnType :: MVParser TypeName
parseFunctionReturnType = parseTypeName <|> parseVoidTypeName

parseFunctionSignature :: MVParser (Expression, [(Expression, TypeName)], TypeName)
parseFunctionSignature = (,,) <$> (rword "func" *> parseFunctionIdentifier)
                              <*> lexeme (char '(' *> parseFunctionArguments <* char ')')
                              <*> parseFunctionReturnType

parseFunctionForwardDeclaration :: MVParser Declaration
parseFunctionForwardDeclaration = (lexeme . endLine) $ parseFunctionSignature >>=
                               \(funcIdentifier, args, returnType) -> let forwardDecl = FunctionDef funcIdentifier args returnType Nothing in
                               modifyState (addFunctionToTable forwardDecl False) >> return forwardDecl

parseFunctionDeclaration :: MVParser Declaration
parseFunctionDeclaration = lexeme $ parseFunctionSignature >>=
                               \(funcIdentifier, args, returnType) -> let partialDecl = FunctionDef funcIdentifier args returnType Nothing in
                               modifyState (addFunctionToTable partialDecl True) >>
                               (FunctionDef funcIdentifier args returnType . Just <$> parseBlock (FunctionBlock returnType))
                               >>= \decl -> modifyState (removeArgumentsFromTable decl) >> return decl

parseFunctionCallArguments :: MVParser [Expression]
parseFunctionCallArguments = sepBy parseExpr (lexeme $ char ',')

parseFunctionCall :: MVParser Expression
parseFunctionCall = getPosition >>= \pos -> functionCall >>= (\expr -> modifyState (checkScope pos expr) >> return expr)
    where
        functionCall = lexeme $ FunctionCall <$> parseFunctionIdentifier <*> between (lexeme $ char '(') (lexeme $ char ')') parseFunctionCallArguments

-- TODO: fix lambda return types
parseLambda :: MVParser Expression
parseLambda = LambdaFunc <$> betweenParentheses parseFunctionArguments <*> (lexeme (char ':') *> (Just <$> (parseBlock (FunctionBlock VoidT) <|> parseStatement))) >>= \expr -> modifyState (removeArgumentsFromTableLambda expr) >> return expr

parseLambdaApplication :: MVParser Expression
parseLambdaApplication =
    LambdaApplication
        <$> betweenParentheses parseLambda
        <*> betweenParentheses parseExpr

parseBlock :: BlockType -> MVParser Statement
parseBlock blocktype = modifyState (changeContext blocktype) >> ((newLine . lexeme) (char '{') *> many ((newLine . lexeme) parseStatement) <* lexeme (char '}')) >>= (\block -> modifyState (removeScopeVariables block . resetContext) >> return block) . Block blocktype 

parseReturn :: MVParser Expression
parseReturn = getPosition >>= \pos -> (endLine . lexeme) (Return <$> (lexeme (string "return") *> optionMaybe parseExpr)) >>= \expr -> modifyState (checkReturnType expr pos . checkForBlock pos) >> return expr

-- COMMENT PARSERS
parseComment :: MVParser String
parseComment = lexeme $ string "#" *> many anyChar

-- CONTROL FLOW PARSERS

parseElse :: MVParser Declaration
parseElse = lexeme $ ElseBlock <$> (rword "else" *> (parseBlock Else <|> parseStatement))

parseIf :: MVParser Declaration
parseIf = lexeme (IfBlock <$> (rword "if" *> betweenParentheses parseExpr) <*> (parseBlock If <|> endLine parseStatement) <*> optionMaybe parseElse) >>= evaluateControlFlow (collapseControlFlow <$> getConfig)

-- OPERATION PARSERS
parseOr :: MVParser Expression
parseOr = chainl1 parseAnd parseOrOp >>= evaluateOperations (collapseOperations <$> getConfig)
  where
    parseOrOp = lexeme $ try (string "||" Data.Functor.$> (\lhs rhs -> Operation (Or lhs rhs)))

parseAnd :: MVParser Expression
parseAnd = chainl1 parseComparison parseAndOp >>= evaluateOperations (collapseOperations <$> getConfig)
  where
    parseAndOp = lexeme $ try (string "&&" Data.Functor.$> (\lhs rhs -> Operation (And lhs rhs)))

parseComparison :: MVParser Expression
parseComparison = chainl1 parseBitwiseOr parseComparisonOp >>= evaluateOperations (collapseOperations <$> getConfig)
  where
    parseComparisonOp =
        lexeme $
            try (string ">=" Data.Functor.$> (\lhs rhs -> Operation (GreaterThanEq lhs rhs)))
                <|> try (string "<=" Data.Functor.$> (\lhs rhs -> Operation (LessThanEq lhs rhs)))
                <|> try (string "==" Data.Functor.$> (\lhs rhs -> Operation (Equals lhs rhs)))
                <|> try (string "!=" Data.Functor.$> (\lhs rhs -> Operation (NotEquals lhs rhs)))
                <|> try (char '>' Data.Functor.$> (\lhs rhs -> Operation (GreaterThan lhs rhs)))
                <|> try (char '<' Data.Functor.$> (\lhs rhs -> Operation (LessThan lhs rhs)))

parseBitwiseOr :: MVParser Expression
parseBitwiseOr = chainl1 parseBitwiseXor parseBitwiseOrOp >>= evaluateOperations (collapseOperations <$> getConfig)
  where
    parseBitwiseOrOp = lexeme $ try (string "b|" Data.Functor.$> (\lhs rhs -> Operation (BitwiseOr lhs rhs)))

parseBitwiseXor :: MVParser Expression
parseBitwiseXor = chainl1 parseBitwiseAnd parseBitwiseXorOp >>= evaluateOperations (collapseOperations <$> getConfig)
  where
    parseBitwiseXorOp = lexeme $ try (char '^' Data.Functor.$> (\lhs rhs -> Operation (BitwiseXor lhs rhs)))

parseBitwiseAnd :: MVParser Expression
parseBitwiseAnd = chainl1 parseAddSub parseBitwiseAndOp >>= evaluateOperations (collapseOperations <$> getConfig)
  where
    parseBitwiseAndOp = lexeme $ try (string "b&" Data.Functor.$> (\lhs rhs -> Operation (BitwiseAnd lhs rhs)))

parseAddSub :: MVParser Expression
parseAddSub = chainl1 parseMulDivMod parseAddSubOp >>= evaluateOperations (collapseOperations <$> getConfig)
  where
    parseAddSubOp =
        lexeme $
            try (char '+' Data.Functor.$> (\lhs rhs -> Operation (Add lhs rhs)))
                <|> try (char '-' Data.Functor.$> (\lhs rhs -> Operation (Subtract lhs rhs)))

parseMulDivMod :: MVParser Expression
parseMulDivMod = chainl1 parseTerm parseMulDivModOp >>= evaluateOperations (collapseOperations <$> getConfig)
  where
    parseMulDivModOp =
        lexeme $
            try (char '*' Data.Functor.$> (\lhs rhs -> Operation (Multiply lhs rhs)))
                <|> try (string "//" Data.Functor.$> (\lhs rhs -> Operation (IntDivide lhs rhs)))
                <|> try (char '/' Data.Functor.$> (\lhs rhs -> Operation (Divide lhs rhs)))
                <|> try (char '%' Data.Functor.$> (\lhs rhs -> Operation (Modulo lhs rhs)))
