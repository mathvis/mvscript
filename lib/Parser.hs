{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser (module Parser) where

import Control.Applicative (liftA2)
import Control.Monad
import Data.Char
import Data.Functor
import Data.Text as T (pack, unpack)
import Eval
import Misc
import Context
import Text.Megaparsec
import Text.Megaparsec.Char
import TypeCheck
import Types
import VariableStorage
import FunctionStorage
import Debug.Trace
import Control.Arrow
import Config.ConfigHandler
import Control.Monad.State

-- MAIN TYPE PARSERS
parseStatement :: MVParser Statement
parseStatement = endLine ((Decl <$> try parseDeclaration) <|> (Expr <$> (try parseReturn <|> try parseExpr)) <|> (Comment <$> parseComment) <|> parseBlock NoType)

parseStatementInBlock :: MVParser Statement
parseStatementInBlock = endLine ((Decl <$> try parseDeclaration) <|> (Expr <$> (try parseReturn <|> try parseExpr)) <|> (Comment <$> parseComment)) 

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
parseInt = Int . read <$> lexeme (some digitChar)

parseBool :: MVParser Expression
parseBool = Type . Bool <$> (parseTrue <|> parseFalse)
  where
    parseTrue = True <$ rword "true"
    parseFalse = False <$ rword "false"

parseFloat :: MVParser Type
parseFloat = Float . read <$> lexeme (liftA2 (++) (some digitChar) ((:) <$> char '.' <*> many digitChar))

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
parseVarIdentifier = getSourcePos >>= \pos -> identifier >>= (\expr -> modify (checkScope expr pos) >> return expr) . VarIdentifier . T.pack
  where
    identifier =
        (lexeme . try) $ do
            name <- (:) <$> firstChar <*> many nonFirstChar
            if name `elem` reservedKeywords
                then fail $ "Cannot use reserved keyword '" ++ name ++ "' as an identifier"
                else return name
    firstChar = letterChar <|> char '_'
    nonFirstChar = firstChar <|> digitChar

parseArgIdentifier :: MVParser Expression
parseArgIdentifier = VarIdentifier . T.pack <$> identifier
  where
    identifier =
        (lexeme . try) $ do
            name <- (:) <$> firstChar <*> many nonFirstChar
            if name `elem` reservedKeywords
                then fail $ "Cannot use reserved keyword '" ++ name ++ "' as an identifier"
                else return name
    firstChar = letterChar <|> char '_'
    nonFirstChar = firstChar <|> digitChar

parseVarIdentifierDecl :: MVParser Expression
parseVarIdentifierDecl = VarIdentifier . T.pack <$> identifier
  where
    identifier =
        (lexeme . try) $ do
            name <- (:) <$> firstChar <*> many nonFirstChar
            if name `elem` reservedKeywords
                then fail $ "Cannot use reserved keyword '" ++ name ++ "' as an identifier"
                else return name
    firstChar = letterChar <|> char '_'
    nonFirstChar = firstChar <|> digitChar

parseVarDeclaration :: MVParser Declaration
parseVarDeclaration =
    Variable
        <$> (rword "let" *> parseVarIdentifierDecl)
        <*> (Just <$> (lexeme (char ':') *> parseTypeName))
        <*> pure Nothing
        >>= \decl -> modify (addVariableToTable decl) >> return decl

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
    getSourcePos >>= \pos ->
        get >>= \state ->
            ( Variable
                <$> (rword "let" *> parseVarIdentifierDecl)
                <*> optional (lexeme (char ':') *> parseTypeName)
                <*> (lexeme (char '=') *> parseExpr >>= \expr -> return (Just expr))
            )
                >>= (\decl -> modify (addVariableToTable decl) >> return decl) . checkType pos state . inferVariableType pos state

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
    getSourcePos >>= \pos ->
        parseVarIdentifierDecl >>= \leftTerm ->
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
                    <*> (\term -> modify (checkScope term pos) >> pure term) leftTerm
                    <*> parseExpr
                )
                        >>= \decl -> modify (updateVariableUninitialized pos decl) >> return decl

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
    firstChar = letterChar <|> char '_'
    nonFirstChar = firstChar <|> digitChar

parseFunctionArguments :: MVParser [(Expression, TypeName)]
parseFunctionArguments = sepBy parseFunctionArgument (lexeme $ char ',') >>= \args -> modify (addArgumentsToTable args) >> return args
  where
    parseFunctionArgument = (,) <$> lexeme (parseArgIdentifier <* char ':') <*> parseTypeName

parseFunctionReturnType :: MVParser TypeName
parseFunctionReturnType = parseTypeName <|> parseVoidTypeName

parseFunctionSignature :: MVParser (Expression, [(Expression, TypeName)], TypeName)
parseFunctionSignature = (,,) <$> (rword "func" *> parseFunctionIdentifier)
                              <*> lexeme (char '(' *> parseFunctionArguments <* char ')')
                              <*> parseFunctionReturnType

parseFunctionForwardDeclaration :: MVParser Declaration
parseFunctionForwardDeclaration = getSourcePos >>= \pos -> lexeme $ lexeme (string "[fwd]") *> parseFunctionSignature >>=
                               \(funcIdentifier, args, returnType) -> let forwardDecl = FunctionDef funcIdentifier args returnType Nothing in
                               modify (checkForSecondOrderFunction pos >>> addFunctionToTable forwardDecl) >> return forwardDecl

parseFunctionDeclaration :: MVParser Declaration
parseFunctionDeclaration =
    getSourcePos >>=
        \pos -> lexeme $ parseFunctionSignature >>=
        \(funcIdentifier, args, returnType) -> parseBlock (FunctionBlock returnType)
        >>= (\decl -> modify (
            checkForSecondOrderFunction pos
            >>> checkForReturn decl pos
            >>> removeArgumentsFromTable decl
            >>> compareFunctionSignatureToForwardDecl decl pos
            >>> addFunctionToTable decl
        ) >> return decl) . FunctionDef funcIdentifier args returnType . Just

parseFunctionCallArguments :: MVParser [Expression]
parseFunctionCallArguments = sepBy parseExpr (lexeme $ char ',')

parseFunctionCall :: MVParser Expression
parseFunctionCall = getSourcePos >>= \pos -> functionCall >>= (\expr -> modify (checkArguments expr pos . checkScope expr pos) >> return expr)
    where
        functionCall = lexeme $ FunctionCall <$> parseFunctionIdentifier <*> between (lexeme $ char '(') (lexeme $ char ')') parseFunctionCallArguments

-- TODO: fix lambda return types
parseLambda :: MVParser Expression
parseLambda = LambdaFunc <$> betweenParentheses parseFunctionArguments <*> (lexeme (char ':') *> (Just <$> (parseBlock (FunctionBlock VoidT) <|> parseStatement))) >>= \expr -> modify (removeArgumentsFromTableLambda expr) >> return expr

parseLambdaApplication :: MVParser Expression
parseLambdaApplication =
    LambdaApplication
        <$> betweenParentheses parseLambda
        <*> betweenParentheses parseExpr

parseBlock :: BlockType -> MVParser Statement
parseBlock blocktype = modify (changeContext blocktype) >> ((newLine . lexeme) (char '{') *> many ((newLine . lexeme) parseStatementInBlock) <* lexeme (char '}')) >>= (\block -> modify (removeScopeVariables block . removeContext) >> return block) . Block blocktype 

parseReturn :: MVParser Expression
parseReturn = getSourcePos >>= \pos -> lexeme (Return <$> (lexeme (string "return") *> optional parseExpr)) >>= \expr -> modify (checkReturnType expr pos . checkForBlock pos) >> return expr

-- COMMENT PARSERS
parseComment :: MVParser String
parseComment = lexeme $ string "#" *> many anySingle

-- CONTROL FLOW PARSERS

parseElse :: MVParser Declaration
parseElse = lexeme $ ElseBlock <$> (rword "else" *> (parseBlock Else <|> parseStatement))

parseIf :: MVParser Declaration
parseIf = lexeme (IfBlock <$> (rword "if" *> betweenParentheses parseExpr) <*> (parseBlock If <|> parseStatement) <*> optional parseElse) >>= evaluateControlFlow (getCollapseControlFlow <$> get)

-- OPERATION PARSERS
parseOr :: MVParser Expression
parseOr = chainl1 parseAnd parseOrOp >>= evaluateOperations (getCollapseOperations <$> get)
  where
    parseOrOp = lexeme $ try (string "||" Data.Functor.$> (\lhs rhs -> Operation (Or lhs rhs)))

parseAnd :: MVParser Expression
parseAnd = chainl1 parseComparison parseAndOp >>= evaluateOperations (getCollapseOperations <$> get)
  where
    parseAndOp = lexeme $ try (string "&&" Data.Functor.$> (\lhs rhs -> Operation (And lhs rhs)))

parseComparison :: MVParser Expression
parseComparison = chainl1 parseBitwiseOr parseComparisonOp >>= evaluateOperations (getCollapseOperations <$> get)
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
parseBitwiseOr = chainl1 parseBitwiseXor parseBitwiseOrOp >>= evaluateOperations (getCollapseOperations <$> get)
  where
    parseBitwiseOrOp = lexeme $ try (string "b|" Data.Functor.$> (\lhs rhs -> Operation (BitwiseOr lhs rhs)))

parseBitwiseXor :: MVParser Expression
parseBitwiseXor = chainl1 parseBitwiseAnd parseBitwiseXorOp >>= evaluateOperations (getCollapseOperations <$> get)
  where
    parseBitwiseXorOp = lexeme $ try (char '^' Data.Functor.$> (\lhs rhs -> Operation (BitwiseXor lhs rhs)))

parseBitwiseAnd :: MVParser Expression
parseBitwiseAnd = chainl1 parseAddSub parseBitwiseAndOp >>= evaluateOperations (getCollapseOperations <$> get)
  where
    parseBitwiseAndOp = lexeme $ try (string "b&" Data.Functor.$> (\lhs rhs -> Operation (BitwiseAnd lhs rhs)))

parseAddSub :: MVParser Expression
parseAddSub = chainl1 parseMulDivMod parseAddSubOp >>= evaluateOperations (getCollapseOperations <$> get)
  where
    parseAddSubOp =
        lexeme $
            try (char '+' Data.Functor.$> (\lhs rhs -> Operation (Add lhs rhs)))
                <|> try (char '-' Data.Functor.$> (\lhs rhs -> Operation (Subtract lhs rhs)))

parseMulDivMod :: MVParser Expression
parseMulDivMod = chainl1 parseTerm parseMulDivModOp >>= evaluateOperations (getCollapseOperations <$> get)
  where
    parseMulDivModOp =
        lexeme $
            try (char '*' Data.Functor.$> (\lhs rhs -> Operation (Multiply lhs rhs)))
                <|> try (string "//" Data.Functor.$> (\lhs rhs -> Operation (IntDivide lhs rhs)))
                <|> try (char '/' Data.Functor.$> (\lhs rhs -> Operation (Divide lhs rhs)))
                <|> try (char '%' Data.Functor.$> (\lhs rhs -> Operation (Modulo lhs rhs)))
