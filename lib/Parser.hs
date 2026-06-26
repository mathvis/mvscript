{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser (module Parser) where

import Config.ConfigHandler
import Context
import Control.Applicative (liftA2)
import Control.Arrow
import Control.Monad
import Control.Monad.Combinators.Expr
import Control.Monad.State
import Data.Char
import Data.Functor
import Data.Text as T (pack, unpack)
import Debug.Trace
import Eval
import FunctionStorage
import Misc
import Text.Megaparsec
import Text.Megaparsec.Char
import TypeCheck
import Types
import VariableStorage
import Text.Megaparsec.Debug

-- MAIN TYPE PARSERS
parseTopLevel :: MVParser TopLevel
parseTopLevel =
  choice
    [ (Stmt <$> try parseStatement <?> "statement"),
      (Expr <$> try parseExpr <?> "expression")
    ]

parseExpr :: MVParser Expression
parseExpr = do
  collapse <- gets getCollapseOperations
  let fold = if collapse then foldExpression else id
  makeExprParser parseTerm (operatorTable fold)

parseType :: MVParser Expression
parseType = parseNum <|> parseBool <|> parseArray <|> parseString <|> parseVector <|> parsePoint <|> parseMatrix

parseAtom :: MVParser Expression
parseAtom = try parseLambda <|> try parseLambdaApplication <|> try parseFunctionCall <|> try parseType <|> try parseParens <|> parseVarIdentifier

parseTerm :: MVParser Expression
parseTerm = try parseUnary <|> try parseAtom

parseStatement :: MVParser Statement
parseStatement =
  try parseVarInitialization
    <|> try parseVarStatement
    <|> try parseAssign
    <|> try parseFunctionForwardStatement
    <|> try parseFunctionStatement
    <|> try parseReturn
    <|> parseIf

-- DATA TYPE PARSERS

parseNum :: MVParser Expression
parseNum = Type <$> (try parseFloat <|> parseInt)

parseString :: MVParser Expression
parseString = lexeme $ Type . String . T.pack <$> (char '"' *> many (noneOf "\"") <* char '"')

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
parseArray = Type . Array <$> lexeme (char '[' *> sepBy parseExpr (symbol ",") <* char ']')

parseVector :: MVParser Expression
parseVector = Type . Vector <$> (rword "Vector" *> symbol "(" *> sepBy parseExpr (symbol ",") <* symbol ")")

parsePoint :: MVParser Expression
parsePoint = Type . Point <$> (rword "Point" *> symbol "(" *> sepBy parseExpr (symbol ",") <* symbol ")")

parseMatrix :: MVParser Expression
parseMatrix = Type . Matrix <$> (rword "Matrix" *> symbol "(" *> sepBy parseArray (symbol ",") <* symbol ")")

-- VARIABLE PARSERS
identifierName :: MVParser String
identifierName = (lexeme . try) $ do
  name <- (:) <$> (letterChar <|> char '_') <*> many (letterChar <|> char '_' <|> digitChar)
  if name `elem` reservedKeywords
    then fail $ "Cannot use reserved keyword '" ++ name ++ "' as an identifier"
    else return name

parseVarIdentifier :: MVParser Expression
parseVarIdentifier = do
  pos <- getSourcePos
  expr <- VarIdentifier . T.pack <$> identifierName
  modify (checkScope expr pos)
  return expr

parseArgIdentifier :: MVParser Expression
parseArgIdentifier = VarIdentifier . T.pack <$> identifierName

parseVarIdentifierDecl :: MVParser Expression
parseVarIdentifierDecl = VarIdentifier . T.pack <$> identifierName

parseVarStatement :: MVParser Statement
parseVarStatement =
  Variable
    <$> (rword "let" *> parseVarIdentifierDecl)
    <*> (Just <$> (lexeme (char ':') *> parseTypeName))
    <*> pure Nothing
    >>= \decl -> modify (addVariableToTable decl) >> return decl

parseVoidTypeName :: MVParser TypeName
parseVoidTypeName = pure VoidT

parseTypeName :: MVParser TypeName
parseTypeName = parseIntTName <|> parseStringTName <|> parseFloatTName <|> parseBoolTName <|> parseVectorTName <|> parseMatrixTName <|> parsePointTName <|> parseArrayTName <|> parseLambdaTName
  where
    parseIntTName = IntT <$ rword "int"
    parseFloatTName = FloatT <$ rword "float"
    parseBoolTName = BoolT <$ rword "bool"
    parseStringTName = StringT <$ rword "string"
    parsePointTName = PointT <$ rword "point"
    parseVectorTName = VectorT <$ rword "vector"
    parseMatrixTName = MatrixT <$ rword "matrix"
    parseArrayTName = ArrayT <$> try (symbol "[" *> parseTypeName <* symbol "]")
    parseLambdaTName = try $ LambdaT <$> (symbol "lambda[" *> sepBy parseTypeName (symbol ",") <* symbol "]") <*> (parseTypeName <|> parseVoidTypeName)

parseVarInitialization :: MVParser Statement
parseVarInitialization =
  getSourcePos >>= \pos ->
    get >>= \state ->
      ( Variable
          <$> (rword "let" *> parseVarIdentifierDecl)
          <*> optional (symbol ":" *> parseTypeName)
          <*> (symbol "=" *> parseExpr >>= \expr -> return (Just expr))
      )
        >>= (\decl -> modify (addVariableToTable decl) >> return decl) . checkType pos state . inferVariableType pos state

-- OPERATION RELATED PARSERS
parseParens :: MVParser Expression
parseParens = Parentheses <$> betweenParentheses parseExpr

parseUnary :: MVParser Expression
parseUnary =
  ( (symbol "-" $> (Operation . Negation))
      <|> (symbol "!" $> (Operation . Not))
      <|> (symbol "~" $> (Operation . BitwiseNot))
  )
    <*> parseTerm

parseAssign :: MVParser Statement
parseAssign = do
  pos <- getSourcePos
  leftTerm <- parseVarIdentifierDecl
  op <-
    try (symbol "//=")
      $> (\lhs rhs -> Assignment (IntDivAssign lhs rhs))
        <|> try (symbol "+=")
      $> (\lhs rhs -> Assignment (AddAssign lhs rhs))
        <|> try (symbol "-=")
      $> (\lhs rhs -> Assignment (SubAssign lhs rhs))
        <|> try (symbol "*=")
      $> (\lhs rhs -> Assignment (MulAssign lhs rhs))
        <|> try (symbol "/=")
      $> (\lhs rhs -> Assignment (DivAssign lhs rhs))
        <|> try (symbol "%=")
      $> (\lhs rhs -> Assignment (ModAssign lhs rhs))
        <|> try (symbol "|=")
      $> (\lhs rhs -> Assignment (BitwiseOrAssign lhs rhs))
        <|> try (symbol "&=")
      $> (\lhs rhs -> Assignment (BitwiseAndAssign lhs rhs))
        <|> try (symbol "^=")
      $> (\lhs rhs -> Assignment (BitwiseXorAssign lhs rhs))
        <|> try (symbol "=")
      $> (\lhs rhs -> Assignment (Assign lhs rhs))
  modify (checkScope leftTerm pos)
  rhs <- parseExpr
  let decl = op leftTerm rhs
  modify (updateVariableUninitialized pos decl)
  return decl

-- FUNCTION RELATED PARSERS
parseFunctionIdentifier :: MVParser Expression
parseFunctionIdentifier = FunctionIdentifier . T.pack <$> identifierName

parseFunctionArguments :: MVParser [(Expression, TypeName)]
parseFunctionArguments = sepBy parseFunctionArgument (lexeme $ char ',') >>= \args -> modify (addArgumentsToTable args) >> return args
  where
    parseFunctionArgument = (,) <$> (parseArgIdentifier <* symbol ":") <*> parseTypeName

parseFunctionReturnType :: MVParser TypeName
parseFunctionReturnType = parseTypeName <|> parseVoidTypeName

parseFunctionSignature :: MVParser (Expression, [(Expression, TypeName)], TypeName)
parseFunctionSignature =
  (,,)
    <$> (rword "func" *> parseFunctionIdentifier)
    <*> (symbol "(" *> parseFunctionArguments <* symbol ")")
    <*> parseFunctionReturnType

parseFunctionForwardStatement :: MVParser Statement
parseFunctionForwardStatement =
  getSourcePos >>= \pos ->
    lexeme $
      symbol "[fwd]" *> parseFunctionSignature
        >>= \(funcIdentifier, args, returnType) ->
          let forwardDecl = FunctionDef funcIdentifier args returnType Nothing
           in modify (checkForSecondOrderFunction pos >>> addFunctionToTable forwardDecl) >> return forwardDecl

parseFunctionStatement :: MVParser Statement
parseFunctionStatement =
  getSourcePos
    >>= \pos ->
      lexeme $
        parseFunctionSignature
          >>= \(funcIdentifier, args, returnType) ->
            parseBlock (FunctionBlock returnType)
              >>= ( \decl ->
                      modify
                        ( checkForSecondOrderFunction pos
                            >>> checkForReturn decl pos
                            >>> removeArgumentsFromTable decl
                            >>> compareFunctionSignatureToForwardDecl decl pos
                            >>> addFunctionToTable decl
                        )
                        >> return decl
                  )
                . FunctionDef funcIdentifier args returnType
                . Just

parseFunctionCallArguments :: MVParser [Expression]
parseFunctionCallArguments = sepBy parseExpr (lexeme $ char ',')

parseFunctionCall :: MVParser Expression
parseFunctionCall = getSourcePos >>= \pos -> functionCall >>= (\expr -> modify (checkArguments expr pos . checkScope expr pos) >> return expr)
  where
    functionCall = lexeme $ FunctionCall <$> parseFunctionIdentifier <*> between (symbol "(") (symbol ")") parseFunctionCallArguments

-- TODO: fix lambda return types
parseLambda :: MVParser Expression
parseLambda = LambdaFunc <$> betweenParentheses parseFunctionArguments <*> (symbol ":" *> (Just <$> (parseBlock (FunctionBlock VoidT) <|> parseTopLevel))) >>= \expr -> modify (removeArgumentsFromTableLambda expr) >> return expr

parseLambdaApplication :: MVParser Expression
parseLambdaApplication =
  LambdaApplication
    <$> betweenParentheses parseLambda
    <*> betweenParentheses parseExpr

parseBlock :: BlockType -> MVParser TopLevel
parseBlock blocktype = do
  symbol "{"
  modify (changeContext blocktype)
  stmts <- many (lexeme parseTopLevel)
  symbol "}"
  let block = Block blocktype stmts
  modify (removeScopeVariables block . removeContext)
  return block

parseReturn :: MVParser Statement
parseReturn = getSourcePos >>= \pos -> lexeme (Return <$> (symbol "return" *> optional parseExpr)) >>= \expr -> modify (checkReturnType expr pos . checkForBlock pos) >> return expr

-- CONTROL FLOW PARSERS
parseElse :: MVParser Statement
parseElse = lexeme $ ElseBlock <$> (rword "else" *> (parseBlock Else <|> parseTopLevel))

parseIf :: MVParser Statement
parseIf = lexeme (IfBlock <$> (rword "if" *> betweenParentheses parseExpr) <*> (parseBlock If <|> parseTopLevel) <*> optional parseElse) >>= evaluateControlFlow (gets getCollapseControlFlow)

operatorTable :: (Expression -> Expression) -> [[Operator MVParser Expression]]
operatorTable fold =
  [ [ Prefix (Operation . Negation <$ symbol "-" <&> \f x -> fold (f x)),
      Prefix (Operation . Not <$ symbol "!" <&> \f x -> fold (f x)),
      Prefix (Operation . BitwiseNot <$ symbol "~" <&> \f x -> fold (f x))
    ],
    [ InfixL (symbol "*" $> \l r -> fold (Operation (Multiply l r))),
      InfixL (try (symbol "//") $> \l r -> fold (Operation (IntDivide l r))),
      InfixL (symbol "/" $> \l r -> fold (Operation (Divide l r))),
      InfixL (symbol "%" $> \l r -> fold (Operation (Modulo l r)))
    ],
    [ InfixL (symbol "+" $> \l r -> fold (Operation (Add l r))),
      InfixL (symbol "-" $> \l r -> fold (Operation (Subtract l r)))
    ],
    [InfixL (try (symbol "b&") $> \l r -> fold (Operation (BitwiseAnd l r)))],
    [InfixL (try (symbol "b|") $> \l r -> fold (Operation (BitwiseOr l r)))],
    [InfixL (symbol "^" $> \l r -> fold (Operation (BitwiseXor l r)))],
    [ InfixL (try (symbol ">=") $> \l r -> fold (Operation (GreaterThanEq l r))),
      InfixL (try (symbol "<=") $> \l r -> fold (Operation (LessThanEq l r))),
      InfixL (try (symbol "==") $> \l r -> fold (Operation (Equals l r))),
      InfixL (try (symbol "!=") $> \l r -> fold (Operation (NotEquals l r))),
      InfixL (symbol ">" $> \l r -> fold (Operation (GreaterThan l r))),
      InfixL (symbol "<" $> \l r -> fold (Operation (LessThan l r)))
    ],
    [InfixL (try (symbol "&&") $> \l r -> fold (Operation (And l r)))],
    [InfixL (try (symbol "||") $> \l r -> fold (Operation (Or l r)))]
  ]
