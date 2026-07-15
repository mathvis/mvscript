module Parser (module Parser) where

import Control.Monad.Combinators.Expr
import Data.Functor
import Data.Text as T (pack)
import Misc
import ParserTypes
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- MAIN TYPE PARSERS
topLevel :: MVParser TopLevel
topLevel =
  choice
    [ Stmt <$> statement <?> "statement",
      Expr <$> expr <?> "expression"
    ]

expr :: MVParser Expression
expr = makeExprParser term operatorTable

literal :: MVParser Expression
literal =
  choice
    [ number <?> "number",
      bool <?> "bool",
      array <?> "array",
      stringLiteral <?> "string",
      vector <?> "vector",
      point <?> "point",
      matrix <?> "matrix"
    ]

term :: MVParser Expression
term =
  choice
    [ try lambdaApplication <?> "lambda application",
      try lambda <?> "lambda",
      parens <?> "parentheses",
      try functionCall <?> "function call",
      try identifier <?> "identifier",
      literal <?> "literal"
    ]

statement :: MVParser Statement
statement =
  choice
    [ varDeclaration <?> "variable declaration",
      functionDeclaration <?> "function declaration",
      returnStmt <?> "return",
      ifStmt <?> "if statement",
      try assignment <?> "assignment"
    ]

-- DATA TYPE PARSERS

number :: MVParser Expression
number = lexeme $ do
  pos <- getSourcePos
  intPart <- some digitChar
  fracPart <- optional (char '.' *> some digitChar)
  return $ case fracPart of
    Nothing -> Literal pos $ Int (read intPart)
    Just frac -> Literal pos $ Float (read (intPart ++ "." ++ frac))

stringLiteral :: MVParser Expression
stringLiteral = lexeme $ do
  pos <- getSourcePos
  Literal pos . String . T.pack <$> stringContents
  where
    stringContents = char '"' *> manyTill L.charLiteral (char '"')

bool :: MVParser Expression
bool = do
  pos <- getSourcePos
  Literal pos . Bool <$> (parseTrue <|> parseFalse)
  where
    parseTrue = True <$ rword "true"
    parseFalse = False <$ rword "false"

array :: MVParser Expression
array = do
  pos <- getSourcePos
  Literal pos . Array <$> between (symbol "[") (symbol "]") (sepBy expr (symbol ","))

structLikeType :: String -> MVParser a -> MVParser [a]
structLikeType keyword elementParser =
  rword keyword *> betweenParentheses (sepBy elementParser (symbol ","))

vector :: MVParser Expression
vector = do
  pos <- getSourcePos
  Literal pos . Vector <$> structLikeType "Vector" expr

point :: MVParser Expression
point = do
  pos <- getSourcePos
  Literal pos . Point <$> structLikeType "Point" expr

matrix :: MVParser Expression
matrix = do
  pos <- getSourcePos
  Literal pos . Matrix <$> structLikeType "Matrix" array

-- VARIABLE PARSERS
identifier :: MVParser Expression
identifier = do
  pos <- getSourcePos
  Identifier pos . T.pack <$> mkIdentifier
  where
    firstChar = letterChar <|> char '_'
    nextChar = firstChar <|> digitChar
    mkIdentifier = lexeme $ do
      name <- liftA2 (:) firstChar (many nextChar)
      if name `elem` reservedKeywords
        then customFailure (ReservedKeywordUsed name)
        else return name

varDeclaration :: MVParser Statement
varDeclaration = do
  pos <- getSourcePos
  name <- rword "let" *> identifier
  maybeType <- optional (symbol ":" *> typeName)
  maybeExpr <- optional (symbol "=" *> expr)
  return (Variable pos name maybeType maybeExpr)

voidType :: MVParser ParserType
voidType = pure VoidT

typeName :: MVParser ParserType
typeName =
  choice
    [ parseIntTName,
      parseStringTName,
      parseFloatTName,
      parseBoolTName,
      parseVectorTName,
      parseMatrixTName,
      parsePointTName,
      parseArrayTName,
      parseLambdaTName
    ]
  where
    parseIntTName = IntT <$ rword "int"
    parseFloatTName = FloatT <$ rword "float"
    parseBoolTName = BoolT <$ rword "bool"
    parseStringTName = StringT <$ rword "string"
    parsePointTName = PointT <$ rword "point"
    parseVectorTName = VectorT <$ rword "vector"
    parseMatrixTName = MatrixT <$ rword "matrix"
    parseArrayTName = ArrayT <$> (symbol "[" *> typeName <* symbol "]")
    parseLambdaTName = do
      _ <- symbol "lambda"
      _ <- symbol "["
      params <- sepBy typeName (symbol ",")
      _ <- symbol "]"
      ret <- typeName <|> voidType
      return $ LambdaT params ret

-- OPERATION RELATED PARSERS
parens :: MVParser Expression
parens = do
  pos <- getSourcePos
  Parentheses pos <$> betweenParentheses expr

unary :: MVParser Expression
unary = do
  pos <- getSourcePos
  choice
    [ symbol "-" $> (Operation pos . Negation),
      symbol "!" $> (Operation pos . Not),
      symbol "~" $> (Operation pos . BitwiseNot)
    ]
    <*> term

assignment :: MVParser Statement
assignment = do
  leftTerm <- identifier
  mkOp <- choice (map mkAssignOp assignOps)
  rhs <- expr
  pos <- getSourcePos
  return $ Assignment pos (mkOp leftTerm rhs)
  where
    mkAssignOp (sym, ctor) = symbol sym $> ctor
    assignOps =
      [ ("//=", IntDivAssign),
        ("+=", AddAssign),
        ("-=", SubAssign),
        ("*=", MulAssign),
        ("/=", DivAssign),
        ("%=", ModAssign),
        ("|=", BitwiseOrAssign),
        ("&=", BitwiseAndAssign),
        ("^=", BitwiseXorAssign),
        ("=", Assign)
      ]

-- FUNCTION RELATED PARSERS
functionParameters :: MVParser [(Expression, ParserType)]
functionParameters = sepBy parseFunctionArgument (lexeme $ char ',')
  where
    parseFunctionArgument = do
      ident <- identifier
      _ <- symbol ":"
      typename <- typeName
      return (ident, typename)

functionReturnType :: MVParser ParserType
functionReturnType = typeName <|> voidType

functionSignature :: MVParser (Expression, [(Expression, ParserType)], ParserType)
functionSignature = do
  _ <- rword "func"
  ident <- identifier
  _ <- symbol "("
  params <- functionParameters
  _ <- symbol ")"
  ret <- functionReturnType
  return (ident, params, ret)

functionDeclaration :: MVParser Statement
functionDeclaration =
  lexeme $ do
    pos <- getSourcePos
    (funcIdentifier, args, returnType') <- functionSignature
    maybeBlock <- optional (block (FunctionBlock returnType'))
    return $ FunctionDef pos funcIdentifier args returnType' maybeBlock

functionCallArguments :: MVParser [Expression]
functionCallArguments = sepBy expr (lexeme $ char ',')

functionCall :: MVParser Expression
functionCall = lexeme $ do
  ident <- identifier
  args <- between (symbol "(") (symbol ")") functionCallArguments
  pos <- getSourcePos
  return $ FunctionCall pos ident args

lambda :: MVParser Expression
lambda = do
  pos <- getSourcePos
  params <- betweenParentheses functionParameters
  _ <- symbol ":"
  body <- block (FunctionBlock VoidT) <|> topLevel
  return $ LambdaFunc pos params body

lambdaApplication :: MVParser Expression
lambdaApplication = do
  pos <- getSourcePos
  LambdaApplication pos
    <$> betweenParentheses lambda
    <*> betweenParentheses expr

block :: BlockType -> MVParser TopLevel
block blocktype = do
  _ <- symbol "{"
  stmts <- many (lexeme topLevel)
  _ <- symbol "}"
  return $ Block blocktype stmts

returnStmt :: MVParser Statement
returnStmt = do
  pos <- getSourcePos
  lexeme (Return pos <$> (symbol "return" *> optional expr))

-- CONTROL FLOW PARSERS
elseStmt :: MVParser Statement
elseStmt = lexeme $ do
  pos <- getSourcePos
  _ <- rword "else"
  body <- block Else <|> (Stmt <$> ifStmt)
  return $ ElseStmt pos body

ifStmt :: MVParser Statement
ifStmt = lexeme $ do
  pos <- getSourcePos
  _ <- rword "if"
  cond <- betweenParentheses (optional expr)
  body <- block If
  maybeElse <- optional (Stmt <$> elseStmt)
  return $ IfStmt pos cond body maybeElse

binOp :: String -> (Expression -> Expression -> Operation) -> MVParser (Expression -> Expression -> Expression)
binOp sym ctor = do
  pos <- getSourcePos
  _ <- symbol sym
  return (\l r -> Operation pos (ctor l r))

unaryOp :: String -> (Expression -> Operation) -> MVParser (Expression -> Expression)
unaryOp sym ctor = do
  pos <- getSourcePos
  _ <- symbol sym
  return $ Operation pos . ctor

operatorTable :: [[Operator MVParser Expression]]
operatorTable = 
  [ [ Prefix (unaryOp "-" Negation),
      Prefix (unaryOp "!" Not),
      Prefix (unaryOp "~" BitwiseNot)
    ],
    [ InfixL (binOp "*" Multiply),
      InfixL (try (binOp "//" IntDivide)),
      InfixL (binOp "/" Divide),
      InfixL (binOp "%" Modulo)
    ],
    [ InfixL (binOp "+" Add),
      InfixL (binOp "-" Subtract)
    ],
    [InfixL (try (binOp "&" BitwiseAnd <* notFollowedBy (char '&')))],
    [InfixL (try (binOp "|" BitwiseOr <* notFollowedBy (char '|')))],
    [InfixL (binOp "^" BitwiseXor)],
    [ InfixL (try (binOp ">=" GreaterThanEq)),
      InfixL (try (binOp "<=" LessThanEq)),
      InfixL (try (binOp "==" Equals)),
      InfixL (try (binOp "!=" NotEquals)),
      InfixL (binOp ">" GreaterThan),
      InfixL (binOp "<" LessThan)
    ],
    [InfixL (try (binOp "&&" And))],
    [InfixL (try (binOp "||" Or))]
  ]
