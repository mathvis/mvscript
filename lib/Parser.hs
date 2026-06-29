module Parser (module Parser) where

import Control.Monad.Combinators.Expr
import Data.Functor
import Data.Text as T (pack)
import Misc
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types hiding (identifier)

-- MAIN TYPE PARSERS
topLevel :: MVParser TopLevel
topLevel =
  choice
    [ Stmt <$> try statement <?> "statement",
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
    [ try lambda <?> "lambda",
      try lambdaApplication <?> "lambda application",
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
      assignment <?> "assignment"
    ]

-- DATA TYPE PARSERS

number :: MVParser Expression
number = lexeme $ do
  intPart <- some digitChar
  fracPart <- optional (char '.' *> some digitChar)
  return $ case fracPart of
    Nothing -> Literal $ Int (read intPart)
    Just frac -> Literal $ Float (read (intPart ++ "." ++ frac))

stringLiteral :: MVParser Expression
stringLiteral = lexeme (mkString <$> stringContents)
  where
    mkString = Literal . String . T.pack
    stringContents = char '"' *> manyTill L.charLiteral (char '"')

bool :: MVParser Expression
bool = Literal . Bool <$> (parseTrue <|> parseFalse)
  where
    parseTrue = True <$ rword "true"
    parseFalse = False <$ rword "false"

array :: MVParser Expression
array = Literal . Array <$> between (symbol "[") (symbol "]") (sepBy expr (symbol ","))

structLikeType :: String -> MVParser a -> MVParser [a]
structLikeType keyword elementParser =
  rword keyword *> betweenParentheses (sepBy elementParser (symbol ","))

vector :: MVParser Expression
vector = Literal . Vector <$> structLikeType "Vector" expr

point :: MVParser Expression
point = Literal . Point <$> structLikeType "Point" expr

matrix :: MVParser Expression
matrix = Literal . Matrix <$> structLikeType "Matrix" array

-- VARIABLE PARSERS
identifier :: MVParser Expression
identifier =
  Identifier . T.pack <$> mkIdentifier
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
  name <- rword "let" *> identifier
  maybeType <- optional (symbol ":" *> typeName)
  maybeExpr <- optional (symbol "=" *> expr)
  return (Variable name maybeType maybeExpr)

voidType :: MVParser Type
voidType = pure VoidT

typeName :: MVParser Type
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
parens = Parentheses <$> betweenParentheses expr

unary :: MVParser Expression
unary =
  choice
    [ (symbol "-" $> (Operation . Negation)),
      (symbol "!" $> (Operation . Not)),
      (symbol "~" $> (Operation . BitwiseNot))
    ]
    <*> term

assignment :: MVParser Statement
assignment = do
  leftTerm <- identifier
  mkOp <- choice (map mkAssignOp assignOps)
  rhs <- expr
  return $ Assignment (mkOp leftTerm rhs)
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
functionParameters :: MVParser [(Expression, Type)]
functionParameters = sepBy parseFunctionArgument (lexeme $ char ',')
  where
    parseFunctionArgument = do
      ident <- identifier
      _ <- symbol ":"
      typename <- typeName
      return (ident, typename)

functionReturnType :: MVParser Type
functionReturnType = typeName <|> voidType

functionSignature :: MVParser (Expression, [(Expression, Type)], Type)
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
    (funcIdentifier, args, returnType') <- functionSignature
    maybeBlock <- optional (block (FunctionBlock returnType'))
    return $ FunctionDef funcIdentifier args returnType' maybeBlock

functionCallArguments :: MVParser [Expression]
functionCallArguments = sepBy expr (lexeme $ char ',')

functionCall :: MVParser Expression
functionCall = lexeme $ do
  ident <- identifier
  args <- between (symbol "(") (symbol ")") functionCallArguments
  return $ FunctionCall ident args

lambda :: MVParser Expression
lambda = do
  params <- betweenParentheses functionParameters
  _ <- symbol ":"
  body <- block (FunctionBlock VoidT) <|> topLevel
  return $ LambdaFunc params body

lambdaApplication :: MVParser Expression
lambdaApplication =
  LambdaApplication
    <$> betweenParentheses lambda
    <*> betweenParentheses expr

block :: BlockType -> MVParser TopLevel
block blocktype = do
  _ <- symbol "{"
  stmts <- many (lexeme topLevel)
  _ <- symbol "}"
  return $ Block blocktype stmts

returnStmt :: MVParser Statement
returnStmt = lexeme (Return <$> (symbol "return" *> optional expr))

-- CONTROL FLOW PARSERS
elseStmt :: MVParser Statement
elseStmt = lexeme $ do
  _ <- rword "else"
  body <- block Else <|> (Stmt <$> ifStmt)
  return $ ElseStmt body

ifStmt :: MVParser Statement
ifStmt = lexeme $ do
  _ <- rword "if"
  cond <- betweenParentheses (optional expr)
  body <- block If
  maybeElse <- optional (Stmt <$> elseStmt)
  return $ IfStmt cond body maybeElse

operatorTable :: [[Operator MVParser Expression]]
operatorTable =
  [ [ Prefix (Operation . Negation <$ symbol "-"),
      Prefix (Operation . Not <$ symbol "!"),
      Prefix (Operation . BitwiseNot <$ symbol "~")
    ],
    [ InfixL (symbol "*" $> \l r -> Operation (Multiply l r)),
      InfixL (try (symbol "//") $> \l r -> Operation (IntDivide l r)),
      InfixL (symbol "/" $> \l r -> Operation (Divide l r)),
      InfixL (symbol "%" $> \l r -> Operation (Modulo l r))
    ],
    [ InfixL (symbol "+" $> \l r -> Operation (Add l r)),
      InfixL (symbol "-" $> \l r -> Operation (Subtract l r))
    ],
    [InfixL (try (symbol "&" <* notFollowedBy (char '&')) $> \l r -> Operation (BitwiseAnd l r))],
    [InfixL (try (symbol "|" <* notFollowedBy (char '|')) $> \l r -> Operation (BitwiseOr l r))],
    [InfixL (symbol "^" $> \l r -> Operation (BitwiseXor l r))],
    [ InfixL (try (symbol ">=") $> \l r -> Operation (GreaterThanEq l r)),
      InfixL (try (symbol "<=") $> \l r -> Operation (LessThanEq l r)),
      InfixL (try (symbol "==") $> \l r -> Operation (Equals l r)),
      InfixL (try (symbol "!=") $> \l r -> Operation (NotEquals l r)),
      InfixL (symbol ">" $> \l r -> Operation (GreaterThan l r)),
      InfixL (symbol "<" $> \l r -> Operation (LessThan l r))
    ],
    [InfixL (try (symbol "&&") $> \l r -> Operation (And l r))],
    [InfixL (try (symbol "||") $> \l r -> Operation (Or l r))]
  ]
