module ParserSpec (spec) where

import Control.Monad (forM_)
import Data.Text as T
import Parser
import ParserTypes
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (ParseErrorBundle, SourcePos, eof, initialPos, runParser)

testParse :: MVParser a -> String -> Either (ParseErrorBundle String MVParseError) a
testParse p input = runParser p "" input

dummyPos :: SourcePos
dummyPos = initialPos ""

spec :: Spec
spec = do
  describe "number" $ do
    it "parses an int" $ do
      testParse number "87"
        `shouldParse` Literal dummyPos (Int 87)
    it "parses a float" $ do
      testParse number "8.7"
        `shouldParse` Literal dummyPos (Float 8.7)
    it "does not parse letters" $ do
      testParse number
        `shouldFailOn` "string"
  describe "stringLiteral" $ do
    it "parses a string" $ do
      testParse stringLiteral "\"test\""
        `shouldParse` Literal dummyPos (String (T.pack "test"))
    it "does not parse incomplete string" $ do
      testParse stringLiteral
        `shouldFailOn` "\"test"
  describe "bool" $ do
    it "parses true" $ do
      testParse bool "true"
        `shouldParse` Literal dummyPos (Bool True)
    it "parses false" $ do
      testParse bool "false"
        `shouldParse` Literal dummyPos (Bool False)
  describe "array" $ do
    it "parses an array" $ do
      testParse array "[0, 1, 2]"
        `shouldParse` Literal dummyPos (Array [Literal dummyPos (Int 0), Literal dummyPos (Int 1), Literal dummyPos (Int 2)])
    it "does not parse incomplete array" $ do
      testParse array
        `shouldFailOn` "[0, 1, "
  describe "vector" $ do
    it "parses a vector" $ do
      testParse vector "vector(0, 1)"
        `shouldParse` Literal dummyPos (Vector [Literal dummyPos (Int 0), Literal dummyPos (Int 1)])
    it "does not parse incomplete vector" $ do
      testParse vector
        `shouldFailOn` "vector(0"
  describe "point" $ do
    it "parses a point" $ do
      testParse point "point(0, 1)"
        `shouldParse` Literal dummyPos (Point [Literal dummyPos (Int 0), Literal dummyPos (Int 1)])
    it "does not parse incomplete point" $ do
      testParse point
        `shouldFailOn` "point(0"
  describe "matrix" $ do
    it "parses a matrix" $ do
      testParse matrix "matrix([1, 0], [0, 1])"
        `shouldParse` Literal dummyPos (Matrix [Literal dummyPos (Array [Literal dummyPos (Int 1), Literal dummyPos (Int 0)]), Literal dummyPos (Array [Literal dummyPos (Int 0), Literal dummyPos (Int 1)])])
    it "does not parse incomplete matrix" $ do
      testParse matrix
        `shouldFailOn` "matrix([0, 1]"
  describe "literal" $ do
    it "parses an int" $ do
      testParse literal "5"
        `parseSatisfies` isIntLiteral
    it "parses a float" $ do
      testParse literal "5.5"
        `parseSatisfies` isFloatLiteral
    it "parses a string" $ do
      testParse literal "\"test\""
        `parseSatisfies` isStringLiteral
    it "parses a bool" $ do
      testParse literal "false"
        `parseSatisfies` isBoolLiteral
    it "parses an array" $ do
      testParse literal "[0, 1, 2, 3]"
        `parseSatisfies` isArrayLiteral
    it "parses a vector" $ do
      testParse literal "vector(0, 1, 2)"
        `parseSatisfies` isVectorLiteral
    it "parses a point" $ do
      testParse literal "point(0, 1, 2)"
        `parseSatisfies` isPointLiteral
    it "parses a matrix" $ do
      testParse literal "matrix([0, 1], [1, 0])"
        `parseSatisfies` isMatrixLiteral
  describe "parens" $ do
    it "parses one level of parens" $ do
      testParse parens "(0)"
        `shouldParse` Parentheses dummyPos (Literal dummyPos (Int 0))
    it "parses multiple levels of parens" $ do
      testParse parens "((0))"
        `shouldParse` Parentheses dummyPos (Parentheses dummyPos (Literal dummyPos (Int 0)))
    it "does not parse unclosed parens" $ do
      testParse parens
        `shouldFailOn` "(0"
    it "does not parse mismatched parens" $ do
      testParse parens
        `shouldFailOn` "(((0))"
  describe "identifier" $ do
    it "parses identifier with only letters" $ do
      testParse identifier "id"
        `shouldParse` Identifier dummyPos (T.pack "id")
    it "parses identifier starting with underscore" $ do
      testParse identifier "_id"
        `shouldParse` Identifier dummyPos (T.pack "_id")
    it "parses identifier with numbers after first character" $ do
      testParse identifier "_id1d"
        `shouldParse` Identifier dummyPos (T.pack "_id1d")
    it "does not parse identifiers with starting numbers" $ do
      testParse identifier
        `shouldFailOn` "0abc"
    it "does not parse identifiers with starting symbols" $ do
      testParse identifier
        `shouldFailOn` "?abc"
    it "does not parse reserved keywords as identifiers" $
      forM_ reservedKeywords $ \kw ->
        testParse identifier
          `shouldFailOn` kw
    it "parses identifiers that begin with keywords" $
      forM_ reservedKeywords $ \kw ->
        testParse identifier (kw ++ "identifier")
          `shouldParse` Identifier dummyPos (T.pack (kw ++ "identifier"))
  describe "typeName" $ do
    it "parses int type" $ do
      testParse typeName "int"
        `shouldParse` IntT
    it "parses float type" $ do
      testParse typeName "float"
        `shouldParse` FloatT
    it "parses string type" $ do
      testParse typeName "string"
        `shouldParse` StringT
    it "parses bool type" $ do
      testParse typeName "bool"
        `shouldParse` BoolT
    it "parses vector type" $ do
      testParse typeName "vector"
        `shouldParse` VectorT
    it "parses point type" $ do
      testParse typeName "point"
        `shouldParse` PointT
    it "parses matrix type" $ do
      testParse typeName "matrix"
        `shouldParse` MatrixT
    it "parses array type" $ do
      testParse typeName "[int]"
        `shouldParse` ArrayT IntT
    it "parses nested array type" $ do
      testParse typeName "[[int]]"
        `shouldParse` ArrayT (ArrayT IntT)
    it "parses array with lambda" $ do
      testParse typeName "[lambda[]]"
        `shouldParse` ArrayT (LambdaT [] VoidT)
    it "does not parse incomplete array type" $ do
      testParse typeName
        `shouldFailOn` "[int"
    it "does not parse mismatched brackets array type" $ do
      testParse typeName
        `shouldFailOn` "[[int]"
    it "parses empty lambda" $ do
      testParse typeName "lambda[]"
        `shouldParse` LambdaT [] VoidT
    it "parses lambda with no arguments" $ do
      testParse typeName "lambda[]string"
        `shouldParse` LambdaT [] StringT
    it "parses lambda with arguments" $ do
      testParse typeName "lambda[int, bool, float]string"
        `shouldParse` LambdaT [IntT, BoolT, FloatT] StringT
    it "parses lambda with nested array argument" $ do
      testParse typeName "lambda[[[int]]]string"
        `shouldParse` LambdaT [ArrayT (ArrayT IntT)] StringT
    it "parses lambda with nested array return type" $ do
      testParse typeName "lambda[][[int]]"
        `shouldParse` LambdaT [] (ArrayT (ArrayT IntT))
    it "parses lambda with lambda argument" $ do
      testParse typeName "lambda[lambda[]]"
        `shouldParse` LambdaT [LambdaT [] VoidT] VoidT
    it "parses lambda with lambda return type" $ do
      testParse typeName "lambda[]lambda[]string"
        `shouldParse` LambdaT [] (LambdaT [] StringT)
    it "does not parse incomplete lambda" $ do
      testParse typeName
        `shouldFailOn` "lambda["
  describe "returnStmt" $ do
    it "parses empty return" $ do
      testParse returnStmt "return"
        `shouldParse` Return dummyPos Nothing
    it "parses non-empty return" $ do
      testParse returnStmt "return 1"
        `shouldParse` Return dummyPos (Just (Literal dummyPos (Int 1)))
    it "parses non-empty return with parens" $ do
      testParse returnStmt "return (1)"
        `shouldParse` Return dummyPos (Just (Parentheses dummyPos (Literal dummyPos (Int 1))))
    it "does not parse non-expression" $ do
      testParse (returnStmt <* eof)
        `shouldFailOn` "return int"
  describe "block" $ do
    it "parses empty block" $ do
      testParse block "{}"
        `shouldParse` Block dummyPos []
    it "parses block with expression" $ do
      testParse block "{\n0\n}"
        `shouldParse` Block dummyPos [Expr (Literal dummyPos (Int 0))]
    it "parses block with statement" $ do
      testParse block "{\nreturn\n}"
        `shouldParse` Block dummyPos [Stmt (Return dummyPos Nothing)]
    it "does not parse unclosed block" $ do
      testParse block
        `shouldFailOn` "{"
  describe "lambda" $ do
    it "parses empty lambda" $ do
      testParse lambda "():{}"
        `shouldParse` LambdaFunc dummyPos [] (Block dummyPos [])
    it "parses empty lambda with args" $ do
      testParse lambda "(a: int):{}"
        `shouldParse` LambdaFunc dummyPos [(Identifier dummyPos (T.pack "a"), IntT)] (Block dummyPos [])
    it "parses lambda with expression" $ do
      testParse lambda "():0"
        `shouldParse` LambdaFunc dummyPos [] (Expr (Literal dummyPos (Int 0)))
    it "parses lambda with statement" $ do
      testParse lambda "():return"
        `shouldParse` LambdaFunc dummyPos [] (Stmt (Return dummyPos Nothing))
    it "does not parse lambda without body" $ do
      testParse lambda
        `shouldFailOn` "():"
  describe "lambdaApplication" $ do
    it "parses application" $ do
      testParse lambdaApplication "((a:int):a)(0)"
        `shouldParse` LambdaApplication dummyPos (LambdaFunc dummyPos [(Identifier dummyPos (T.pack "a"), IntT)] (Expr (Identifier dummyPos (T.pack "a")))) (Literal dummyPos (Int 0))
    it "does not parse if no application" $ do
      testParse lambdaApplication
        `shouldFailOn` "((a:int):a)"
    it "does not parse if application is statement" $ do
      testParse lambdaApplication
        `shouldFailOn` "((a:int):a)(return)"
    it "does not parse if application is not inline lambda" $ do
      testParse lambdaApplication
        `shouldFailOn` "(a)(return)"
  describe "ifStmt" $ do
    it "parses empty if statement" $ do
      testParse ifStmt "if () {}"
        `shouldParse` IfStmt dummyPos Nothing (Block dummyPos []) Nothing
    it "parses if statement with condition" $ do
      testParse ifStmt "if (true) {}"
        `shouldParse` IfStmt dummyPos (Just (Literal dummyPos (Bool True))) (Block dummyPos []) Nothing
    it "parses if statement with body" $ do
      testParse ifStmt "if (true) {\nreturn\n}"
        `shouldParse` IfStmt dummyPos (Just (Literal dummyPos (Bool True))) (Block dummyPos [Stmt (Return dummyPos Nothing)]) Nothing
    it "parses if statement with else" $ do
      testParse ifStmt "if (true) {\nreturn\n} else {}"
        `shouldParse` IfStmt dummyPos (Just (Literal dummyPos (Bool True))) (Block dummyPos [Stmt (Return dummyPos Nothing)]) (Just (Stmt (ElseStmt dummyPos (Block dummyPos []))))
    it "parses if statement with else if" $ do
      testParse ifStmt "if (true) {\nreturn\n} else if () {}"
        `shouldParse` IfStmt dummyPos (Just (Literal dummyPos (Bool True))) (Block dummyPos [Stmt (Return dummyPos Nothing)]) (Just (Stmt (ElseStmt dummyPos (Stmt (IfStmt dummyPos Nothing (Block dummyPos []) Nothing)))))
    it "parses if statement with else if and else" $ do
      testParse ifStmt "if (true) {\nreturn\n} else if () {} else {}"
        `shouldParse` IfStmt dummyPos (Just (Literal dummyPos (Bool True))) (Block dummyPos [Stmt (Return dummyPos Nothing)]) (Just (Stmt (ElseStmt dummyPos (Stmt (IfStmt dummyPos Nothing (Block dummyPos []) (Just (Stmt (ElseStmt dummyPos (Block dummyPos [])))))))))
    it "does not parse if with no body" $ do
      testParse ifStmt
        `shouldFailOn` "if (true)"
    it "does not parse if with no condition" $ do
      testParse ifStmt
        `shouldFailOn` "if {}"
  describe "elseStmt" $ do
    it "parses empty else" $ do
      testParse elseStmt "else {}"
        `shouldParse` ElseStmt dummyPos (Block dummyPos [])
    it "parses else with body" $ do
      testParse elseStmt "else {\nreturn\n}"
        `shouldParse` ElseStmt dummyPos (Block dummyPos [Stmt (Return dummyPos Nothing)])
    it "parses else if" $ do
      testParse elseStmt "else if () {}"
        `shouldParse` ElseStmt dummyPos (Stmt (IfStmt dummyPos Nothing (Block dummyPos []) Nothing))
    it "parses else if and else" $ do
      testParse elseStmt "else if () {} else {}"
        `shouldParse` ElseStmt dummyPos (Stmt (IfStmt dummyPos Nothing (Block dummyPos []) (Just (Stmt (ElseStmt dummyPos (Block dummyPos []))))))
    it "does not parse else with no body" $ do
      testParse elseStmt
        `shouldFailOn` "else"
  describe "functionParameters" $ do
    it "parses no parameters" $ do
      testParse functionParameters ""
        `shouldParse` []
    it "parses one parameter" $ do
      testParse functionParameters "a: int"
        `shouldParse` [(Identifier dummyPos (T.pack "a"), IntT)]
    it "parses multiple parameters" $ do
      testParse functionParameters "a: int, b: float"
        `shouldParse` [(Identifier dummyPos (T.pack "a"), IntT), (Identifier dummyPos (T.pack "b"), FloatT)]
    it "does not parse if no type" $ do
      testParse functionParameters
        `shouldFailOn` "a, b"
    it "does not parse if rhs is not type" $ do
      testParse functionParameters
        `shouldFailOn` "a: if"
    it "does not parse if lhs is keyword" $ do
      testParse (functionParameters <* eof)
        `shouldFailOn` "if: int"
  describe "functionSignature" $ do
    it "parses signature" $ do
      testParse functionSignature "func f(a: int) bool"
        `shouldParse` (Identifier dummyPos (T.pack "f"), [(Identifier dummyPos (T.pack "a"), IntT)], BoolT)
    it "parses signature with void return" $ do
      testParse functionSignature "func f(a: int)"
        `shouldParse` (Identifier dummyPos (T.pack "f"), [(Identifier dummyPos (T.pack "a"), IntT)], VoidT)
    it "does not parse signature with keyword identifier" $ do
      testParse (functionSignature <* eof)
        `shouldFailOn` "func else(a: int)"
    it "does not parse signature with keyword return type" $ do
      testParse (functionSignature <* eof)
        `shouldFailOn` "func f(a: int) else"
  describe "functionDeclaration" $ do
    it "parses empty function" $ do
      testParse functionDeclaration "func f() {}"
        `shouldParse` FunctionDef dummyPos (Identifier dummyPos (T.pack "f")) [] VoidT (Just (Block dummyPos []))
    it "parses function with body" $ do
      testParse functionDeclaration "func f() {\nreturn\n}"
        `shouldParse` FunctionDef dummyPos (Identifier dummyPos (T.pack "f")) [] VoidT (Just (Block dummyPos [Stmt (Return dummyPos Nothing)]))
    it "parses forward declaration" $ do
      testParse functionDeclaration "func f()"
        `shouldParse` FunctionDef dummyPos (Identifier dummyPos (T.pack "f")) [] VoidT Nothing
  describe "functionCallArguments" $ do
    it "parses no args" $ do
      testParse functionCallArguments ""
        `shouldParse` []
    it "parses one arg" $ do
      testParse functionCallArguments "0"
        `shouldParse` [Literal dummyPos (Int 0)]
    it "parses multiple args" $ do
      testParse functionCallArguments "0, 1"
        `shouldParse` [Literal dummyPos (Int 0), Literal dummyPos (Int 1)]
    it "does not parse if arg is keyword" $ do
      testParse (functionCallArguments <* eof)
        `shouldFailOn` "if"
  describe "functionCall" $ do
    it "parses call" $ do
      testParse functionCall "f()"
        `shouldParse` FunctionCall dummyPos (Identifier dummyPos (T.pack "f")) []
    it "does not parse if identifier is keyword" $ do
      testParse functionCall
        `shouldFailOn` "if()"
  describe "varDeclaration" $ do
    it "parses var declaration" $
      do
        testParse varDeclaration "let a: int"
        `shouldParse` Variable dummyPos (Identifier dummyPos (T.pack "a")) (Just IntT) Nothing
    it "does not parse if identifier is keyword" $
      do
        testParse varDeclaration
        `shouldFailOn` "let if: int"
    it "does not parse if type is keyword" $
      do
        testParse varDeclaration
        `shouldFailOn` "let a: else"
    it "parses var declaration with initializer" $ do
      testParse varDeclaration "let a: int = 0"
        `shouldParse` Variable dummyPos (Identifier dummyPos (T.pack "a")) (Just IntT) (Just (Literal dummyPos (Int 0)))
    it "parses var initialization with no type" $ do
      testParse varDeclaration "let a = 3"
        `shouldParse` Variable dummyPos (Identifier dummyPos (T.pack "a")) Nothing (Just (Literal dummyPos (Int 3)))
    it "does not parse if identifier is keyword" $ do
      testParse varDeclaration
        `shouldFailOn` "let if: int = 0"
    it "does not parse if type is keyword" $ do
      testParse varDeclaration
        `shouldFailOn` "let a: else = 3"
  describe "assignment" $ do
    it "parses normal assignment" $ do
      testParse assignment "a = 3"
        `shouldParse` Assignment dummyPos (Assign (Identifier dummyPos (T.pack "a")) (Literal dummyPos (Int 3)))
    it "parses add assignment" $ do
      testParse assignment "a += 3"
        `shouldParse` Assignment dummyPos (AddAssign (Identifier dummyPos (T.pack "a")) (Literal dummyPos (Int 3)))
    it "parses sub assignment" $ do
      testParse assignment "a -= 3"
        `shouldParse` Assignment dummyPos (SubAssign (Identifier dummyPos (T.pack "a")) (Literal dummyPos (Int 3)))
    it "parses mul assignment" $ do
      testParse assignment "a *= 3"
        `shouldParse` Assignment dummyPos (MulAssign (Identifier dummyPos (T.pack "a")) (Literal dummyPos (Int 3)))
    it "parses div assignment" $ do
      testParse assignment "a /= 3"
        `shouldParse` Assignment dummyPos (DivAssign (Identifier dummyPos (T.pack "a")) (Literal dummyPos (Int 3)))
    it "parses int div assignment" $ do
      testParse assignment "a //= 3"
        `shouldParse` Assignment dummyPos (IntDivAssign (Identifier dummyPos (T.pack "a")) (Literal dummyPos (Int 3)))
    it "parses mod assignment" $ do
      testParse assignment "a %= 3"
        `shouldParse` Assignment dummyPos (ModAssign (Identifier dummyPos (T.pack "a")) (Literal dummyPos (Int 3)))
    it "parses bitwise or assignment" $ do
      testParse assignment "a |= 3"
        `shouldParse` Assignment dummyPos (BitwiseOrAssign (Identifier dummyPos (T.pack "a")) (Literal dummyPos (Int 3)))
    it "parses bitwise and assignment" $ do
      testParse assignment "a &= 3"
        `shouldParse` Assignment dummyPos (BitwiseAndAssign (Identifier dummyPos (T.pack "a")) (Literal dummyPos (Int 3)))
    it "parses bitwise xor assignment" $ do
      testParse assignment "a ^= 3"
        `shouldParse` Assignment dummyPos (BitwiseXorAssign (Identifier dummyPos (T.pack "a")) (Literal dummyPos (Int 3)))
    it "does not parse if lhs is keyword" $ do
      testParse assignment
        `shouldFailOn` "if = 4"
    it "does not parse if rhs is keyword" $ do
      testParse assignment
        `shouldFailOn` "a = else"
    it "does not parse if no rhs" $ do
      testParse assignment
        `shouldFailOn` "a = "
  describe "term" $ do
    it "parses lambda" $ do
      testParse term "():{}"
        `parseSatisfies` isLambda
    it "parses lambda application" $ do
      testParse term "((a:int):a)(0)"
        `parseSatisfies` isLambdaApplication
    it "parses function call" $ do
      testParse term "a(0)"
        `parseSatisfies` isFunctionCall
    it "parses literal" $ do
      testParse term "5"
        `parseSatisfies` isLiteral
    it "parses parentheses" $ do
      testParse term "(0)"
        `parseSatisfies` isParentheses
    it "parses identifier" $ do
      testParse term "a"
        `parseSatisfies` isIdentifier
  describe "statement" $ do
    it "parses var initialization" $ do
      testParse statement "let a = 0"
        `parseSatisfies` isVarInitialization
    it "parses var declaration" $ do
      testParse statement "let a: int"
        `parseSatisfies` isVarDeclaration
    it "parses assignment" $ do
      testParse statement "a += 1"
        `parseSatisfies` isAssignment
    it "parses forward declaration" $ do
      testParse statement "func f()"
        `parseSatisfies` isForwardDeclaration
    it "parses return" $ do
      testParse statement "return"
        `parseSatisfies` isReturn
    it "parses function definition" $ do
      testParse statement "func f() {}"
        `parseSatisfies` isFunctionDefinition
    it "parses if statement" $ do
      testParse statement "if () {}"
        `parseSatisfies` isIfStatement
    describe "expr" $ do
      describe "binary operators" $ do
        let cases =
              [ ("1 + 2", Operation dummyPos (Add (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))),
                ("1 - 2", Operation dummyPos (Subtract (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))),
                ("1 * 2", Operation dummyPos (Multiply (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))),
                ("1 / 2", Operation dummyPos (Divide (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))),
                ("1 // 2", Operation dummyPos (IntDivide (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))),
                ("1 % 2", Operation dummyPos (Modulo (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))),
                ("1 & 2", Operation dummyPos (BitwiseAnd (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))),
                ("1 | 2", Operation dummyPos (BitwiseOr (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))),
                ("1 ^ 2", Operation dummyPos (BitwiseXor (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))),
                ("1 >= 2", Operation dummyPos (GreaterThanEq (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))),
                ("1 <= 2", Operation dummyPos (LessThanEq (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))),
                ("1 == 2", Operation dummyPos (Equals (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))),
                ("1 != 2", Operation dummyPos (NotEquals (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))),
                ("1 > 2", Operation dummyPos (GreaterThan (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))),
                ("1 < 2", Operation dummyPos (LessThan (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))),
                ("1 && 2", Operation dummyPos (And (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))),
                ("1 || 2", Operation dummyPos (Or (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2))))
              ]
        forM_ cases $ \(input, expected) ->
          it ("parses " ++ input) $
            testParse expr input `shouldParse` expected
      describe "prefix operators" $ do
        let cases =
              [ ("-1", Operation dummyPos (Negation (Literal dummyPos (Int 1)))),
                ("!1", Operation dummyPos (Not (Literal dummyPos (Int 1)))),
                ("~1", Operation dummyPos (BitwiseNot (Literal dummyPos (Int 1))))
              ]
        forM_ cases $ \(input, expected) ->
          it ("parses " ++ input) $
            testParse expr input `shouldParse` expected
      describe "precedence" $ do
        it "multiplication binds tighter than addition" $
          testParse expr "1 + 2 * 3"
            `shouldParse` Operation dummyPos (Add (Literal dummyPos (Int 1)) (Operation dummyPos (Multiply (Literal dummyPos (Int 2)) (Literal dummyPos (Int 3)))))

        it "addition binds tighter than bitwise and" $
          testParse expr "1 & 2 + 3"
            `shouldParse` Operation dummyPos (BitwiseAnd (Literal dummyPos (Int 1)) (Operation dummyPos (Add (Literal dummyPos (Int 2)) (Literal dummyPos (Int 3)))))

        it "bitwise and binds tighter than bitwise or" $
          testParse expr "1 | 2 & 3"
            `shouldParse` Operation dummyPos (BitwiseOr (Literal dummyPos (Int 1)) (Operation dummyPos (BitwiseAnd (Literal dummyPos (Int 2)) (Literal dummyPos (Int 3)))))

        it "bitwise or binds tighter than comparison" $
          testParse expr "1 == 2 | 3"
            `shouldParse` Operation dummyPos (Equals (Literal dummyPos (Int 1)) (Operation dummyPos (BitwiseOr (Literal dummyPos (Int 2)) (Literal dummyPos (Int 3)))))

        it "comparison binds tighter than &&" $
          testParse expr "1 && 2 == 3"
            `shouldParse` Operation dummyPos (And (Literal dummyPos (Int 1)) (Operation dummyPos (Equals (Literal dummyPos (Int 2)) (Literal dummyPos (Int 3)))))

        it "&& binds tighter than ||" $
          testParse expr "1 || 2 && 3"
            `shouldParse` Operation dummyPos (Or (Literal dummyPos (Int 1)) (Operation dummyPos (And (Literal dummyPos (Int 2)) (Literal dummyPos (Int 3)))))
      describe "associativity" $ do
        it "subtraction is left-associative" $
          testParse expr "1 - 2 - 3"
            `shouldParse` Operation dummyPos (Subtract (Operation dummyPos (Subtract (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))) (Literal dummyPos (Int 3)))
      describe "operator tokenization disambiguation" $ do
        it "parses >= as one operator, not > followed by =" $
          testParse expr "1 >= 2"
            `shouldParse` Operation dummyPos (GreaterThanEq (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))

        it "parses && as one operator, not & followed by &" $
          testParse expr "1 && 2"
            `shouldParse` Operation dummyPos (And (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))

        it "parses // as one operator, not / followed by /" $
          testParse expr "1 // 2"
            `shouldParse` Operation dummyPos (IntDivide (Literal dummyPos (Int 1)) (Literal dummyPos (Int 2)))
  where
    isVarInitialization (Variable _ _ _ (Just _)) = True
    isVarInitialization _ = False
    isVarDeclaration (Variable _ _ _ Nothing) = True
    isVarDeclaration _ = False
    isAssignment (Assignment _ _) = True
    isAssignment _ = False
    isForwardDeclaration (FunctionDef _ _ _ _ Nothing) = True
    isForwardDeclaration _ = False
    isFunctionDefinition (FunctionDef _ _ _ _ (Just _)) = True
    isFunctionDefinition _ = False
    isReturn (Return _ _) = True
    isReturn _ = False
    isIfStatement (IfStmt {}) = True
    isIfStatement _ = False
    isLambda (LambdaFunc {}) = True
    isLambda _ = False
    isLambdaApplication (LambdaApplication {}) = True
    isLambdaApplication _ = False
    isFunctionCall (FunctionCall {}) = True
    isFunctionCall _ = False
    isLiteral (Literal _ _) = True
    isLiteral _ = False
    isParentheses (Parentheses _ _) = True
    isParentheses _ = False
    isIdentifier (Identifier _ _) = True
    isIdentifier _ = False
    isIntLiteral (Literal _ (Int _)) = True
    isIntLiteral _ = False
    isFloatLiteral (Literal _ (Float _)) = True
    isFloatLiteral _ = False
    isStringLiteral (Literal _ (String _)) = True
    isStringLiteral _ = False
    isBoolLiteral (Literal _ (Bool _)) = True
    isBoolLiteral _ = False
    isArrayLiteral (Literal _ (Array _)) = True
    isArrayLiteral _ = False
    isVectorLiteral (Literal _ (Vector _)) = True
    isVectorLiteral _ = False
    isPointLiteral (Literal _ (Point _)) = True
    isPointLiteral _ = False
    isMatrixLiteral (Literal _ (Matrix _)) = True
    isMatrixLiteral _ = False
