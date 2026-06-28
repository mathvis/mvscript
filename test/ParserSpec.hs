module ParserSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.State (evalState)
import Data.Text as T
import Data.Void
import Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (ParseErrorBundle, eof, parse, runParserT)
import Types

testParse :: MVParser a -> String -> Either (ParseErrorBundle String Void) a
testParse p input = evalState (runParserT p "" input) defaultParserState

spec :: Spec
spec = do
  describe "parseNumber" $ do
    it "parses an int" $ do
      testParse parseNumber "87"
        `shouldParse` (Literal (Int 87))
    it "parses a float" $ do
      testParse parseNumber "8.7"
        `shouldParse` (Literal (Float 8.7))
    it "does not parse letters" $ do
      testParse parseNumber
        `shouldFailOn` "string"
  describe "parseString" $ do
    it "parses a string" $ do
      testParse parseString "\"test\""
        `shouldParse` (Literal (String (T.pack "test")))
    it "does not parse incomplete string" $ do
      testParse parseString
        `shouldFailOn` "\"test"
  describe "parseBool" $ do
    it "parses true" $ do
      testParse parseBool "true"
        `shouldParse` (Literal (Bool True))
    it "parses false" $ do
      testParse parseBool "false"
        `shouldParse` (Literal (Bool False))
  describe "parseArray" $ do
    it "parses an array" $ do
      testParse parseArray "[0, 1, 2]"
        `shouldParse` (Literal (Array [Literal (Int 0), Literal (Int 1), Literal (Int 2)]))
    it "does not parse incomplete array" $ do
      testParse parseArray
        `shouldFailOn` "[0, 1, "
  describe "parseVector" $ do
    it "parses a vector" $ do
      testParse parseVector "Vector(0, 1)"
        `shouldParse` (Literal (Vector [Literal (Int 0), Literal (Int 1)]))
    it "does not parse incomplete vector" $ do
      testParse parseVector
        `shouldFailOn` "Vector(0"
  describe "parsePoint" $ do
    it "parses a point" $ do
      testParse parsePoint "Point(0, 1)"
        `shouldParse` (Literal (Point [Literal (Int 0), Literal (Int 1)]))
    it "does not parse incomplete point" $ do
      testParse parsePoint
        `shouldFailOn` "Point(0"
  describe "parseMatrix" $ do
    it "parses a matrix" $ do
      testParse parseMatrix "Matrix([1, 0], [0, 1])"
        `shouldParse` (Literal (Matrix [Literal (Array ([Literal (Int 1), Literal (Int 0)])), Literal (Array ([Literal (Int 0), Literal (Int 1)]))]))
    it "does not parse incomplete matrix" $ do
      testParse parseMatrix
        `shouldFailOn` "Matrix([0, 1]"
  describe "parseLiteral" $ do
    it "parses an int" $ do
      testParse parseLiteral "5"
        `parseSatisfies` isIntLiteral
    it "parses a float" $ do
      testParse parseLiteral "5.5"
        `parseSatisfies` isFloatLiteral
    it "parses a string" $ do
      testParse parseLiteral "\"test\""
        `parseSatisfies` isStringLiteral
    it "parses a bool" $ do
      testParse parseLiteral "false"
        `parseSatisfies` isBoolLiteral
    it "parses an array" $ do
      testParse parseLiteral "[0, 1, 2, 3]"
        `parseSatisfies` isArrayLiteral
    it "parses a vector" $ do
      testParse parseLiteral "Vector(0, 1, 2)"
        `parseSatisfies` isVectorLiteral
    it "parses a point" $ do
      testParse parseLiteral "Point(0, 1, 2)"
        `parseSatisfies` isPointLiteral
    it "parses a matrix" $ do
      testParse parseLiteral "Matrix([0, 1], [1, 0])"
        `parseSatisfies` isMatrixLiteral
  describe "parseParens" $ do
    it "parses one level of parens" $ do
      testParse parseParens "(0)"
        `shouldParse` (Parentheses (Literal (Int 0)))
    it "parses multiple levels of parens" $ do
      testParse parseParens "((0))"
        `shouldParse` (Parentheses (Parentheses (Literal (Int 0))))
    it "does not parse unclosed parens" $ do
      testParse parseParens
        `shouldFailOn` "(0"
    it "does not parse mismatched parens" $ do
      testParse parseParens
        `shouldFailOn` "(((0))"
  describe "parseIdentifier" $ do
    it "parses identifier with only letters" $ do
      testParse parseIdentifier "id"
        `shouldParse` (Identifier (T.pack "id"))
    it "parses identifier starting with underscore" $ do
      testParse parseIdentifier "_id"
        `shouldParse` (Identifier (T.pack "_id"))
    it "parses identifier with numbers after first character" $ do
      testParse parseIdentifier "_id1d"
        `shouldParse` (Identifier (T.pack "_id1d"))
    it "does not parse identifiers with starting numbers" $ do
      testParse parseIdentifier
        `shouldFailOn` "0abc"
    it "does not parse identifiers with starting symbols" $ do
      testParse parseIdentifier
        `shouldFailOn` "?abc"
    it "does not parse reserved keywords as identifiers" $
      forM_ reservedKeywords $ \kw ->
        testParse parseIdentifier
          `shouldFailOn` kw
    it "parses identifiers that begin with keywords" $
      forM_ reservedKeywords $ \kw ->
        testParse parseIdentifier (kw ++ "identifier")
          `shouldParse` (Identifier (T.pack (kw ++ "identifier")))
  describe "parseType" $ do
    it "parses int type" $ do
      testParse parseType "int"
        `shouldParse` IntT
    it "parses float type" $ do
      testParse parseType "float"
        `shouldParse` FloatT
    it "parses string type" $ do
      testParse parseType "string"
        `shouldParse` StringT
    it "parses bool type" $ do
      testParse parseType "bool"
        `shouldParse` BoolT
    it "parses vector type" $ do
      testParse parseType "vector"
        `shouldParse` VectorT
    it "parses point type" $ do
      testParse parseType "point"
        `shouldParse` PointT
    it "parses matrix type" $ do
      testParse parseType "matrix"
        `shouldParse` MatrixT
    it "parses array type" $ do
      testParse parseType "[int]"
        `shouldParse` ArrayT IntT
    it "parses nested array type" $ do
      testParse parseType "[[int]]"
        `shouldParse` ArrayT (ArrayT IntT)
    it "parses array with lambda" $ do
      testParse parseType "[lambda[]]"
        `shouldParse` ArrayT (LambdaT [] VoidT)
    it "does not parse incomplete array type" $ do
      testParse parseType
        `shouldFailOn` "[int"
    it "does not parse mismatched brackets array type" $ do
      testParse parseType
        `shouldFailOn` "[[int]"
    it "parses empty lambda" $ do
      testParse parseType "lambda[]"
        `shouldParse` LambdaT [] VoidT
    it "parses lambda with no arguments" $ do
      testParse parseType "lambda[]string"
        `shouldParse` LambdaT [] StringT
    it "parses lambda with arguments" $ do
      testParse parseType "lambda[int, bool, float]string"
        `shouldParse` LambdaT [IntT, BoolT, FloatT] StringT
    it "parses lambda with nested array argument" $ do
      testParse parseType "lambda[[[int]]]string"
        `shouldParse` LambdaT [ArrayT (ArrayT IntT)] StringT
    it "parses lambda with nested array return type" $ do
      testParse parseType "lambda[][[int]]"
        `shouldParse` LambdaT [] (ArrayT (ArrayT IntT))
    it "parses lambda with lambda argument" $ do
      testParse parseType "lambda[lambda[]]"
        `shouldParse` LambdaT [LambdaT [] VoidT] VoidT
    it "parses lambda with lambda return type" $ do
      testParse parseType "lambda[]lambda[]string"
        `shouldParse` LambdaT [] (LambdaT [] StringT)
    it "does not parse incomplete lambda" $ do
      testParse parseType
        `shouldFailOn` "lambda["
  describe "parseReturn" $ do
    it "parses empty return" $ do
      testParse parseReturn "return"
        `shouldParse` Return Nothing
    it "parses non-empty return" $ do
      testParse parseReturn "return 1"
        `shouldParse` Return (Just (Literal (Int 1)))
    it "parses non-empty return with parens" $ do
      testParse parseReturn "return (1)"
        `shouldParse` Return (Just (Parentheses (Literal (Int 1))))
    it "does not parse non-expression" $ do
      testParse (parseReturn <* eof)
        `shouldFailOn` "return int"
  describe "parseBlock" $ do
    it "parses empty block" $ do
      testParse (parseBlock NoType) "{}"
        `shouldParse` Block NoType []
    it "parses block with expression" $ do
      testParse (parseBlock NoType) "{\n0\n}"
        `shouldParse` Block NoType [Expr (Literal (Int 0))]
    it "parses block with statement" $ do
      testParse (parseBlock NoType) "{\nreturn\n}"
        `shouldParse` Block NoType [Stmt (Return Nothing)]
    it "does not parse unclosed block" $ do
      testParse (parseBlock NoType)
        `shouldFailOn` "{"
  describe "parseLambda" $ do
    it "parses empty lambda" $ do
      testParse (parseLambda) "():{}"
        `shouldParse` LambdaFunc [] (Block (FunctionBlock VoidT) [])
    it "parses empty lambda with args" $ do
      testParse (parseLambda) "(a: int):{}"
        `shouldParse` LambdaFunc [((Identifier (T.pack "a")), IntT)] (Block (FunctionBlock VoidT) [])
    it "parses lambda with expression" $ do
      testParse (parseLambda) "():0"
        `shouldParse` LambdaFunc [] (Expr (Literal (Int 0)))
    it "parses lambda with statement" $ do
      testParse (parseLambda) "():return"
        `shouldParse` LambdaFunc [] (Stmt (Return Nothing))
    it "does not parse lambda without body" $ do
      testParse (parseLambda)
        `shouldFailOn` "():"
  describe "parseLambdaApplication" $ do
    it "parses application" $ do
      testParse parseLambdaApplication "((a:int):a)(0)"
        `shouldParse` LambdaApplication (LambdaFunc [(Identifier (T.pack "a"), IntT)] (Expr (Identifier (T.pack "a")))) (Literal (Int 0))
    it "does not parse if no application" $ do
      testParse parseLambdaApplication
        `shouldFailOn` "((a:int):a)"
    it "does not parse if application is statement" $ do
      testParse parseLambdaApplication
        `shouldFailOn` "((a:int):a)(return)"
    it "does not parse if application is not inline lambda" $ do
      testParse parseLambdaApplication
        `shouldFailOn` "(a)(return)"
  describe "parseIf" $ do
    it "parses empty if statement" $ do
      testParse parseIf "if () {}"
        `shouldParse` IfStmt Nothing (Block If []) Nothing
    it "parses if statement with condition" $ do
      testParse parseIf "if (true) {}"
        `shouldParse` IfStmt (Just (Literal (Bool True))) (Block If []) Nothing
    it "parses if statement with body" $ do
      testParse parseIf "if (true) {\nreturn\n}"
        `shouldParse` IfStmt (Just (Literal (Bool True))) (Block If [Stmt (Return Nothing)]) Nothing
    it "parses if statement with else" $ do
      testParse parseIf "if (true) {\nreturn\n} else {}"
        `shouldParse` IfStmt (Just (Literal (Bool True))) (Block If [Stmt (Return Nothing)]) (Just (Stmt (ElseStmt (Block Else []))))
    it "parses if statement with else if" $ do
      testParse parseIf "if (true) {\nreturn\n} else if () {}"
        `shouldParse` IfStmt (Just (Literal (Bool True))) (Block If [Stmt (Return Nothing)]) (Just (Stmt (ElseStmt (Stmt (IfStmt Nothing (Block If []) Nothing)))))
    it "parses if statement with else if and else" $ do
      testParse parseIf "if (true) {\nreturn\n} else if () {} else {}"
        `shouldParse` IfStmt (Just (Literal (Bool True))) (Block If [Stmt (Return Nothing)]) (Just (Stmt (ElseStmt (Stmt (IfStmt Nothing (Block If []) (Just (Stmt (ElseStmt (Block Else [])))))))))
    it "does not parse if with no body" $ do
      testParse parseIf
        `shouldFailOn` "if (true)"
    it "does not parse if with no condition" $ do
      testParse parseIf
        `shouldFailOn` "if {}"
  describe "parseElse" $ do
    it "parses empty else" $ do
      testParse parseElse "else {}"
        `shouldParse` ElseStmt (Block Else [])
    it "parses else with body" $ do
      testParse parseElse "else {\nreturn\n}"
        `shouldParse` ElseStmt (Block Else [Stmt (Return Nothing)])
    it "parses else if" $ do
      testParse parseElse "else if () {}"
        `shouldParse` ElseStmt (Stmt (IfStmt Nothing (Block If []) Nothing))
    it "parses else if and else" $ do
      testParse parseElse "else if () {} else {}"
        `shouldParse` ElseStmt (Stmt (IfStmt Nothing (Block If []) (Just (Stmt (ElseStmt (Block Else []))))))
    it "does not parse else with no body" $ do
      testParse parseElse
        `shouldFailOn` "else"
  describe "parseFunctionParameters" $ do
    it "parses no parameters" $ do
      testParse parseFunctionParameters ""
        `shouldParse` []
    it "parses one parameter" $ do
      testParse parseFunctionParameters "a: int"
        `shouldParse` [(Identifier (T.pack "a"), IntT)]
    it "parses multiple parameters" $ do
      testParse parseFunctionParameters "a: int, b: float"
        `shouldParse` [(Identifier (T.pack "a"), IntT), (Identifier (T.pack "b"), FloatT)]
    it "does not parse if no type" $ do
      testParse parseFunctionParameters
        `shouldFailOn` "a, b"
    it "does not parse if rhs is not type" $ do
      testParse parseFunctionParameters
        `shouldFailOn` "a: if"
    it "does not parse if lhs is keyword" $ do
      testParse (parseFunctionParameters <* eof)
        `shouldFailOn` "if: int"
  describe "parseFunctionSignature" $ do
    it "parses signature" $ do
      testParse (parseFunctionSignature) "func f(a: int) bool"
        `shouldParse` (FunctionIdentifier (T.pack "f"), [(Identifier (T.pack "a"), IntT)], BoolT)
    it "parses signature with void return" $ do
      testParse (parseFunctionSignature) "func f(a: int)"
        `shouldParse` (FunctionIdentifier (T.pack "f"), [(Identifier (T.pack "a"), IntT)], VoidT)
    it "does not parse signature with keyword identifier" $ do
      testParse (parseFunctionSignature <* eof)
        `shouldFailOn` "func else(a: int)"
    it "does not parse signature with keyword return type" $ do
      testParse (parseFunctionSignature <* eof)
        `shouldFailOn` "func f(a: int) else"
  describe "parseFunctionDefinition" $ do
    it "parses empty function" $ do
      testParse parseFunctionDefinition "func f() {}"
        `shouldParse` FunctionDef (FunctionIdentifier (T.pack "f")) [] VoidT (Just (Block (FunctionBlock VoidT) []))
    it "parses function with body" $ do
      testParse parseFunctionDefinition "func f() {\nreturn\n}"
        `shouldParse` FunctionDef (FunctionIdentifier (T.pack "f")) [] VoidT (Just (Block (FunctionBlock VoidT) [Stmt (Return Nothing)]))
    it "does not parse function without body" $ do
      testParse parseFunctionDefinition
        `shouldFailOn` "func f()"
  describe "parseFunctionForwardDeclaration" $ do
    it "parses forward declaration" $ do
      testParse parseFunctionForwardDeclaration "[fwd] func f()"
        `shouldParse` FunctionDef (FunctionIdentifier (T.pack "f")) [] VoidT Nothing
    it "does not parse forward declaration with body" $ do
      testParse (parseFunctionForwardDeclaration <* eof)
        `shouldFailOn` "[fwd] func f() {}"
    it "does not parse forward declaration without [fwd]" $ do
      testParse parseFunctionForwardDeclaration
        `shouldFailOn` "func f()"
  describe "parseFunctionCallArguments" $ do
    it "parses no args" $ do
      testParse parseFunctionCallArguments ""
        `shouldParse` []
    it "parses one arg" $ do
      testParse parseFunctionCallArguments "0"
        `shouldParse` [Literal (Int 0)]
    it "parses multiple args" $ do
      testParse parseFunctionCallArguments "0, 1"
        `shouldParse` [Literal (Int 0), Literal (Int 1)]
    it "does not parse if arg is keyword" $ do
      testParse (parseFunctionCallArguments <* eof)
        `shouldFailOn` "if"
  describe "parseFunctionCall" $ do
    it "parses call" $ do
      testParse parseFunctionCall "f()"
        `shouldParse` FunctionCall (FunctionIdentifier (T.pack "f")) []
    it "does not parse if identifier is keyword" $ do
      testParse parseFunctionCall
        `shouldFailOn` "if()"
  describe "parseVarDeclaration" $ do
    it "parses var declaration" $
      do
        testParse parseVarDeclaration "let a: int"
        `shouldParse` Variable (Identifier (T.pack "a")) (Just IntT) Nothing
    it "does not parse if given initializer" $
      do
        testParse (parseVarDeclaration <* eof)
        `shouldFailOn` "let a = 0"
    it "does not parse if identifier is keyword" $
      do
        testParse parseVarDeclaration
        `shouldFailOn` "let if: int"
    it "does not parse if type is keyword" $
      do
        testParse parseVarDeclaration
        `shouldFailOn` "let a: else"
  describe "parseVarInitialization" $ do
    it "parses var initialization" $ do
      testParse parseVarInitialization "let a: int = 0"
        `shouldParse` Variable (Identifier (T.pack "a")) (Just IntT) (Just (Literal (Int 0)))
    it "parses var initialization with no type" $ do
      testParse parseVarInitialization "let a = 3"
        `shouldParse` Variable (Identifier (T.pack "a")) Nothing (Just (Literal (Int 3)))
    it "does not parse if not given initializer" $ do
      testParse (parseVarInitialization <* eof)
        `shouldFailOn` "let a"
    it "does not parse if identifier is keyword" $ do
      testParse parseVarInitialization
        `shouldFailOn` "let if: int = 0"
    it "does not parse if type is keyword" $ do
      testParse parseVarInitialization
        `shouldFailOn` "let a: else = 3"
  describe "parseAssign" $ do
    it "parses normal assignment" $ do
      testParse parseAssign "a = 3"
        `shouldParse` Assignment (Assign (Identifier (T.pack "a")) (Literal (Int 3)))
    it "parses add assignment" $ do
      testParse parseAssign "a += 3"
        `shouldParse` Assignment (AddAssign (Identifier (T.pack "a")) (Literal (Int 3)))
    it "parses sub assignment" $ do
      testParse parseAssign "a -= 3"
        `shouldParse` Assignment (SubAssign (Identifier (T.pack "a")) (Literal (Int 3)))
    it "parses mul assignment" $ do
      testParse parseAssign "a *= 3"
        `shouldParse` Assignment (MulAssign (Identifier (T.pack "a")) (Literal (Int 3)))
    it "parses div assignment" $ do
      testParse parseAssign "a /= 3"
        `shouldParse` Assignment (DivAssign (Identifier (T.pack "a")) (Literal (Int 3)))
    it "parses int div assignment" $ do
      testParse parseAssign "a //= 3"
        `shouldParse` Assignment (IntDivAssign (Identifier (T.pack "a")) (Literal (Int 3)))
    it "parses mod assignment" $ do
      testParse parseAssign "a %= 3"
        `shouldParse` Assignment (ModAssign (Identifier (T.pack "a")) (Literal (Int 3)))
    it "parses bitwise or assignment" $ do
      testParse parseAssign "a |= 3"
        `shouldParse` Assignment (BitwiseOrAssign (Identifier (T.pack "a")) (Literal (Int 3)))
    it "parses bitwise and assignment" $ do
      testParse parseAssign "a &= 3"
        `shouldParse` Assignment (BitwiseAndAssign (Identifier (T.pack "a")) (Literal (Int 3)))
    it "parses bitwise xor assignment" $ do
      testParse parseAssign "a ^= 3"
        `shouldParse` Assignment (BitwiseXorAssign (Identifier (T.pack "a")) (Literal (Int 3)))
    it "does not parse if lhs is keyword" $ do
      testParse parseAssign
        `shouldFailOn` "if = 4"
    it "does not parse if rhs is keyword" $ do
      testParse parseAssign
        `shouldFailOn` "a = else"
    it "does not parse if no rhs" $ do
      testParse parseAssign
        `shouldFailOn` "a = "
  describe "parseTerm" $ do
    it "parses lambda" $ do
      testParse parseTerm "():{}"
        `parseSatisfies` isLambda
    it "parses lambda application" $ do
      testParse parseTerm "((a:int):a)(0)"
        `parseSatisfies` isLambdaApplication
    it "parses function call" $ do
      testParse parseTerm "a(0)"
        `parseSatisfies` isFunctionCall
    it "parses literal" $ do
      testParse parseTerm "5"
        `parseSatisfies` isLiteral
    it "parses parentheses" $ do
      testParse parseTerm "(0)"
        `parseSatisfies` isParentheses
    it "parses identifier" $ do
      testParse parseTerm "a"
        `parseSatisfies` isIdentifier
  describe "parseStatement" $ do
    it "parses var initialization" $ do
      testParse parseStatement "let a = 0"
        `parseSatisfies` isVarInitialization
    it "parses var declaration" $ do
      testParse parseStatement "let a: int"
        `parseSatisfies` isVarDeclaration
    it "parses assignment" $ do
      testParse parseStatement "a += 1"
        `parseSatisfies` isAssignment
    it "parses forward declaration" $ do
      testParse parseStatement "[fwd] func f()"
        `parseSatisfies` isForwardDeclaration
    it "parses return" $ do
      testParse parseStatement "return"
        `parseSatisfies` isReturn
    it "parses function definition" $ do
      testParse parseStatement "func f() {}"
        `parseSatisfies` isFunctionDefinition
    it "parses if statement" $ do
      testParse parseStatement "if () {}"
        `parseSatisfies` isIfStatement
    describe "parseExpr" $ do
      describe "binary operators" $ do
        let cases =
              [ ("1 + 2", Operation (Add (Literal (Int 1)) (Literal (Int 2))))
              , ("1 - 2", Operation (Subtract (Literal (Int 1)) (Literal (Int 2))))
              , ("1 * 2", Operation (Multiply (Literal (Int 1)) (Literal (Int 2))))
              , ("1 / 2", Operation (Divide (Literal (Int 1)) (Literal (Int 2))))
              , ("1 // 2", Operation (IntDivide (Literal (Int 1)) (Literal (Int 2))))
              , ("1 % 2", Operation (Modulo (Literal (Int 1)) (Literal (Int 2))))
              , ("1 & 2", Operation (BitwiseAnd (Literal (Int 1)) (Literal (Int 2))))
              , ("1 | 2", Operation (BitwiseOr (Literal (Int 1)) (Literal (Int 2))))
              , ("1 ^ 2", Operation (BitwiseXor (Literal (Int 1)) (Literal (Int 2))))
              , ("1 >= 2", Operation (GreaterThanEq (Literal (Int 1)) (Literal (Int 2))))
              , ("1 <= 2", Operation (LessThanEq (Literal (Int 1)) (Literal (Int 2))))
              , ("1 == 2", Operation (Equals (Literal (Int 1)) (Literal (Int 2))))
              , ("1 != 2", Operation (NotEquals (Literal (Int 1)) (Literal (Int 2))))
              , ("1 > 2", Operation (GreaterThan (Literal (Int 1)) (Literal (Int 2))))
              , ("1 < 2", Operation (LessThan (Literal (Int 1)) (Literal (Int 2))))
              , ("1 && 2", Operation (And (Literal (Int 1)) (Literal (Int 2))))
              , ("1 || 2", Operation (Or (Literal (Int 1)) (Literal (Int 2))))
              ]
        forM_ cases $ \(input, expected) ->
          it ("parses " ++ input) $
            testParse parseExpr input `shouldParse` expected
      describe "prefix operators" $ do
        let cases =
              [ ("-1", Operation (Negation (Literal (Int 1))))
              , ("!1", Operation (Not (Literal (Int 1))))
              , ("~1", Operation (BitwiseNot (Literal (Int 1))))
              ]
        forM_ cases $ \(input, expected) ->
          it ("parses " ++ input) $
            testParse parseExpr input `shouldParse` expected
      describe "precedence" $ do
        it "multiplication binds tighter than addition" $
          testParse parseExpr "1 + 2 * 3"
            `shouldParse` Operation (Add (Literal (Int 1)) (Operation (Multiply (Literal (Int 2)) (Literal (Int 3)))))

        it "addition binds tighter than bitwise and" $
          testParse parseExpr "1 & 2 + 3"
            `shouldParse` Operation (BitwiseAnd (Literal (Int 1)) (Operation (Add (Literal (Int 2)) (Literal (Int 3)))))

        it "bitwise and binds tighter than bitwise or" $
          testParse parseExpr "1 | 2 & 3"
            `shouldParse` Operation (BitwiseOr (Literal (Int 1)) (Operation (BitwiseAnd (Literal (Int 2)) (Literal (Int 3)))))

        it "bitwise or binds tighter than comparison" $
          testParse parseExpr "1 == 2 | 3"
            `shouldParse` Operation (Equals (Literal (Int 1)) (Operation (BitwiseOr (Literal (Int 2)) (Literal (Int 3)))))

        it "comparison binds tighter than &&" $
          testParse parseExpr "1 && 2 == 3"
            `shouldParse` Operation (And (Literal (Int 1)) (Operation (Equals (Literal (Int 2)) (Literal (Int 3)))))

        it "&& binds tighter than ||" $
          testParse parseExpr "1 || 2 && 3"
            `shouldParse` Operation (Or (Literal (Int 1)) (Operation (And (Literal (Int 2)) (Literal (Int 3)))))
      describe "associativity" $ do
        it "subtraction is left-associative" $
          testParse parseExpr "1 - 2 - 3"
            `shouldParse` Operation (Subtract (Operation (Subtract (Literal (Int 1)) (Literal (Int 2)))) (Literal (Int 3)))
      describe "operator tokenization disambiguation" $ do
        it "parses >= as one operator, not > followed by =" $
          testParse parseExpr "1 >= 2"
            `shouldParse` Operation (GreaterThanEq (Literal (Int 1)) (Literal (Int 2)))

        it "parses && as one operator, not & followed by &" $
          testParse parseExpr "1 && 2"
            `shouldParse` Operation (And (Literal (Int 1)) (Literal (Int 2)))

        it "parses // as one operator, not / followed by /" $
          testParse parseExpr "1 // 2"
            `shouldParse` Operation (IntDivide (Literal (Int 1)) (Literal (Int 2)))

  where
    isVarInitialization (Variable _ _ (Just _)) = True
    isVarInitialization _ = False
    isVarDeclaration (Variable _ _ Nothing) = True
    isVarDeclaration _ = False
    isAssignment (Assignment _) = True
    isAssignment _ = False
    isForwardDeclaration (FunctionDef _ _ _ Nothing) = True
    isForwardDeclaration _ = False
    isFunctionDefinition (FunctionDef _ _ _ (Just _)) = True
    isFunctionDefinition _ = False
    isReturn (Return _) = True
    isReturn _ = False
    isIfStatement (IfStmt _ _ _) = True
    isIfStatement _ = False
    isLambda (LambdaFunc _ _) = True
    isLambda _ = False
    isLambdaApplication (LambdaApplication _ _) = True
    isLambdaApplication _ = False
    isFunctionCall (FunctionCall _ _) = True
    isFunctionCall _ = False
    isLiteral (Literal _) = True
    isLiteral _ = False
    isParentheses (Parentheses _) = True
    isParentheses _ = False
    isIdentifier (Identifier _) = True
    isIdentifier _ = False
    isIntLiteral (Literal (Int _)) = True
    isIntLiteral _ = False
    isFloatLiteral (Literal (Float _)) = True
    isFloatLiteral _ = False
    isStringLiteral (Literal (String _)) = True
    isStringLiteral _ = False
    isBoolLiteral (Literal (Bool _)) = True
    isBoolLiteral _ = False
    isArrayLiteral (Literal (Array _)) = True
    isArrayLiteral _ = False
    isVectorLiteral (Literal (Vector _)) = True
    isVectorLiteral _ = False
    isPointLiteral (Literal (Point _)) = True
    isPointLiteral _ = False
    isMatrixLiteral (Literal (Matrix _)) = True
    isMatrixLiteral _ = False
