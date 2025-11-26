module ParserTests (tests) where
import Parsing.Implementation.Parser
import Test.Tasty
import Test.Tasty.HUnit
import AST

parseSuccessfully :: (Eq a, Show a) => Parser a -> TestName -> String -> a -> TestTree
parseSuccessfully parser name input expected = testCase name $ parseString parser input @?= Right expected

parseFail :: (Show a) => Parser a -> TestName -> String -> TestTree
parseFail parser name input = testCase name $
                                case parseString parser input of
                                    Left _ -> return ()
                                    Right e -> assertFailure $ "Failed to assert that input does not parse, got successful parse: " ++ show e

tests::TestTree
tests =
    testGroup "Parsing tests"
        [ expressionTests
        , statementTests
        , procedureTests
        , programTests
        , whitespaceTests
        ]

programTests :: TestTree
programTests = testGroup "Program tests"
    [ parseProgramT "Single procedure" "x y procedure f x += y" (Program ["x","y"] [] [Procedure "f" [Assignment (Variable "x") Add (EVariable $ Variable "y")]])
    ]
    where
     parseProgramT = parseSuccessfully pProgram
     parseProgramFail = parseFail pProgram

procedureTests :: TestTree
procedureTests = testGroup "Procedure tests"
    [ parseProcedure "Basic procedure" "procedure f x += y" (Procedure "f" [Assignment (Variable "x") Add (EVariable $ Variable "y")])
    , parseProcedure "Muliple statements" "procedure f  x+=y x -= y" (Procedure "f" [Assignment (Variable "x") Add (EVariable $ Variable "y"), Assignment (Variable "x") Sub (EVariable $ Variable "y")])
    , parseProcedureFail "Keyword as procedure name" "procedure loop x+=y"
    ]
    where
     parseProcedure = parseSuccessfully pProcedure
     parseProcedureFail = parseFail pProcedure

statementTests :: TestTree
statementTests = testGroup "Statement tests"
    [ parseStatement "Reversible assignment add" "x+= 3" (Assignment (Variable "x") Add (Constant 3))
    , parseStatement "Loop statement" "from x=3 do x-=5 loop x+= 1 until x>4" (Loop (Operation Eq (EVariable $ Variable "x") (Constant 3))
                                                                                        [(Assignment (Variable "x") Sub (Constant 5))]
                                                                                        [(Assignment (Variable "x") Add (Constant 1))]
                                                                                        (Operation Gt (EVariable $ Variable "x") (Constant 4))
                                                                                    )
    , parseStatement "Conditional statement" "if x=3 then x-=5 else x+=1 fi x>4" (Conditional (Operation Eq (EVariable $ Variable "x") (Constant 3))
                                                                                        [(Assignment (Variable "x") Sub (Constant 5))]
                                                                                        [(Assignment (Variable "x") Add (Constant 1))]
                                                                                        (Operation Gt (EVariable $ Variable "x") (Constant 4))
                                                                                      )
    , parseStatement "Swap" "x <=> y" (Swap (Variable "x") (Variable "y"))
    , parseStatement "Skip" "skip" Skip
    ]
    where
    parseStatement = parseSuccessfully pStatement
    parseStatementFail = parseFail pStatement


expressionTests :: TestTree
expressionTests = testGroup "Expression tests"
    [ parseExpression "constant" "1" (Constant 1)
    , parseExpression "identifier" "var" (EVariable $ Variable "var")
    , parseExpression "array indexing" "var[2]" (EVariable $ ArrayIndex "var" (Constant 2))
    , parseExpression "operation" "var * 2" (Operation Mul (EVariable $ Variable "var") (Constant 2))
    , parseExpression "operation >=" "var >= 2" (Operation Geq (EVariable $ Variable "var") (Constant 2)) 
    , parseExpression "precedence * and +" "x + y * 2" (Operation Add (EVariable $ Variable "x") (Operation Mul (EVariable $ Variable "y") (Constant 2)))
    , parseExpression "precedence + and &&" "x + y && 2" (Operation And (Operation Add (EVariable $ Variable "x") (EVariable $ Variable "y")) (Constant 2))
    , parseExpression "precedence && and =" "x && y = 2" (Operation And (EVariable $ Variable "x") (Operation Eq  (EVariable $ Variable "y") (Constant 2)))
    , parseExpression "precedence over associativity" "2 = x && y" (Operation And (Operation Eq (Constant 2) (EVariable $ Variable "x")) (EVariable $ Variable "y"))
    , parseExpression "parentheses over precedence" "(2 = x) && y" (Operation And (Operation Eq (Constant 2) (EVariable $ Variable "x"))   (EVariable $ Variable "y"))
    , parseExpression "parentheses over associativity" "x *( y * 2)" (Operation Mul  (EVariable $ Variable "x") (Operation Mul (EVariable $ Variable "y") (Constant 2)))
    , parseExpression "associativity *" "x * y * 2" (Operation Mul (Operation Mul (EVariable $ Variable "x") (EVariable $ Variable "y")) (Constant 2))
    , parseExpressionFail "associativity =" "x = y = 2"
    ]
    where
     parseExpression = parseSuccessfully pExpression
     parseExpressionFail = parseFail pExpression

whitespaceTests :: TestTree
whitespaceTests =
    testGroup "Whitespace tests"
    [ parseSuccessfully (keyword  "symbol") "whitespace after symbol" "symbol  " ()
    , parseSuccessfully (keyword  "symbol") "newline after symbol" "symbol    \n" ()
    , parseSuccessfully (keyword  "symbol") "comment after symbol" "symbol  // safasf asfasfasf asf \n" ()
    , parseFail (keyword  "symbol") "line break ends comment" "symbol  // safasf asfasfasf asf \n asdasdasddasd"
    , parseSuccessfully (keyword  "symbol") "spaces after newline" "symbol    \n        " ()
    , parseSuccessfully (keyword  "symbol") "comment after newline" "symbol    \n    // bla blab bal    " ()
    , parseSuccessfully (keyword  "symbol") "Muliple comments" "symbol // bla blab bal    \n    // bla blab bal \n // bla blab bal     " ()
    ]