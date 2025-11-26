module Parsing.Implementation.InputParser where
import Parsing.Implementation.Parser (parseString,whitespace,constant,inBrackets, Parser,ParseResult)
import AST
import Text.Parsec


data Input = Array ArrayDeclaration [Value] | Scalar ScalarDeclaration Value
parseInput :: String -> ParseResult [Input]
parseInput = parseString (whitespace *> pInputs)

pInputs :: Parser [Input]
pInputs = many pInput

pInput :: Parser Input
pInput =
    do decl <- pVariableDeclaration
       case decl of
        Left scalarDecl -> constant >>= Scalar scalarDecl
        Right arrDecl -> inBrackets (many constant) >>= Array arrDecl
