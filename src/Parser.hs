module Parser (parseProgram) where

import AST
import Text.Parsec
import Control.Monad (void)
type Parser = Parsec String ()
type ParseResult = Either String

parseString :: Parser a -> String -> ParseResult a
parseString p s = case parse (p <* eof) "" s of
                    Right a -> return a
                    Left e -> Left (show e)

parseProgram :: String -> ParseResult Program
parseProgram = parseString (whitespace *> pProgram)

pProgram :: Parser Program
pProgram =
   do variableDeclaration <- many pVariableDeclaration
      procedures <- many1 pProcedure
      return $ Program variableDeclaration procedures

pVariableDeclaration :: Parser VariableDeclaration
pVariableDeclaration =
   (VariableDeclaration <$> variableName)
   <|> (do name <- variableName
           size <- inBrackets constant 
           return $ ArrayDeclaration name size)

pProcedure :: Parser Procedure
pProcedure = do procedureId <- identifier
                body <- many1 pStatement
                return $ Procedure procedureId body

pStatement :: Parser Statement
pStatement =
   Call <$> (keyword "call" *> identifier)
   <|> Uncall <$> (keyword "uncall" *> identifier)
   <|> (do v1 <- pVariable
           symbol "<=>"
           v2 <- pVariable
           return $ Swap v1 v2)
   <|> Skip <$ keyword "skip"
   <|> (do keyword "from"
           e1 <- pExpression
           keyword "do"
           s1 <- pStatement
           keyword "loop"
           s2 <- pStatement
           keyword "until"
           e2 <- pExpression
           return $ Loop e1 s1 s2 e2)
   <|> (do keyword "if"
           e1 <- pExpression
           keyword "then"
           s1 <- pStatement
           keyword "else"
           s2 <- pStatement
           keyword "fi"
           e2 <- pExpression
           return $ Conditional e1 s1 s2 e2)
   <|> (do x <- pVariable
           op <- pReversibleOperator
           symbol "="
           e <- pExpression
           return $ Assignment x op e)


pReversibleOperator :: Parser Operator
pReversibleOperator =choice [ Add <$ symbol "+", Sub <$ symbol "-", Xor <$ symbol "^"]
                    <?> "Expecting reversible operator."



------------------------------------------------------------------------------------------------
pExpression :: Parser Expression
pExpression = pExpression1 `chainl1` operator0

pExpression1 :: Parser Expression
pExpression1 =
    do exp1 <- pExpression2
       option exp1
        (do
            op <- operator1
            exp2 <- pExpression2
            return $ Operation op exp1 exp2
        )
pExpression2 :: Parser Expression
pExpression2 = pExpression3 `chainl1` operator2

pExpression3 :: Parser Expression
pExpression3 = pExpression4 `chainl1` operator3

pExpression4 :: Parser Expression
pExpression4 = (Constant <$> constant) <|> (EVariable <$> pVariable) <|> inParentheses pExpression

pOperator :: Parser Operator -> Parser (Expression -> Expression -> Expression)
pOperator p = do op <- p 
                 return $ Operation op

operator0 :: Parser (Expression -> Expression -> Expression)
operator0 = pOperator $ choice [ And <$ symbol "&&"
                               , Or <$ symbol "||"
                               , Xor <$ symbol "^"]
operator1 :: Parser Operator
operator1 = choice [ Geq <$ try (symbol ">=")
                   , Leq <$ try (symbol "<=")
                   , Gt <$ symbol ">"
                   , Lt <$ symbol "<"
                   , Eq <$ symbol "="
                   , Neq <$ symbol "!="]


operator2 :: Parser (Expression -> Expression -> Expression)
operator2 =
    let  p = choice [ Add <$ symbol "+"
                    , Sub <$ symbol "-"]
    in pOperator p

operator3 :: Parser (Expression -> Expression -> Expression)
operator3 =
    let  p = choice [ Mul <$ symbol "*"
                    , Div <$ symbol "/"
                    , Mod <$ symbol "%"]
    in pOperator p

------------------------------------------------------------------------------
constant :: Parser Int
constant = read <$> lexeme (many1 digit)

symbol :: String -> Parser ()
symbol s = void . lexeme $ string s

keyword :: String -> Parser ()
keyword s = lexeme . try $ string s *> notFollowedBy alphaNum

identifier :: Parser Identifier
identifier = lexeme . try $
                do c <- letter
                   cs <- many pChar
                   if (c:cs) `elem` keywords then fail "keyword used as identifier" else return (c:cs)

pVariable :: Parser Variable
pVariable = 
   (Variable <$> variableName)
   <|> (do name <- variableName
           index <- inBrackets pExpression
           return $ ArrayIndex name index)

variableName :: Parser VariableName
variableName =  lexeme . try $
                do c <- letter
                   cs <- many pChar
                   if (c:cs) `elem` keywords then fail "keyword used as variable" else return (c:cs)

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

inBrackets :: Parser a -> Parser a
inBrackets = between (symbol "[") (symbol "]")

inParentheses :: Parser a -> Parser a
inParentheses = between (symbol "(") (symbol ")")

whitespace :: Parser ()
whitespace = many space *> optional (comment >> whitespace)

comment :: Parser ()
comment = void $ try (string "//") *> manyTill anyChar ( void newline <|> eof)

pChar :: Parser Char
pChar = choice [alphaNum, char '_', char '\'']

keywords :: [String]
keywords = ["procedure","skip","call","uncall","from","do","loop","until","if","then","else","fi"]