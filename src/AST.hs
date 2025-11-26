module AST where

type VariableName = String
data Variable =
    Variable VariableName
    | ArrayIndex VariableName Index
    deriving (Eq, Show, Read)

type Identifier = String
type Length = Int
type Index = Expression
type Value = Int

data Program = Program [VariableDeclaration] [Procedure]
    deriving (Eq, Show, Read)

type ScalarDeclaration = String

type ArrayDeclaration = (String,Length)

type VariableDeclaration = Either ScalarDeclaration ArrayDeclaration

data Procedure = Procedure Identifier [Statement]
    deriving (Eq, Show, Read)

data Statement =
    Call Identifier
    |Uncall Identifier
    |Swap Variable Variable
    |Skip
    |Conditional Expression [Statement] [Statement] Expression
    |Loop Expression [Statement] [Statement] Expression
    |Assignment Variable Operator Expression
    deriving (Eq, Show, Read)

data Expression =
    Constant Value
    |EVariable Variable
    |Operation Operator Expression Expression
    deriving (Eq, Show, Read)

data Operator =
    Xor
    |Add
    |Sub
    |Mul
    |Div
    |Mod
    |And
    |Or
    |LAnd
    |LOr
    |Lt
    |Gt
    |Eq
    |Neq
    |Leq
    |Geq
    deriving (Eq, Show, Read)

