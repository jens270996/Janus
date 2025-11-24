module AST where

type VariableName = String
data Variable =
    Variable VariableName
    | ArrayIndex VariableName Index 
type Identifier = String
type Length = Int
type Index = Expression
type Value = Int

data Program = Program [VariableDeclaration] [Procedure]

data VariableDeclaration = 
    VariableDeclaration String
    | ArrayDeclaration String Length

data Procedure = Procedure Identifier [Statement]

data Statement =
    Call Identifier
    |Uncall Identifier
    |Swap Variable Variable
    |Skip
    |Conditional Expression Statement Statement Expression
    |Loop Expression Statement Statement Expression
    |Assignment Variable Operator Expression

data Expression =
    Constant Value
    |EVariable Variable
    |Operation Operator Expression Expression

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


