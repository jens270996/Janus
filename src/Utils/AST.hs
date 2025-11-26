module Utils.AST where
import Data.List
import Data.Either (lefts, rights)
import AST
procedureIds :: Program -> [Identifier]
procedureIds (Program _ procs) = map (\(Procedure id _) -> id) procs

procedures :: Program -> [Procedure]
procedures (Program _ procs) = procs


scalarDeclarations :: [VariableDeclaration] -> [ScalarDeclaration]
scalarDeclarations = lefts

arrayDeclarations :: [VariableDeclaration] -> [ArrayDeclaration]
arrayDeclarations = rights