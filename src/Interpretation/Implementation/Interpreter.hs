module Interpretation.Implementation.Interpreter where

import AST
import Interpretation.Implementation.Computation
import Data.Bits (xor, (.|.),(.&.))

interpretProgram :: Program -> ComputationError VariableStore
interpretProgram prog =
    let procedureStore = constructProcedureStore prog
        variableStore = constructVariableStore prog
    in case runComputation interpretMain procedureStore variableStore [] of
        Right (vars,vars',[]) | vars == vars' -> Right vars
        Right (vars,vars',[]) -> Left "Discrepant variable stores"
        Right (_,_,(x:xs)) -> Left "Non-empty call stack at termination"
        Left e -> Left e



interpretMain :: Computation VariableStore
interpretMain = do main <- getProcedure "main"
                   interpretProcedure main
                   getEnvironment

interpretProcedure :: Procedure -> Computation ()
interpretProcedure (Procedure id stmts) =
    do call id
       interpretStatements stmts
       exit

inverseInterpretProcedure :: Procedure -> Computation ()
inverseInterpretProcedure (Procedure id stmts) =
    do call (id ++ " (inverse)")
       inverseInterpretStatements stmts
       exit

interpretStatements :: [Statement] -> Computation ()
interpretStatements = mapM_ interpretStatement

inverseInterpretStatements :: [Statement] -> Computation ()
inverseInterpretStatements = mapM_ inverseInterpretStatement . reverse

interpretStatement :: Statement -> Computation ()
interpretStatement (Call id) =
    do procedure <- getProcedure id
       interpretProcedure procedure
interpretStatement (Uncall id) =
    do procedure <- getProcedure id
       inverseInterpretProcedure procedure 
interpretStatement (Swap x y) =
    do cx <- get x
       cy <- get y
       set x cy
       set y cx
interpretStatement Skip = return ()
interpretStatement (Conditional e1 s1 s2 e2) =
    do c1 <- interpretExpression e1
       if bool c1 then interpretStatements s1 else interpretStatements s2
       c2 <- interpretExpression e2
       if bool c1 == bool c2 then return () else throw "Assertion in conditional failed." 
interpretStatement (Loop e1 s1 s2 e2) = loopInner True e1 s1 s2 e2

interpretStatement (Assignment x op e) =
    do cx <- get x
       ce <- interpretExpression e
       set x $ applyOp op cx ce

inverseInterpretStatement :: Statement -> Computation ()
inverseInterpretStatement (Call id) = interpretStatement (Uncall id)
inverseInterpretStatement (Uncall id) = interpretStatement (Call id)
inverseInterpretStatement (Swap x y) = interpretStatement (Swap x y)
inverseInterpretStatement Skip = return ()
inverseInterpretStatement (Conditional e1 s1 s2 e2) =
    do c2 <- interpretExpression e2
       if bool c2 then inverseInterpretStatements s1 else inverseInterpretStatements s2
       c1 <- interpretExpression e1
       if bool c1 == bool c2 then return () else throw "Assertion in conditional failed." 
inverseInterpretStatement (Loop e1 s1 s2 e2) = inverseLoopInner True e1 s1 s2 e2
inverseInterpretStatement (Assignment x op e) =
    do cx <- get x
       ce <- interpretExpression e
       set x $ applyOp (invertOp op) cx ce

interpretExpression :: Expression -> Computation Value
interpretExpression (Constant c) = return c
interpretExpression (EVariable var) = look var
interpretExpression (Operation op e1 e2) =
    do c1 <- interpretExpression e1
       c2 <- interpretExpression e2
       return $ applyOp op c1 c2 

applyOp :: Operator -> Value -> Value -> Value
applyOp Xor a b = a `xor` b
applyOp Add a b = a + b
applyOp Sub a b = a - b
applyOp Mul a b = a * b
applyOp Div a b = a `div` b
applyOp Mod a b = a `mod` b
applyOp And a b = a .&. b
applyOp Or a b = a .|. b
applyOp LAnd a b | a == 0 || b == 0 = 0
applyOp LAnd a _ = a
applyOp LOr a b | a == 0 = b
applyOp LOr a _ = a
applyOp Lt a b | a < b = 1
applyOp Lt _ _ = 0
applyOp Gt a b | a > b = 1
applyOp Gt _ _ = 0
applyOp Eq a b | a == b = 1
applyOp Eq _ _ = 0
applyOp Neq a b | a /= b = 1
applyOp Neq _ _ = 0
applyOp Leq a b | a <= b = 1
applyOp Leq _ _ = 0
applyOp Geq a b | a >= b = 1
applyOp Geq _ _ = 0

invertOp :: Operator -> Operator
invertOp Add = Sub
invertOp Sub = Add
invertOp Xor = Xor
invertOp _ = undefined


loopInner :: Bool -> Expression -> [Statement] -> [Statement] -> Expression -> Computation ()
loopInner b e1 s1 s2 e2 =
    do c1 <- interpretExpression e1
       if bool c1 == b
       then do interpretStatements s1
               c2 <- interpretExpression e2
               if bool c2
               then return ()
               else interpretStatements s2 >> loopInner False e1 s1 s2 e2
       else throw $ "Failed from assertion, should be: " ++ show b

inverseLoopInner :: Bool -> Expression -> [Statement] -> [Statement] -> Expression -> Computation ()
inverseLoopInner b e1 s1 s2 e2 =
    do c2 <- interpretExpression e2
       if bool c2 == b
       then do inverseInterpretStatements s1
               c1 <- interpretExpression e1
               if bool c1
               then return ()
               else inverseInterpretStatements s2 >> inverseLoopInner False e1 s1 s2 e2
       else throw $ "Failed from assertion, should be: " ++ show b

get :: Variable -> Computation Value
get (Variable x) = getScalar x
get (ArrayIndex x e) = do i <- interpretExpression e
                          getArray x i

set :: Variable -> Value -> Computation ()
set (Variable x) c = setScalar x c
set (ArrayIndex x e) c = do i <- interpretExpression e
                            setArray x i c

look :: Variable -> Computation Value
look (Variable x) = lookScalar x
look (ArrayIndex x e) = do i <- interpretExpression e
                           lookArray x i

bool :: Value -> Bool
bool 0 = False
bool _ = True