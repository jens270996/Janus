module Interpretation.Implementation.Computation where

import qualified Data.Map as Map
import AST
import Control.Monad
import Data.Array
import Utils.AST

type ComputationError = Either String
type VariableStore = (Map.Map VariableName Value, Map.Map VariableName (Array Length Value))

type ProcedureStore = Map.Map Identifier Procedure
type CallStack = [String]

constructProcedureStore :: Program -> ProcedureStore
constructProcedureStore p = Map.fromList  (zip (procedureIds p) (procedures p))

constructVariableStore :: Program -> VariableStore
constructVariableStore (Program decl _) =
    let scalarMap = Map.fromList $ map (\x -> (x,0)) (scalarDeclarations decl)
        arrayMap = Map.fromList $ map (\(x,n) -> (x,array (0,n-1) [(i,0) | i <- [0..n-1]])) (arrayDeclarations decl)
    in (scalarMap,arrayMap)

newtype Computation a = Computation
    { runComputation ::
        ProcedureStore ->
        VariableStore ->
        CallStack ->
        ComputationError (a,VariableStore,CallStack)
    }


instance Functor Computation where
    fmap = liftM

instance Applicative Computation where
    pure a = Computation (\_ vars cs -> return (a, vars,cs))
    (<*>) = ap

instance Monad Computation where
    m >>= f =
        let transferFunction funcs vars cs =
                do (a, vars',cs') <- runComputation m funcs vars cs
                   runComputation (f a) funcs vars' cs'
        in Computation transferFunction

getProcedureC :: Identifier -> Computation Procedure
getProcedureC id = Computation (\procs vars cs -> case Map.lookup id procs of
                                                        Just procedure -> Right (procedure, vars,cs)
                                                        Nothing -> Left $ "Trying to lookup undeclared procedure: " ++ id
                                )
lookC :: VariableName -> Computation Value
lookC var = Computation (\_ (scalars,arrays) cs -> case Map.lookup var scalars of
                                                        Just c -> Right (c, (scalars,arrays),cs)
                                                        Nothing -> Left $ "Trying to lookup undeclared variable: " ++ var
                         )
setC :: VariableName -> Value -> Computation ()
setC var c = Computation (\_ (scalars,arrays) cs -> case Map.lookup var scalars of
                                            Just 0 -> Right ((),(Map.insert var c scalars,arrays),cs)
                                            Just _ -> Left $ "Trying to perform destructive assignment of variable: " ++ var
                                            _ ->      Left $ "Trying to set undeclared variable: " ++ var
                         )

resetC :: VariableName -> Computation ()
resetC var = Computation (\_ (scalars,arrays) cs -> case Map.lookup var scalars of
                                            Just _ -> Right ((),(Map.insert var 0 scalars,arrays),cs)
                                            _ ->      Left $ "Trying to reset undeclared variable: " ++ var
                         )

getC :: VariableName -> Computation Value
getC var = do c <- lookC var
              resetC var
              return c


lookArrayC :: VariableName -> Int -> Computation Value
lookArrayC var i = Computation (\_ (scalars,arrays) cs -> case Map.lookup var arrays of
                                                        Just arr | (snd $ bounds arr) >= i -> Right (arr!i, (scalars,arrays),cs)
                                                        Just _ -> Left $ "Out of bounds indexing at index " ++ show i ++ " of " ++ var
                                                        Nothing -> Left $ "Trying to lookup undeclared variable: " ++ var
                         )
setArrayC :: VariableName-> Int -> Value -> Computation ()
setArrayC var i c = Computation (\_ (scalars,arrays) cs -> case Map.lookup var arrays of
                                            Just arr | (snd $ bounds arr) >= i  && arr!i == 0 -> Right ((),(scalars, Map.insert var (arr//[(i,c)]) arrays),cs)
                                            Just arr | (snd $ bounds arr) < i -> Left $ "Out of bounds indexing at index " ++ show i ++ " of " ++ var
                                            Just _ -> Left $ "Trying to perform destructive assignment of variable: " ++ var ++"["++show i++"]"
                                            _ ->      Left $ "Trying to set undeclared variable: " ++ var
                         )

resetArrayC :: VariableName-> Int -> Computation ()
resetArrayC var i = Computation (\_ (scalars,arrays) cs -> case Map.lookup var arrays of
                                            Just arr | (snd $ bounds arr) >= i  -> Right ((),(scalars, Map.insert var (arr//[(i,0)]) arrays),cs)
                                            Just arr | (snd $ bounds arr) < i -> Left $ "Out of bounds indexing at index " ++ show i ++ " of " ++ var
                                            _ ->      Left $ "Trying to reset undeclared variable: " ++ var
                         )

getArrayC :: VariableName -> Int -> Computation Value
getArrayC var i = do c <- lookArrayC var i
                     resetArrayC var i
                     return c

printError :: CallStack -> VariableStore -> String -> String
printError cs v s =
    "Environment contained: \n"++ printStore v ++ "\nCallstack contained: \n"++ printCallStack cs ++ "\nCaught error: " ++ s  ++ "\n"

throwC :: String -> Computation ()
throwC e = Computation (\_ varStore cs -> Left $ printError cs varStore e )

printCallStack :: CallStack -> String
printCallStack cs = "[" ++ (concatMap (++ ",\n\n") . reverse $ cs) ++ "]\n"

printStore :: VariableStore -> String
printStore (scalars,arrays) =
    "Program store: \n"
    ++
    Map.foldrWithKey  (\k a str ->  (k ++ "=" ++ show a ++ "\n") ++ str) "" scalars
    ++
    Map.foldrWithKey  (\k a str ->  k ++ "[" ++ show ((snd . bounds $ a)+1) ++"]=" ++ printArray a ++ "\n" ++ str) "" arrays

printArray :: (Show b) => Array a b -> String
printArray = show . elems

getEnvironmentC :: Computation VariableStore
getEnvironmentC = Computation (\_ vars cs -> Right (vars,vars,cs))

callC :: String -> Computation ()
callC s = Computation (\_ vars cs -> Right ((),vars,s:cs))
returnC :: Computation ()
returnC = Computation (\_ vars cs-> Right ((),vars,tail cs))

-- The class of monads that support the core RWS operations
class Monad cm => ComputationMonad cm where
  -- set variable to a value. Variable must be 0 prior to update.
  setScalar :: VariableName -> Value -> cm ()
  -- get value of variable and set it to 0.
  getScalar :: VariableName -> cm Value
  -- get value of variable.
  lookScalar :: VariableName -> cm Value
  -- set array[i] to a value. Variable must be 0 prior to update.
  setArray :: VariableName -> Int -> Value -> cm ()
  -- get value of variable and set it to 0.
  getArray :: VariableName -> Int -> cm Value
  -- get value of variable.
  lookArray :: VariableName -> Int -> cm Value
  -- Get procedure by its identifier
  getProcedure :: Identifier -> cm Procedure

  getEnvironment :: cm VariableStore

  call :: String -> cm ()
  exit :: cm()
  -- Throw an error
  throw :: String -> cm ()

instance ComputationMonad Computation where
    setScalar = setC
    getScalar = getC
    lookScalar = lookC
    setArray = setArrayC
    getArray = getArrayC
    lookArray = lookArrayC
    getProcedure = getProcedureC
    getEnvironment = getEnvironmentC
    throw = throwC
    call = callC
    exit = returnC