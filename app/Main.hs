module Main where

import Parsing.Parser (parseProgram,ParseResult)
import Control.Monad
import Options.Applicative
import System.Exit (die)
import Interpretation.Interpreter (interpretProgram)
import Wellformed (wellformedProgram)
-- import ASTPrinting.Printer (printProgram, printProgramOrdered)
import AST

data Options = Interpret InterpretOptions
data InterpretOptions = InterpretOptions
  {
    programFile :: String
  -- , inputFile :: String
  , verbose :: Bool
  }


interpretParser :: Parser Options
interpretParser = Interpret <$> (InterpretOptions
               <$> argument str (metavar "<Program file>")
              --  <*> argument str (metavar "<Input file>")
               <*> flag True False (long "verbose"
                           <> short 'v'
                           <> help "Print additional information for debugging purposes.")
              )


optParser :: Parser Options
optParser = hsubparser
              ( command "interpret" (info interpretParser
                (progDesc "Interpret a TSL program"))
              )

optsParser :: ParserInfo Options
optsParser = info (optParser <**> helper)
  ( fullDesc
  <> progDesc "An interpreter for Janus"
  )



main :: IO ()
main = do
  options <- execParser optsParser
  case options of
    Interpret opts -> interpretMain opts

interpretMain:: InterpretOptions -> IO()
interpretMain InterpretOptions { programFile=programPath, verbose=v} =
  do trace v "\n" 
     program <- parseFile v parseProgram programPath
     trace v "\n\n\n"
     trace v $ "Program: \n" ++ show program
     trace v "\n"
     runProgram program

runProgram :: Program -> IO ()
runProgram program =
  case wellformedProgram program of
      Nothing -> case interpretProgram program of
                    Right c -> putStrLn $ "Output: " ++ show c
                    Left e -> putStrLn $ "Error during evaluation: " ++ e
      Just e -> putStrLn $ "Program is not wellformed: " ++ e

parseFile :: Bool -> (String -> ParseResult a) -> String ->  IO a
parseFile v parser file =
  do trace v $ "- Reading input from file: " ++ show file
     input <- readFile file
     trace v "- Parsing input"
     fromErrorMonad "parsing" $ parser input


trace :: Bool -> String -> IO()
trace v s = when v $ putStrLn s

fromErrorMonad :: String -> Either String a -> IO a
fromErrorMonad _ (Right a) = return a
fromErrorMonad s (Left e) = die $ "While " ++ s ++ " Error occurred: " ++ e