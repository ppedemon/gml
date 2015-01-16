module Main where

import IO
import System

import GMLLexer
import GMLParser
import Eval


main :: IO ()
main = 
  do s <- hGetContents stdin
     let tks = gml_lexer s
     case last tks of
  
       t@(TError _) -> 
         do putStrLn $ "Parser error at: " ++ show (token_pos t)
            exitFailure
        
       _            -> 
          case parse tks of
            Right s -> do { putStrLn s; exitFailure }
            Left p  -> 
              do r <- evalProg p
                 case r of
                   Right s -> do { putStrLn ("Eval: " ++ s); exitFailure }
                   Left _  -> do { exitWith ExitSuccess }
