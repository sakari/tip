module Main where
import Language.Tip.Parser
import System.Environment
import System.Exit

main = do
  (file:_) <- getArgs
  r <- parse file
  case r of
    Left e -> do
           print e
           exitFailure
    Right ast -> do
           putStrLn "ok"
           print ast
