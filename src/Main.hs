module Main where
import Language.Tip.Parser
import System.Environment
import System.Exit

main = do
  (file:_) <- getArgs
  r <- parse file
  case r of
    Left e -> do
           putStrLn $ show e
           exitFailure
    Right _ -> do
           putStrLn "ok"
