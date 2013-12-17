module Main where
import Language.Tip.Parser
import Language.Tip.Compiler
import System.Environment
import System.Exit

main = do
  (file:_) <- getArgs
  r <- parse file
  case r of
    Left e -> do
           print e
           exitFailure
    Right ast ->
           case compile ast of
             Left es -> do
                    mapM putStrLn es
                    exitFailure
             Right out -> do
                    print out
