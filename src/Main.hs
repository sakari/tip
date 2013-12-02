module Main where
import Language.Tip.Parser
import Language.Tip.Generator
import Language.Tip.Desugarer
import System.Environment
import System.Exit
import Language.ECMAScript3.PrettyPrint

main = do
  (file:_) <- getArgs
  r <- parse file
  case r of
    Left e -> do
           print e
           exitFailure
    Right ast -> do
           print $ prettyPrint $ generate $ desugar ast
