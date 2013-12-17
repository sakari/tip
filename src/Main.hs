module Main where
import Language.Tip.Parser
import Language.Tip.Generator
import Language.Tip.Desugarer
import Language.Tip.Types
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
           let p = precheck ast
           case infer p of
             Left es -> do
                   mapM_ putStrLn es
                   exitFailure
             Right m -> print $ prettyPrint $ generate $ pregenerate p
