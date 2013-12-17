module Language.Tip.Compiler where

import Language.Tip.Desugarer
import Language.Tip.Types
import Language.Tip.Ast
import Language.Tip.Generator
import Language.ECMAScript3.PrettyPrint

compile ast = case infer p of
                 Right m -> Right $ prettyPrint $ generate $ pregenerate p
                 Left es -> Left es
    where
      p = precheck ast
