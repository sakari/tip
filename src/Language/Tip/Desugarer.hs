module Language.Tip.Desugarer (desugar) where

desugar ast = asyncTransform ast

asyncTransform = id
