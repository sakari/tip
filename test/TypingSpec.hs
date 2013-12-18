{-# LANGUAGE NamedFieldPuns, QuasiQuotes #-}
module TypingSpec (main, spec) where

import Test.Hspec
import Language.Tip.Quote
import Language.Tip.Ast
import Language.Tip.Types
import Language.Tip.Compiler

main :: IO ()
main = hspec spec

ok :: Module -> IO ()
ok ast = case compile ast of
           Left e -> error $ "nok: " ++ show e
           Right _ -> return ()

typeError :: Module -> IO ()
typeError ast = case compile ast of
                            Right _ -> error "expected to fail"
                            Left _ -> return ()

spec :: Spec
spec = do
  describe "type checking" $ do
         it "checks 'string' type" $ do
                               typeError $ [tipModule| a : number = "string value" |]
         it "passes on correct string type" $ do
                               ok $ [tipModule| a : string = "string value" |]
         it "checks that application is to a function type" $ do
                               typeError $ [tipModule| a : number; a(1) |]
         it "accepts correctly typed application" $ do
                               ok [tipModule| a: (number); a(a) |]
