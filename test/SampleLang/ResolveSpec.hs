{-# LANGUAGE OverloadedStrings #-}
module SampleLang.ResolveSpec
    ( spec
    ) where

import qualified Data.Map.Strict as Map (fromList)
import qualified SampleLang.Ast.Parsed as P
import qualified SampleLang.Ast.Resolved as R
import SampleLang.Ast.Types
import SampleLang.Resolve
import Test.Hspec

spec :: Spec
spec = do
    describe "resolveExpr" $ do
        it "resolve local variable name" $ do
            let funcMap = mempty
                gvarMap = mempty
                lvarMap = Map.fromList [("a", (LocalVarIdx 0, TypeInt))]
                e = P.ExprReference "a"
                result = resolveExpr funcMap gvarMap lvarMap e
            result `shouldBe` Right (R.ExprReference TypeInt (ReferenceLocal (LocalVarIdx 0) TypeInt))
        it "resolve global variable name" $ do
            let funcMap = mempty
                gvarMap = Map.fromList [("a", (GlobalVarIdx 0, TypeInt))]
                lvarMap = mempty
                e = P.ExprReference "a"
                result = resolveExpr funcMap gvarMap lvarMap e
            result `shouldBe` Right (R.ExprReference TypeInt (ReferenceGlobal (GlobalVarIdx 0) TypeInt))
        it "priorize local variable" $ do
            let funcMap = mempty
                gvarMap = Map.fromList [("a", (GlobalVarIdx 0, TypeInt))]
                lvarMap = Map.fromList [("a", (LocalVarIdx 0, TypeInt))]
                e = P.ExprReference "a"
                result = resolveExpr funcMap gvarMap lvarMap e
            result `shouldBe` Right (R.ExprReference TypeInt (ReferenceLocal (LocalVarIdx 0) TypeInt))

