{-# LANGUAGE OverloadedStrings #-}
module SampleLang.CodeGen.WasmSpec
    ( spec
    ) where

import qualified Data.Vector as Vector (fromList, singleton)
import qualified SampleLang.Ast.Resolved as R
import SampleLang.Ast.Types
import SampleLang.CodeGen.Wasm (gen)
import Test.Hspec
import qualified Wasm.Types as Wasm

spec :: Spec
spec = do
    describe "gen" $
        it "gen wasm case1" $
            gen case1 `shouldBe` Right expected1

case1 :: R.Program
case1 = R.Program (Vector.singleton func1) mempty
    where
    func1 = R.Function "f1" (FunctionType [Parameter "" TypeInt, Parameter "" TypeInt] TypeInt) mempty . Vector.singleton . R.StatementExpr $
        R.ExprBinary TypeInt Add (R.ExprReference TypeInt (ReferenceLocal (LocalVarIdx 0) TypeInt)) (R.ExprReference TypeInt (ReferenceLocal (LocalVarIdx 1) TypeInt))

expected1 :: Wasm.Module
expected1 = Wasm.Module types funcs exports
    where
    types = Vector.singleton $ Wasm.FuncType (Wasm.ResultType (Vector.fromList [Wasm.NumType Wasm.I32, Wasm.NumType Wasm.I32])) (Wasm.ResultType (Vector.fromList [Wasm.NumType Wasm.I32]))
    funcs = Vector.singleton $ Wasm.Func 0 mempty (Vector.fromList [Wasm.LocalGet 0, Wasm.LocalGet 1, Wasm.I32Binary Wasm.IAdd])
    exports = Vector.singleton $ Wasm.Export "f1" (Wasm.ExportFunc 0)
