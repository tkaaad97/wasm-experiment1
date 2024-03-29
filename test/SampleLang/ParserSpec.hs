{-# LANGUAGE OverloadedStrings #-}
module SampleLang.ParserSpec
    ( spec
    ) where

import SampleLang.Ast.Parsed
import SampleLang.Parser as Parser
import Test.Hspec
import qualified Text.Megaparsec as Parser (parse)

spec :: Spec
spec = do
    describe "expr" $
        mapM_ runExprCase exprCases
    describe "statement" $
        mapM_ runStmtCase stmtCases
    where
    runExprCase (name, input, expected) =
        it name $ Parser.parse Parser.expr name input `shouldBe` expected
    runStmtCase (name, input, expected) =
        it name $ Parser.parse Parser.statement name input `shouldBe` expected

exprCases =
    [ ("constant int", "123", Right $ ExprConstant (ConstInt 123))
    , ("negate constant int", "- 123", Right $ ExprUnary Negate (ExprConstant (ConstInt 123)))
    , ("reference", "aaa", Right $ ExprReference "aaa")
    , ("add nums", "1+2", Right $ ExprBinary Add (ExprConstant (ConstInt 1)) (ExprConstant (ConstInt 2)))
    , ("add and mul", "1+2*3", Right $ ExprBinary Add (ExprConstant (ConstInt 1)) (ExprBinary Mul (ExprConstant (ConstInt 2)) (ExprConstant (ConstInt 3))))
    , ("assign", "a = 1", Right $ ExprAssign (ExprReference "a") (ExprConstant (ConstInt 1)))
    , ("assign", "a = b = 1", Right $ ExprAssign (ExprReference "a") (ExprAssign (ExprReference "b") (ExprConstant (ConstInt 1))))
    ]

stmtCases =
    [ ("int declaration", "int a;", Right $ StatementDecl (Declaration (Parameter "a" TypeInt) Nothing))
    , ("int declaration with initializer", "int a = 1 + 2;", Right $ StatementDecl (Declaration (Parameter "a" TypeInt) (Just $ ExprBinary Add (ExprConstant (ConstInt 1)) (ExprConstant (ConstInt 2)))))
    , ("if statement", "if (true) { }", Right $ StatementIf (ExprConstant (ConstBool True)) [] [])
    , ("expr statement", "a == b;", Right $ StatementExpr (ExprBinary Equ (ExprReference "a") (ExprReference "b")))
    , ("for 1", "for (a = 0; a < 10; ++a) {}", Right $ StatementFor (Right (ExprAssign (ExprReference "a") (ExprConstant (ConstInt 0)))) (ExprBinary Lt (ExprReference "a") (ExprConstant (ConstInt 10))) (ExprUnary Increment (ExprReference "a")) mempty)
    , ("for 2", "for (int a; a < 10; ++a) {}", Right $ StatementFor (Left (Declaration (Parameter "a" TypeInt) Nothing)) (ExprBinary Lt (ExprReference "a") (ExprConstant (ConstInt 10))) (ExprUnary Increment (ExprReference "a")) mempty)
    , ("for 3", "for (int a = 0; a < 10; ++a) {}", Right $ StatementFor (Left (Declaration (Parameter "a" TypeInt) (Just . ExprConstant . ConstInt $ 0))) (ExprBinary Lt (ExprReference "a") (ExprConstant (ConstInt 10))) (ExprUnary Increment (ExprReference "a")) mempty)
    ]
