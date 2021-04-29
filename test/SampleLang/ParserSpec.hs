{-# LANGUAGE OverloadedStrings #-}
module SampleLang.ParserSpec
    ( spec
    ) where

import Data.Text (Text)
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
    , ("negate constant int", "- 123", Right $ ExprUnary (Negate (ExprConstant (ConstInt 123))))
    , ("reference", "aaa", Right $ ExprReference "aaa")
    , ("add nums", "1+2", Right $ ExprBinary (Add (ExprConstant (ConstInt 1)) (ExprConstant (ConstInt 2))))
    , ("add and mul", "1+2*3", Right $ ExprBinary (Add (ExprConstant (ConstInt 1)) (ExprBinary (Mul (ExprConstant (ConstInt 2)) (ExprConstant (ConstInt 3))))))
    ]

stmtCases =
    [ ("int declaration", "int a;", Right $ StatementDecl (Parameter "a" TypeInt))
    , ("if statement", "if (true) { }", Right $ StatementIf (ExprConstant (ConstBool True)) [])
    , ("expr statement", "a == b;", Right $ StatementExpr (ExprRelational (Equ (ExprReference "a") (ExprReference "b"))))
    ]
