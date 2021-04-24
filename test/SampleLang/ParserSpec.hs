{-# LANGUAGE OverloadedStrings #-}
module SampleLang.ParserSpec
    ( spec
    ) where

import Data.Text (Text)
import SampleLang.Ast
import SampleLang.Parser
import Test.Hspec
import qualified Text.Megaparsec as Parser (parse)

spec :: Spec
spec =
    describe "expr" $
        mapM_ runExprCase exprCases
    where
    runExprCase (name, input, expected) =
        it name $ Parser.parse expr name input `shouldBe` expected

exprCases =
    [ ("constant int", "123", Right $ ExprConstant (ConstInt 123))
    , ("negate constant int", "- 123", Right $ ExprUnary (Negate (ExprConstant (ConstInt 123))))
    , ("reference", "aaa", Right $ ExprReference "aaa")
    , ("add nums", "1+2", Right $ ExprBinary (Add (ExprConstant (ConstInt 1)) (ExprConstant (ConstInt 2))))
    , ("add and mul", "1+2*3", Right $ ExprBinary (Add (ExprConstant (ConstInt 1)) (ExprBinary (Mul (ExprConstant (ConstInt 2)) (ExprConstant (ConstInt 3))))))
    ]