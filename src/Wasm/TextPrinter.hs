{-# LANGUAGE OverloadedStrings #-}
module Wasm.TextPrinter
    ( printText
    ) where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (imap, toList, (!))
import Text.Builder (Builder)
import qualified Text.Builder as Builder (decimal, intercalate, run, text)
import Wasm.Types

printText :: Module -> Text
printText = Builder.run . buildModule

buildModule :: Module -> Builder
buildModule m =
    Builder.intercalate "\n  " (["(module"] ++ buildTypes types ++ buildFuncs types funcs)
    `mappend`
    "\n)\n"
    where
    funcs = moduleFuncs m
    types = moduleTypes m

buildIndex :: Int -> Builder
buildIndex i =
    mconcat ["(;", Builder.decimal i, ";)"]

buildTypes :: Vector FuncType -> [Builder]
buildTypes = Vector.toList . Vector.imap buildType

buildType :: Int -> FuncType -> Builder
buildType typeidx (FuncType (ResultType params) (ResultType results)) =
    Builder.intercalate " "
    [ "(type"
    , buildIndex typeidx
    , buildFuncType params results
    ]
    `mappend`
    ")"

buildFuncType :: [ValType] -> [ValType] -> Builder
buildFuncType [] [] = "(func)"
buildFuncType params [] =
    Builder.intercalate " "
    [ "(func"
    , buildParam params
    ]
    `mappend`
    ")"
buildFuncType [] results =
    Builder.intercalate " "
    [ "(func"
    , buildResult results
    ]
    `mappend`
    ")"
buildFuncType params results =
    Builder.intercalate " "
    [ "(func"
    , buildParam params
    , buildResult results
    ]
    `mappend`
    ")"

buildParam :: [ValType] -> Builder
buildParam params =
    Builder.intercalate " " (["(param"] `mappend` map buildValType params)
    `mappend`
    ")"

buildResult :: [ValType] -> Builder
buildResult params =
    Builder.intercalate (Builder.text " ") (["(result"] `mappend` map buildValType params)
    `mappend`
    ")"

buildValType :: ValType -> Builder
buildValType (NumType I32)       = Builder.text "i32"
buildValType (NumType I64)       = Builder.text "i64"
buildValType (NumType F32)       = Builder.text "f32"
buildValType (NumType F64)       = Builder.text "f64"
buildValType (RefType ExternRef) = Builder.text "externref"
buildValType (RefType FuncRef)   = Builder.text "funcref"

buildFuncs :: Vector FuncType -> Vector Func -> [Builder]
buildFuncs types = Vector.toList . Vector.imap f
    where
    f idx func =
        let TypeIdx tidx = funcType func
            ftype = types Vector.! tidx
        in buildFunc ftype idx func

buildFunc :: FuncType -> Int -> Func -> Builder
buildFunc (FuncType (ResultType params) (ResultType results)) funcidx (Func (TypeIdx typeidx) locals body) =
    Builder.intercalate (Builder.text "\n    ")
    [ "(func"
    , buildIndex funcidx
    , mconcat ["(type ", Builder.decimal typeidx, ")"]
    , buildParam params
    , buildResult results
    , buildFuncBody body
    ]
    `mappend`
    "  )"

buildFuncBody :: [Instr] -> Builder
buildFuncBody instrs =
    Builder.intercalate (Builder.text "\n    ") (map buildInstr instrs)
    `mappend`
    "\n"

buildInstr :: Instr -> Builder
buildInstr _ = Builder.text "nop"
