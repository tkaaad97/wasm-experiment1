{-# LANGUAGE OverloadedStrings #-}
module Wasm.TextPrinter
    ( printText
    ) where

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text.Lazy as Text (toStrict)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder (fromText, toLazyText)
import qualified Data.Text.Lazy.Builder.Int as Builder (decimal)
import qualified Data.Text.Lazy.Builder.RealFloat as Builder (realFloat)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (imap, map, toList, (!))
import Wasm.Types

printText :: Module -> Text
printText = Text.toStrict . Builder.toLazyText . buildModule

intercalateBuilder :: Builder -> [Builder] -> Builder
intercalateBuilder x xs = mconcat (intersperse x xs)

buildModule :: Module -> Builder
buildModule m =
    intercalateBuilder "\n  " (["(module"] ++ buildTypes types ++ buildFuncs types funcs ++ buildExports exports)
    <>
    "\n)\n"
    where
    funcs = moduleFuncs m
    types = moduleTypes m
    exports = moduleExports m

buildIndex :: Int -> Builder
buildIndex i =
    mconcat ["(;", Builder.decimal i, ";)"]

buildTypes :: Vector FuncType -> [Builder]
buildTypes = Vector.toList . Vector.imap buildType

buildType :: Int -> FuncType -> Builder
buildType typeidx (FuncType (ResultType params) (ResultType results)) =
    intercalateBuilder " "
    [ "(type"
    , buildIndex typeidx
    , buildFuncType params results
    ]
    <>
    ")"

buildFuncType :: [ValType] -> [ValType] -> Builder
buildFuncType [] [] = "(func)"
buildFuncType params [] =
    intercalateBuilder " "
    [ "(func"
    , buildParam params
    ]
    <>
    ")"
buildFuncType [] results =
    intercalateBuilder " "
    [ "(func"
    , buildResult results
    ]
    <>
    ")"
buildFuncType params results =
    intercalateBuilder " "
    [ "(func"
    , buildParam params
    , buildResult results
    ]
    <>
    ")"

buildParam :: [ValType] -> Builder
buildParam params =
    intercalateBuilder " " (["(param"] <> map buildValType params)
    <>
    ")"

buildResult :: [ValType] -> Builder
buildResult params =
    intercalateBuilder " " (["(result"] <> map buildValType params)
    <>
    ")"

buildValType :: ValType -> Builder
buildValType (NumType I32)       = "i32"
buildValType (NumType I64)       = "i64"
buildValType (NumType F32)       = "f32"
buildValType (NumType F64)       = "f64"
buildValType (RefType ExternRef) = "externref"
buildValType (RefType FuncRef)   = "funcref"

buildFuncs :: Vector FuncType -> Vector Func -> [Builder]
buildFuncs types = Vector.toList . Vector.imap f
    where
    f idx func =
        let tidx = fromIntegral (funcType func)
            ftype = types Vector.! tidx
        in buildFunc ftype idx func

buildFunc :: FuncType -> Int -> Func -> Builder
buildFunc (FuncType (ResultType params) (ResultType results)) funcidx (Func typeidx locals body) =
    intercalateBuilder "\n    "
    [ "(func"
    , buildIndex funcidx
    , mconcat ["(type ", Builder.decimal typeidx, ")"]
    , buildParam params
    , buildResult results
    , buildFuncBody body
    ]
    <>
    "  )"

buildFuncBody :: [Instr] -> Builder
buildFuncBody instrs =
    intercalateBuilder "\n    " (map buildInstr instrs)
    <>
    "\n"

buildInstr :: Instr -> Builder
buildInstr Nop                  = "nop"
buildInstr Unreachable          = "unreachable"
buildInstr (I32Const a)         = "i32.const " <> Builder.decimal a
buildInstr (I64Const a)         = "i64.const " <> Builder.decimal a
buildInstr (F32Const a)         = "f32.const " <> Builder.realFloat a
buildInstr (F64Const a)         = "f64.const " <> Builder.realFloat a
buildInstr (I32Unary Clz)       = "i32.clz"
buildInstr (I32Unary Ctz)       = "i32.ctz"
buildInstr (I32Unary Popcnt)    = "i32.popcnt"
buildInstr (I64Unary Clz)       = "i64.clz"
buildInstr (I64Unary Ctz)       = "i64.ctz"
buildInstr (I64Unary Popcnt)    = "i64.popcnt"
buildInstr (F32Unary Abs)       = "f32.abs"
buildInstr (F32Unary Neg)       = "f32.neg"
buildInstr (F32Unary Sqrt)      = "f32.sqrt"
buildInstr (F32Unary Ceil)      = "f32.ceil"
buildInstr (F32Unary Floor)     = "f32.floor"
buildInstr (F32Unary Trunc)     = "f32.trunc"
buildInstr (F32Unary Nearest)   = "f32.nearest"
buildInstr (F64Unary Abs)       = "f64.abs"
buildInstr (F64Unary Neg)       = "f64.neg"
buildInstr (F64Unary Sqrt)      = "f64.sqrt"
buildInstr (F64Unary Ceil)      = "f64.ceil"
buildInstr (F64Unary Floor)     = "f64.floor"
buildInstr (F64Unary Trunc)     = "f64.trunc"
buildInstr (F64Unary Nearest)   = "f64.nearest"
buildInstr (I32Binary IAdd)     = "i32.add"
buildInstr (I32Binary ISub)     = "i32.sub"
buildInstr (I32Binary IMul)     = "i32.mul"
buildInstr (I32Binary DivS)     = "i32.div_s"
buildInstr (I32Binary DivU)     = "i32.div_u"
buildInstr (I32Binary RemS)     = "i32.rem_s"
buildInstr (I32Binary RemU)     = "i32.rem_u"
buildInstr (I32Binary And)      = "i32.and"
buildInstr (I32Binary Or)       = "i32.or"
buildInstr (I32Binary Xor)      = "i32.xor"
buildInstr (I32Binary Shl)      = "i32.shl"
buildInstr (I32Binary ShrS)     = "i32.shrs"
buildInstr (I32Binary ShrU)     = "i32.shru"
buildInstr (I32Binary Rotl)     = "i32.rotl"
buildInstr (I32Binary Rotr)     = "i32.rotr"
buildInstr (I64Binary IAdd)     = "i64.add"
buildInstr (I64Binary ISub)     = "i64.sub"
buildInstr (I64Binary IMul)     = "i64.mul"
buildInstr (I64Binary DivS)     = "i64.div_s"
buildInstr (I64Binary DivU)     = "i64.div_u"
buildInstr (I64Binary RemS)     = "i64.rem_s"
buildInstr (I64Binary RemU)     = "i64.rem_u"
buildInstr (I64Binary And)      = "i64.and"
buildInstr (I64Binary Or)       = "i64.or"
buildInstr (I64Binary Xor)      = "i64.xor"
buildInstr (I64Binary Shl)      = "i64.shl"
buildInstr (I64Binary ShrS)     = "i64.shrs"
buildInstr (I64Binary ShrU)     = "i64.shru"
buildInstr (I64Binary Rotl)     = "i64.rotl"
buildInstr (I64Binary Rotr)     = "i64.rotr"
buildInstr (F32Binary Add)      = "f32.add"
buildInstr (F32Binary Sub)      = "f32.sub"
buildInstr (F32Binary Mul)      = "f32.mul"
buildInstr (F32Binary Div)      = "f32.div"
buildInstr (F32Binary Min)      = "f32.min"
buildInstr (F32Binary Max)      = "f32.max"
buildInstr (F32Binary Copysign) = "f32.copysign"
buildInstr (F64Binary Add)      = "f64.add"
buildInstr (F64Binary Sub)      = "f64.sub"
buildInstr (F64Binary Mul)      = "f64.mul"
buildInstr (F64Binary Div)      = "f64.div"
buildInstr (F64Binary Min)      = "f64.min"
buildInstr (F64Binary Max)      = "f64.max"
buildInstr (F64Binary Copysign) = "f64.copysign"
buildInstr (I32Test Eqz)        = "i32.eqz"
buildInstr (I64Test Eqz)        = "i64.eqz"
buildInstr (I32Relation IEq)    = "i32.eq"
buildInstr (I32Relation INe)    = "i32.ne"
buildInstr (I32Relation LtS)    = "i32.lt_s"
buildInstr (I32Relation LtU)    = "i32.lt_u"
buildInstr (I32Relation GtS)    = "i32.gt_s"
buildInstr (I32Relation GtU)    = "i32.gt_u"
buildInstr (I32Relation LeS)    = "i32.le_s"
buildInstr (I32Relation LeU)    = "i32.le_u"
buildInstr (I32Relation GeS)    = "i32.ge_s"
buildInstr (I32Relation GeU)    = "i32.ge_u"
buildInstr (I64Relation IEq)    = "i64.eq"
buildInstr (I64Relation INe)    = "i64.ne"
buildInstr (I64Relation LtS)    = "i64.lt_s"
buildInstr (I64Relation LtU)    = "i64.lt_u"
buildInstr (I64Relation GtS)    = "i64.gt_s"
buildInstr (I64Relation GtU)    = "i64.gt_u"
buildInstr (I64Relation LeS)    = "i64.le_s"
buildInstr (I64Relation LeU)    = "i64.le_u"
buildInstr (I64Relation GeS)    = "i64.ge_s"
buildInstr (I64Relation GeU)    = "i64.ge_u"
buildInstr (F32Relation FEq)    = "f32.eq"
buildInstr (F32Relation FNe)    = "f32.ne"
buildInstr (F32Relation Lt)     = "f32.lt"
buildInstr (F32Relation Gt)     = "f32.gt"
buildInstr (F32Relation Le)     = "f32.le"
buildInstr (F32Relation Ge)     = "f32.ge"
buildInstr (F64Relation FEq)    = "f64.eq"
buildInstr (F64Relation FNe)    = "f64.ne"
buildInstr (F64Relation Lt)     = "f64.lt"
buildInstr (F64Relation Gt)     = "f64.gt"
buildInstr (F64Relation Le)     = "f64.le"
buildInstr (F64Relation Ge)     = "f64.ge"
buildInstr (LocalGet a)         = "local.get " <> Builder.decimal a
buildInstr (LocalSet a)         = "local.set " <> Builder.decimal a
buildInstr (LocalTee a)         = "local.tee " <> Builder.decimal a
buildInstr (GlobalGet a)        = "global.get " <> Builder.decimal a
buildInstr (GlobalSet a)        = "global.set " <> Builder.decimal a

buildExports :: Vector Export -> [Builder]
buildExports = Vector.toList . Vector.map buildExport

buildExport (Export name (ExportFunc idx)) = mconcat ["(export ", Builder.fromText name, " (func ", Builder.decimal idx, ")"]
buildExport (Export name (ExportTable idx)) = mconcat ["(export ", Builder.fromText name, " (table ", Builder.decimal idx, ")"]
buildExport (Export name (ExportMemory idx)) = mconcat ["(export ", Builder.fromText name, " (mem ", Builder.decimal idx, ")"]
buildExport (Export name (ExportGlobal idx)) = mconcat ["(export ", Builder.fromText name, " (global ", Builder.decimal idx, ")"]
