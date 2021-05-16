{-# LANGUAGE OverloadedStrings #-}
module Wasm.TextPrinter
    ( printText
    ) where

import qualified Data.Char as Char (showLitChar)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as Text (foldl')
import qualified Data.Text.Lazy as Text (toStrict)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder (fromString, fromText,
                                                    toLazyText)
import qualified Data.Text.Lazy.Builder.Int as Builder (decimal)
import qualified Data.Text.Lazy.Builder.RealFloat as Builder (realFloat)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (imap, length, map, toList, (!))
import Wasm.Types

printText :: Module -> Text
printText = Text.toStrict . Builder.toLazyText . buildModule

intercalateBuilder :: Builder -> [Builder] -> Builder
intercalateBuilder x xs = mconcat (intersperse x xs)

buildModule :: Module -> Builder
buildModule m =
    intercalateBuilder "\n  "
        ( ["(module"] ++
          buildImports imports ++
          buildTypes types ++
          buildFuncs (Vector.length imports) types funcs ++
          buildGlobals globals ++
          buildExports exports ++
          buildMemories memories ++
          buildDataSegments datas
        )
    <>
    "\n)\n"
    where
    funcs = moduleFuncs m
    types = moduleTypes m
    globals = moduleGlobals m
    imports = moduleImports m
    exports = moduleExports m
    memories = moduleMemories m
    datas = moduleDatas m

buildIndexComment :: Int -> Builder
buildIndexComment i =
    mconcat ["(;", Builder.decimal i, ";)"]

buildTypes :: Vector FuncType -> [Builder]
buildTypes = Vector.toList . Vector.imap buildType

buildType :: Int -> FuncType -> Builder
buildType typeidx (FuncType (ResultType params) (ResultType results)) =
    intercalateBuilder " "
    [ "(type"
    , buildIndexComment typeidx
    , buildFuncType params results
    ]
    <>
    ")"

buildFuncType :: Vector ValType -> Vector ValType -> Builder
buildFuncType params results =
    intercalateBuilder " "
        ( "(func" :
          [ buildParam params | not (null params)] ++
          [ buildResult results | not (null results) ]
        )
    <> ")"

buildParam :: Vector ValType -> Builder
buildParam params =
    intercalateBuilder " " ("(param" : map buildValType (Vector.toList params))
    <> ")"

buildLocals :: Vector ValType -> Builder
buildLocals locals =
    intercalateBuilder " " ("(local" : map buildValType (Vector.toList locals))
    <> ")"

buildResult :: Vector ValType -> Builder
buildResult params =
    intercalateBuilder " " ("(result" : map buildValType (Vector.toList params))
    <> ")"

buildValType :: ValType -> Builder
buildValType (NumType I32)       = "i32"
buildValType (NumType I64)       = "i64"
buildValType (NumType F32)       = "f32"
buildValType (NumType F64)       = "f64"
buildValType (RefType ExternRef) = "externref"
buildValType (RefType FuncRef)   = "funcref"

buildFuncs :: Int -> Vector FuncType -> Vector Func -> [Builder]
buildFuncs start types = Vector.toList . Vector.imap f
    where
    f idx func =
        let tidx = fromIntegral (funcType func)
            ftype = types Vector.! tidx
        in buildFunc ftype (start + idx) func

buildFunc :: FuncType -> Int -> Func -> Builder
buildFunc (FuncType (ResultType params) (ResultType results)) funcidx (Func typeidx locals body) =
    intercalateBuilder "\n"
        ( [ intercalateBuilder " " $ ["(func", buildIndexComment funcidx, "(type " <> Builder.decimal typeidx <> ")"] ++
              [ buildParam params | not (null params) ] ++
              [ buildResult results | not (null results) ]
          ] ++
          [ indent <> buildLocals locals | not (null locals) ] ++
          [ buildInstrVec indent body ]
        )
    <>
    "  )"
    where
    indent = "    "

buildInstrVec :: Builder -> Vector Instr -> Builder
buildInstrVec indent instrs =
    intercalateBuilder "\n" (map (buildInstr indent) (Vector.toList instrs))
    <>
    "\n"

buildInstr :: Builder -> Instr -> Builder
buildInstr indent Nop                  = indent <> "nop"
buildInstr indent Unreachable          = indent <> "unreachable"
buildInstr indent (I32Const a)         = indent <> "i32.const " <> Builder.decimal a
buildInstr indent (I64Const a)         = indent <> "i64.const " <> Builder.decimal a
buildInstr indent (F32Const a)         = indent <> "f32.const " <> Builder.realFloat a
buildInstr indent (F64Const a)         = indent <> "f64.const " <> Builder.realFloat a
buildInstr indent (I32Unary Clz)       = indent <> "i32.clz"
buildInstr indent (I32Unary Ctz)       = indent <> "i32.ctz"
buildInstr indent (I32Unary Popcnt)    = indent <> "i32.popcnt"
buildInstr indent (I64Unary Clz)       = indent <> "i64.clz"
buildInstr indent (I64Unary Ctz)       = indent <> "i64.ctz"
buildInstr indent (I64Unary Popcnt)    = indent <> "i64.popcnt"
buildInstr indent (F32Unary Abs)       = indent <> "f32.abs"
buildInstr indent (F32Unary Neg)       = indent <> "f32.neg"
buildInstr indent (F32Unary Sqrt)      = indent <> "f32.sqrt"
buildInstr indent (F32Unary Ceil)      = indent <> "f32.ceil"
buildInstr indent (F32Unary Floor)     = indent <> "f32.floor"
buildInstr indent (F32Unary Trunc)     = indent <> "f32.trunc"
buildInstr indent (F32Unary Nearest)   = indent <> "f32.nearest"
buildInstr indent (F64Unary Abs)       = indent <> "f64.abs"
buildInstr indent (F64Unary Neg)       = indent <> "f64.neg"
buildInstr indent (F64Unary Sqrt)      = indent <> "f64.sqrt"
buildInstr indent (F64Unary Ceil)      = indent <> "f64.ceil"
buildInstr indent (F64Unary Floor)     = indent <> "f64.floor"
buildInstr indent (F64Unary Trunc)     = indent <> "f64.trunc"
buildInstr indent (F64Unary Nearest)   = indent <> "f64.nearest"
buildInstr indent (I32Binary IAdd)     = indent <> "i32.add"
buildInstr indent (I32Binary ISub)     = indent <> "i32.sub"
buildInstr indent (I32Binary IMul)     = indent <> "i32.mul"
buildInstr indent (I32Binary DivS)     = indent <> "i32.div_s"
buildInstr indent (I32Binary DivU)     = indent <> "i32.div_u"
buildInstr indent (I32Binary RemS)     = indent <> "i32.rem_s"
buildInstr indent (I32Binary RemU)     = indent <> "i32.rem_u"
buildInstr indent (I32Binary And)      = indent <> "i32.and"
buildInstr indent (I32Binary Or)       = indent <> "i32.or"
buildInstr indent (I32Binary Xor)      = indent <> "i32.xor"
buildInstr indent (I32Binary Shl)      = indent <> "i32.shl"
buildInstr indent (I32Binary ShrS)     = indent <> "i32.shrs"
buildInstr indent (I32Binary ShrU)     = indent <> "i32.shru"
buildInstr indent (I32Binary Rotl)     = indent <> "i32.rotl"
buildInstr indent (I32Binary Rotr)     = indent <> "i32.rotr"
buildInstr indent (I64Binary IAdd)     = indent <> "i64.add"
buildInstr indent (I64Binary ISub)     = indent <> "i64.sub"
buildInstr indent (I64Binary IMul)     = indent <> "i64.mul"
buildInstr indent (I64Binary DivS)     = indent <> "i64.div_s"
buildInstr indent (I64Binary DivU)     = indent <> "i64.div_u"
buildInstr indent (I64Binary RemS)     = indent <> "i64.rem_s"
buildInstr indent (I64Binary RemU)     = indent <> "i64.rem_u"
buildInstr indent (I64Binary And)      = indent <> "i64.and"
buildInstr indent (I64Binary Or)       = indent <> "i64.or"
buildInstr indent (I64Binary Xor)      = indent <> "i64.xor"
buildInstr indent (I64Binary Shl)      = indent <> "i64.shl"
buildInstr indent (I64Binary ShrS)     = indent <> "i64.shrs"
buildInstr indent (I64Binary ShrU)     = indent <> "i64.shru"
buildInstr indent (I64Binary Rotl)     = indent <> "i64.rotl"
buildInstr indent (I64Binary Rotr)     = indent <> "i64.rotr"
buildInstr indent (F32Binary Add)      = indent <> "f32.add"
buildInstr indent (F32Binary Sub)      = indent <> "f32.sub"
buildInstr indent (F32Binary Mul)      = indent <> "f32.mul"
buildInstr indent (F32Binary Div)      = indent <> "f32.div"
buildInstr indent (F32Binary Min)      = indent <> "f32.min"
buildInstr indent (F32Binary Max)      = indent <> "f32.max"
buildInstr indent (F32Binary Copysign) = indent <> "f32.copysign"
buildInstr indent (F64Binary Add)      = indent <> "f64.add"
buildInstr indent (F64Binary Sub)      = indent <> "f64.sub"
buildInstr indent (F64Binary Mul)      = indent <> "f64.mul"
buildInstr indent (F64Binary Div)      = indent <> "f64.div"
buildInstr indent (F64Binary Min)      = indent <> "f64.min"
buildInstr indent (F64Binary Max)      = indent <> "f64.max"
buildInstr indent (F64Binary Copysign) = indent <> "f64.copysign"
buildInstr indent (I32Test Eqz)        = indent <> "i32.eqz"
buildInstr indent (I64Test Eqz)        = indent <> "i64.eqz"
buildInstr indent (I32Relation IEq)    = indent <> "i32.eq"
buildInstr indent (I32Relation INe)    = indent <> "i32.ne"
buildInstr indent (I32Relation LtS)    = indent <> "i32.lt_s"
buildInstr indent (I32Relation LtU)    = indent <> "i32.lt_u"
buildInstr indent (I32Relation GtS)    = indent <> "i32.gt_s"
buildInstr indent (I32Relation GtU)    = indent <> "i32.gt_u"
buildInstr indent (I32Relation LeS)    = indent <> "i32.le_s"
buildInstr indent (I32Relation LeU)    = indent <> "i32.le_u"
buildInstr indent (I32Relation GeS)    = indent <> "i32.ge_s"
buildInstr indent (I32Relation GeU)    = indent <> "i32.ge_u"
buildInstr indent (I64Relation IEq)    = indent <> "i64.eq"
buildInstr indent (I64Relation INe)    = indent <> "i64.ne"
buildInstr indent (I64Relation LtS)    = indent <> "i64.lt_s"
buildInstr indent (I64Relation LtU)    = indent <> "i64.lt_u"
buildInstr indent (I64Relation GtS)    = indent <> "i64.gt_s"
buildInstr indent (I64Relation GtU)    = indent <> "i64.gt_u"
buildInstr indent (I64Relation LeS)    = indent <> "i64.le_s"
buildInstr indent (I64Relation LeU)    = indent <> "i64.le_u"
buildInstr indent (I64Relation GeS)    = indent <> "i64.ge_s"
buildInstr indent (I64Relation GeU)    = indent <> "i64.ge_u"
buildInstr indent (F32Relation FEq)    = indent <> "f32.eq"
buildInstr indent (F32Relation FNe)    = indent <> "f32.ne"
buildInstr indent (F32Relation Lt)     = indent <> "f32.lt"
buildInstr indent (F32Relation Gt)     = indent <> "f32.gt"
buildInstr indent (F32Relation Le)     = indent <> "f32.le"
buildInstr indent (F32Relation Ge)     = indent <> "f32.ge"
buildInstr indent (F64Relation FEq)    = indent <> "f64.eq"
buildInstr indent (F64Relation FNe)    = indent <> "f64.ne"
buildInstr indent (F64Relation Lt)     = indent <> "f64.lt"
buildInstr indent (F64Relation Gt)     = indent <> "f64.gt"
buildInstr indent (F64Relation Le)     = indent <> "f64.le"
buildInstr indent (F64Relation Ge)     = indent <> "f64.ge"
buildInstr indent (RefFunc a)          = indent <> "ref.func " <> Builder.decimal a
buildInstr indent (LocalGet a)         = indent <> "local.get " <> Builder.decimal a
buildInstr indent (LocalSet a)         = indent <> "local.set " <> Builder.decimal a
buildInstr indent (LocalTee a)         = indent <> "local.tee " <> Builder.decimal a
buildInstr indent (GlobalGet a)        = indent <> "global.get " <> Builder.decimal a
buildInstr indent (GlobalSet a)        = indent <> "global.set " <> Builder.decimal a
buildInstr indent (Block bt xs)        = indent <> buildBlockHeader "block" bt <> "\n" <> buildInstrVec (indent <> "  ") xs <> indent <> "end"
buildInstr indent (Loop bt xs)         = indent <> buildBlockHeader "loop" bt <> "\n" <> buildInstrVec (indent <> "  ") xs <> indent <> "end"
buildInstr indent (If bt xs ys)        = indent <> buildBlockHeader "if" bt <> "\n" <> buildInstrVec (indent <> "  ") xs <> indent <> "else\n" <> buildInstrVec (indent <> "  ") ys <> indent <> "end"
buildInstr indent (Br a)               = indent <> "br " <> Builder.decimal a
buildInstr indent (BrIf a)             = indent <> "br_if " <> Builder.decimal a
buildInstr indent Return               = indent <> "return"
buildInstr indent (Call a)             = indent <> "call " <> Builder.decimal a
buildInstr indent Drop                 = indent <> "drop"

buildBlockHeader :: Builder -> BlockType -> Builder
buildBlockHeader name bt =
    intercalateBuilder " " $ name : [buildBlockType bt | bt /= BlockTypeEmpty]

buildBlockType :: BlockType -> Builder
buildBlockType (BlockTypeIdx idx) = "(type " <> Builder.decimal idx <> ")"
buildBlockType (BlockTypeFuncType (FuncType (ResultType params) (ResultType results))) =
    intercalateBuilder " " $
        [ buildParam params | not (null params)] ++
        [ buildResult results | not (null results) ]
buildBlockType BlockTypeEmpty = mempty

buildGlobals :: Vector Global -> [Builder]
buildGlobals = Vector.toList . Vector.map buildGlobal

buildGlobal :: Global -> Builder
buildGlobal (Global mut valType ini) =
    "(global " <> buildGlobalType mut valType <> " (" <> buildInstr mempty ini <> "))"

buildGlobalType :: Mut -> ValType -> Builder
buildGlobalType Const valType = buildValType valType
buildGlobalType Var valType   = "(mut " <> buildValType valType <> ")"

buildImports :: Vector Import -> [Builder]
buildImports = Vector.toList . Vector.imap buildImport

buildImport :: Int -> Import -> Builder
buildImport idx (Import moduleName name (ImportFunc (FuncType (ResultType params) (ResultType results)))) =
    intercalateBuilder " "
    ["(import"
    , buildIndexComment idx
    , "\"" <> Builder.fromText moduleName <> "\""
    , "\"" <> Builder.fromText name <> "\""
    , intercalateBuilder " " (["(func"] ++ [ buildParam params | not (null params) ] ++ [ buildResult results | not (null results) ]) <> ")"
    ]
    <>
    ")"

buildExports :: Vector Export -> [Builder]
buildExports = Vector.toList . Vector.map buildExport

buildExport :: Export -> Builder
buildExport (Export name (ExportFunc idx)) = mconcat ["(export \"", Builder.fromText name, "\" (func ", Builder.decimal idx, "))"]
buildExport (Export name (ExportTable idx)) = mconcat ["(export \"", Builder.fromText name, "\" (table ", Builder.decimal idx, "))"]
buildExport (Export name (ExportMemory idx)) = mconcat ["(export \"", Builder.fromText name, "\" (memory ", Builder.decimal idx, "))"]
buildExport (Export name (ExportGlobal idx)) = mconcat ["(export \"", Builder.fromText name, "\" (global ", Builder.decimal idx, "))"]

buildMemories :: Vector Memory -> [Builder]
buildMemories = Vector.toList . Vector.imap buildMemory

buildMemory :: Int -> Memory -> Builder
buildMemory i (Memory (Limits min_ Nothing)) = mconcat ["(memory ", buildIndexComment i, " ", Builder.decimal min_, ")"]
buildMemory i (Memory (Limits min_ (Just max_))) = mconcat ["(memory ", buildIndexComment i, " ", Builder.decimal min_, " ", Builder.decimal max_, ")"]

buildDataSegments :: Vector DataSegment -> [Builder]
buildDataSegments = Vector.toList . Vector.imap buildDataSegment

buildDataSegment :: Int -> DataSegment -> Builder
buildDataSegment i (DataSegment s Nothing) = mconcat ["(data ", buildIndexComment i, " ", buildStringLiteral s, ")"]
buildDataSegment i (DataSegment s (Just off)) = mconcat ["(data ", buildIndexComment i, " (i32.const ", Builder.decimal off ,") ", buildStringLiteral s, ")"]

buildStringLiteral :: Text -> Builder
buildStringLiteral s = "\"" <> Builder.fromString (Text.foldl' (\a c -> a . Char.showLitChar c) id s "") <> "\""
