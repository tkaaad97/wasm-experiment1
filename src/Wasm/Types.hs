module Wasm.Types
    ( Func(..)
    , Instr(..)
    , Module(..)
    , NumType(..)
    , RefType(..)
    , ValType(..)
    , ResultType(..)
    , FuncType(..)
    , BlockType(..)
    , IUnop(..)
    , IBinop(..)
    , FUnop(..)
    , FBinop(..)
    , ITestop(..)
    , IRelop(..)
    , FRelop(..)
    , Export(..)
    , ExportDesc(..)
    ) where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word32, Word64)

data Module = Module
    { moduleTypes   :: !(Vector FuncType)
    , moduleFuncs   :: !(Vector Func)
    --, moduleGlobals :: !(Vector Global)
    --, moduleTables   :: !(Vector Table)
    --, moduleMemories :: !(Vector Memory)
    --, moduleDatas    :: !(Vector DataSegment)
    --, moduleImports  :: !(Vector Import)
    , moduleExports :: !(Vector Export)
    } deriving (Show, Eq)

data NumType =
    I32 |
    I64 |
    F32 |
    F64
    deriving (Show, Eq)

data RefType =
    ExternRef |
    FuncRef
    deriving (Show, Eq)

data ValType =
    NumType !NumType |
    RefType !RefType
    deriving (Show, Eq)

newtype ResultType = ResultType (Vector ValType)
    deriving (Show, Eq)

data FuncType = FuncType !ResultType !ResultType
    deriving (Show, Eq)

data Func = Func
    { funcType   :: !Word32
    , funcLocals :: !(Vector ValType)
    , funcBody   :: !(Vector Instr)
    } deriving (Show, Eq)

data BlockType =
    BlockTypeIdx !Word32 |
    BlockTypeFuncType !FuncType |
    BlockTypeEmpty
    deriving (Show, Eq)

data IUnop =
    Clz |
    Ctz |
    Popcnt
    deriving (Show, Eq)

data IBinop =
    IAdd |
    ISub |
    IMul |
    DivS |
    DivU |
    RemS |
    RemU |
    And |
    Or |
    Xor |
    Shl |
    ShrS |
    ShrU |
    Rotl |
    Rotr
    deriving (Show, Eq)

data FUnop =
    Abs |
    Neg |
    Sqrt |
    Ceil |
    Floor |
    Trunc |
    Nearest
    deriving (Show, Eq)

data FBinop =
    Add |
    Sub |
    Mul |
    Div |
    Min |
    Max |
    Copysign
    deriving (Show, Eq)

data ITestop = Eqz
    deriving (Show, Eq)

data IRelop =
    IEq |
    INe |
    LtS |
    LtU |
    GtS |
    GtU |
    LeS |
    LeU |
    GeS |
    GeU
    deriving (Show, Eq)

data FRelop =
    FEq |
    FNe |
    Lt |
    Gt |
    Le |
    Ge
    deriving (Show, Eq)

data Instr =
    Nop |
    Unreachable |
    I32Const !Word32 |
    I64Const !Word64 |
    F32Const !Float |
    F64Const !Double |
    I32Unary !IUnop |
    I64Unary !IUnop |
    F32Unary !FUnop |
    F64Unary !FUnop |
    I32Binary !IBinop |
    I64Binary !IBinop |
    F32Binary !FBinop |
    F64Binary !FBinop |
    I32Test !ITestop |
    I64Test !ITestop |
    I32Relation !IRelop |
    I64Relation !IRelop |
    F32Relation !FRelop |
    F64Relation !FRelop |
    RefFunc !Word32 |
    LocalGet !Word32 |
    LocalSet !Word32 |
    LocalTee !Word32 |
    GlobalGet !Word32 |
    GlobalSet !Word32 |
    Block !BlockType !(Vector Instr) |
    Call !Word32
    deriving (Show, Eq)

data Export = Export
    { exportName :: !Text
    , exportDesc :: !ExportDesc
    } deriving (Show, Eq)

data ExportDesc =
    ExportFunc !Word32 |
    ExportTable !Word32 |
    ExportMemory !Word32 |
    ExportGlobal !Word32
    deriving (Show, Eq)
