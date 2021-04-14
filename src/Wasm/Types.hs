module Wasm.Types
    ( Func(..)
    , Instr(..)
    , Module(..)
    , NumType(..)
    , TypeIdx(..)
    , ValType(..)
    , IUnop(..)
    , IBinop(..)
    , FUnop(..)
    , FBinop(..)
    , ITestop(..)
    , IRelop(..)
    , FRelop(..)
    ) where

import Data.Word (Word32, Word64)

data Module = Module
    { moduleFuncs    :: ![Func]
    --, moduleTypes    :: ![Type']
    --, moduleGlobals  :: ![Global]
    --, moduleTables   :: ![Table]
    --, moduleMemories :: ![Memory]
    --, moduleDatas    :: ![DataSegment]
    --, moduleImports  :: ![Import]
    --, moduleExports  :: ![Export]
    } deriving (Show, Eq)

newtype TypeIdx = TypeIdx Int
    deriving (Show, Eq)

data NumType =
    I32 |
    I64 |
    F32 |
    F64
    deriving (Show, Eq)

data ValType =
    NumType
    -- RefType
    deriving (Show, Eq)

data Func = Func
    { funcType   :: !TypeIdx
    , funcLocals :: ![ValType]
    , funcBody   :: ![Instr]
    } deriving (Show, Eq)

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
    F64Relation !FRelop
    deriving (Show, Eq)
