module Wasm
    ( Func(..)
    , Instr(..)
    , Module(..)
    , NumType(..)
    , TypeIdx(..)
    , ValType(..)
    ) where

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

data Instr =
    Nop |
    Unreachable
    deriving (Show, Eq)
