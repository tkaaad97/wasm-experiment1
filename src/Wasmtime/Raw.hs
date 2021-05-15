{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Wasmtime.Raw
    ( WasmByteT
    , WasmByteVecT(..)
    , WasmFuncCallbackT
    , WasmEngineT
    , WasmExternT
    , WasmExternVecT(..)
    , WasmFuncT
    , WasmFuncTypeT
    , WasmInstanceT
    , WasmModuleT
    , WasmRefT
    , WasmStoreT
    , WasmTrapT
    , WasmValKindT
    , WasmValT(..)
    , WasmValTypeT
    , WasmValTypeVecT(..)
    , WasmValVecT(..)
    , WasmtimeErrorT
    , wasmValKindI32
    , wasmValKindI64
    , wasmValKindF32
    , wasmValKindF64
    , wasmValKindAnyRef
    , wasmValKindFuncRef
    , wasmByteVecDelete
    , wasmByteVecNew
    , wasmByteVecNewUninitialized
    , wasmEngineDelete
    , wasmEngineNew
    , wasmExternAsFunc
    , wasmExternVecNew
    , wasmExternVecDelete
    , wasmFuncAsExtern
    , wasmFuncDelete
    , wasmFuncNew
    , wasmFuncParamArity
    , wasmFuncResultArity
    , wasmFuncTypeDelete
    , wasmFuncTypeNew
    , wasmInstanceDelete
    , wasmInstanceExports
    , wasmModuleDelete
    , wasmStoreDelete
    , wasmStoreNew
    , wasmTrapDelete
    , wasmTrapMessage
    , wasmValTypeDelete
    , wasmValTypeKind
    , wasmValTypeNew
    , wasmValTypeVecDelete
    , wasmValVecDelete
    , wasmValVecNew
    , wasmValVecNewEmpty
    , wasmValVecNewUninitialized
    , wasmValTypeVecNew
    , wasmValTypeVecNewEmpty
    , wasmValTypeVecNewUninitialized
    , wasmtimeErrorDelete
    , wasmtimeErrorMessage
    , wasmtimeFuncCall
    , wasmtimeInstanceNew
    , wasmtimeModuleNew
    , wasmtimeWat2Wasm
    ) where

import Control.Exception (throwIO)
import Data.Word (Word32, Word64, Word8)
import Foreign (FunPtr, Ptr, Storable(..), castPtr, plusPtr)
import Foreign.C.Types (CSize(..))

data WasmEngineT

type WasmByteT = Word8

data WasmByteVecT = WasmByteVecT !CSize !(Ptr Word8)

data WasmExternT

data WasmExternVecT = WasmExternVecT !CSize !(Ptr (Ptr WasmExternT))

newtype WasmValKindT = WasmValKindT Word8
    deriving (Show, Eq, Storable)

data WasmRefT

data WasmValT =
    WasmValI32 !Word32 |
    WasmValI64 !Word64 |
    WasmValF32 !Float |
    WasmValF64 !Double |
    WasmValAnyRef !(Ptr WasmRefT) |
    WasmValFuncRef !(Ptr WasmRefT)
    deriving (Show, Eq)

data WasmValVecT = WasmValVecT !CSize !(Ptr WasmValT)

data WasmValTypeT

data WasmValTypeVecT = WasmValTypeVecT !CSize !(Ptr (Ptr WasmValTypeT))

type WasmFuncCallbackT = Foreign.FunPtr (Ptr WasmValVecT -> Ptr WasmValVecT -> IO (Ptr WasmTrapT))

data WasmFuncT

data WasmStoreT

data WasmModuleT

data WasmInstanceT

data WasmFuncTypeT

data WasmTrapT

data WasmtimeErrorT

instance Storable WasmByteVecT where
    sizeOf _ = 16
    alignment _ = 8

    peek ptr = do
        size <- peek $ castPtr ptr
        d <- peek $ castPtr ptr `plusPtr` 8
        return (WasmByteVecT size d)

    poke ptr (WasmByteVecT size d) = do
        poke (castPtr ptr) size
        poke (castPtr ptr `plusPtr` 8) d

instance Storable WasmExternVecT where
    sizeOf _ = 16
    alignment _ = 8

    peek ptr = do
        size <- peek $ castPtr ptr
        d <- peek $ castPtr ptr `plusPtr` 8
        return (WasmExternVecT size d)

    poke ptr (WasmExternVecT size d) = do
        poke (castPtr ptr) size
        poke (castPtr ptr `plusPtr` 8) d

instance Storable WasmValT where
    sizeOf _ = 16
    alignment _ = 8

    peek ptr =
        peekVal =<< peek (castPtr ptr)
        where
        peekVal k
            | k == wasmValKindI32 = do
                d <- peek $ castPtr ptr `plusPtr` 8
                return (WasmValI32 d)
            | k == wasmValKindI64 = do
                d <- peek $ castPtr ptr `plusPtr` 8
                return (WasmValI64 d)
            | k == wasmValKindF32 = do
                d <- peek $ castPtr ptr `plusPtr` 8
                return (WasmValF32 d)
            | k == wasmValKindF64 = do
                d <- peek $ castPtr ptr `plusPtr` 8
                return (WasmValF64 d)
            | k == wasmValKindAnyRef = do
                d <- peek $ castPtr ptr `plusPtr` 8
                return (WasmValAnyRef d)
            | k == wasmValKindFuncRef = do
                d <- peek $ castPtr ptr `plusPtr` 8
                return (WasmValFuncRef d)
            | otherwise = throwIO . userError $ "Unknown kind: " ++ show k

    poke ptr (WasmValI32 a) = do
        poke (castPtr ptr) wasmValKindI32
        poke (castPtr ptr `plusPtr` 8) a
    poke ptr (WasmValI64 a) = do
        poke (castPtr ptr) wasmValKindI64
        poke (castPtr ptr `plusPtr` 8) a
    poke ptr (WasmValF32 a) = do
        poke (castPtr ptr) wasmValKindF32
        poke (castPtr ptr `plusPtr` 8) a
    poke ptr (WasmValF64 a) = do
        poke (castPtr ptr) wasmValKindF64
        poke (castPtr ptr `plusPtr` 8) a
    poke ptr (WasmValAnyRef a) = do
        poke (castPtr ptr) wasmValKindAnyRef
        poke (castPtr ptr `plusPtr` 8) a
    poke ptr (WasmValFuncRef a) = do
        poke (castPtr ptr) wasmValKindFuncRef
        poke (castPtr ptr `plusPtr` 8) a

instance Storable WasmValVecT where
    sizeOf _ = 16
    alignment _ = 8

    peek ptr = do
        size <- peek $ castPtr ptr
        d <- peek $ castPtr ptr `plusPtr` 8
        return (WasmValVecT size d)

    poke ptr (WasmValVecT size d) = do
        poke (castPtr ptr) size
        poke (castPtr ptr `plusPtr` 8) d

instance Storable WasmValTypeVecT where
    sizeOf _ = 16
    alignment _ = 8

    peek ptr = do
        size <- peek $ castPtr ptr
        d <- peek $ castPtr ptr `plusPtr` 8
        return (WasmValTypeVecT size d)

    poke ptr (WasmValTypeVecT size d) = do
        poke (castPtr ptr) size
        poke (castPtr ptr `plusPtr` 8) d

wasmValKindI32 :: WasmValKindT
wasmValKindI32 = WasmValKindT 0

wasmValKindI64 :: WasmValKindT
wasmValKindI64 = WasmValKindT 1

wasmValKindF32 :: WasmValKindT
wasmValKindF32 = WasmValKindT 2

wasmValKindF64 :: WasmValKindT
wasmValKindF64 = WasmValKindT 3

wasmValKindAnyRef :: WasmValKindT
wasmValKindAnyRef = WasmValKindT 128

wasmValKindFuncRef :: WasmValKindT
wasmValKindFuncRef = WasmValKindT 129

foreign import ccall "wasm_byte_vec_delete" wasmByteVecDelete :: Ptr WasmByteVecT -> IO ()
foreign import ccall "wasm_byte_vec_new" wasmByteVecNew :: Ptr WasmByteVecT -> CSize -> Ptr WasmByteT -> IO ()
foreign import ccall "wasm_byte_vec_new_uninitialized" wasmByteVecNewUninitialized :: Ptr WasmByteVecT -> CSize -> IO ()
foreign import ccall "wasm_engine_delete" wasmEngineDelete :: Ptr WasmEngineT -> IO ()
foreign import ccall "wasm_engine_new" wasmEngineNew :: IO (Ptr WasmEngineT)
foreign import ccall "wasm_extern_as_func" wasmExternAsFunc :: Ptr WasmExternT -> IO (Ptr WasmFuncT)
foreign import ccall "wasm_extern_vec_new" wasmExternVecNew :: Ptr WasmExternVecT -> CSize -> Ptr (Ptr WasmExternT) -> IO ()
foreign import ccall "wasm_extern_vec_delete" wasmExternVecDelete :: Ptr WasmExternVecT -> IO ()
foreign import ccall "wasm_func_as_extern" wasmFuncAsExtern :: Ptr WasmFuncT -> IO (Ptr WasmExternT)
foreign import ccall "wasm_func_delete" wasmFuncDelete :: Ptr WasmFuncT -> IO ()
foreign import ccall "wasm_func_new" wasmFuncNew :: Ptr WasmStoreT -> Ptr WasmFuncTypeT -> WasmFuncCallbackT -> IO (Ptr WasmFuncT)
foreign import ccall "wasm_func_param_arity" wasmFuncParamArity :: Ptr WasmFuncT -> IO CSize
foreign import ccall "wasm_func_result_arity" wasmFuncResultArity :: Ptr WasmFuncT -> IO CSize
foreign import ccall "wasm_functype_delete" wasmFuncTypeDelete :: Ptr WasmFuncTypeT -> IO ()
foreign import ccall "wasm_functype_new" wasmFuncTypeNew :: Ptr WasmValTypeVecT -> Ptr WasmValTypeVecT -> IO (Ptr WasmFuncTypeT)
foreign import ccall "wasm_instance_delete" wasmInstanceDelete :: Ptr WasmInstanceT -> IO ()
foreign import ccall "wasm_instance_exports" wasmInstanceExports :: Ptr WasmInstanceT -> Ptr WasmExternVecT -> IO ()
foreign import ccall "wasm_module_delete" wasmModuleDelete :: Ptr WasmModuleT -> IO ()
foreign import ccall "wasm_store_delete" wasmStoreDelete :: Ptr WasmStoreT -> IO ()
foreign import ccall "wasm_store_new" wasmStoreNew :: Ptr WasmEngineT -> IO (Ptr WasmStoreT)
foreign import ccall "wasm_trap_delete" wasmTrapDelete :: Ptr WasmTrapT -> IO ()
foreign import ccall "wasm_trap_message" wasmTrapMessage :: Ptr WasmTrapT -> Ptr WasmByteVecT -> IO ()
foreign import ccall "wasm_valtype_delete" wasmValTypeDelete :: Ptr WasmValTypeT -> IO ()
foreign import ccall "wasm_valtype_kind" wasmValTypeKind :: Ptr WasmValTypeT -> IO WasmValKindT
foreign import ccall "wasm_valtype_new" wasmValTypeNew :: WasmValKindT -> IO (Ptr WasmValTypeT)
foreign import ccall "wasm_valtype_vec_delete" wasmValTypeVecDelete :: Ptr WasmValTypeVecT -> IO ()
foreign import ccall "wasm_val_vec_delete" wasmValVecDelete :: Ptr WasmValVecT -> IO ()
foreign import ccall "wasm_val_vec_new" wasmValVecNew :: Ptr WasmValVecT -> CSize -> Ptr WasmValT -> IO ()
foreign import ccall "wasm_val_vec_new_empty" wasmValVecNewEmpty :: Ptr WasmValVecT -> IO ()
foreign import ccall "wasm_val_vec_new_uninitialized" wasmValVecNewUninitialized :: Ptr WasmValVecT -> CSize -> IO ()
foreign import ccall "wasm_valtype_vec_new" wasmValTypeVecNew :: Ptr WasmValTypeVecT -> CSize -> Ptr (Ptr WasmValTypeT) -> IO ()
foreign import ccall "wasm_valtype_vec_new_empty" wasmValTypeVecNewEmpty :: Ptr WasmValTypeVecT -> IO ()
foreign import ccall "wasm_valtype_vec_new_uninitialized" wasmValTypeVecNewUninitialized :: Ptr WasmValTypeVecT -> CSize -> IO ()
foreign import ccall "wasmtime_error_delete" wasmtimeErrorDelete :: Ptr WasmtimeErrorT -> IO ()
foreign import ccall "wasmtime_error_message" wasmtimeErrorMessage :: Ptr WasmtimeErrorT -> Ptr WasmByteVecT -> IO ()
foreign import ccall "wasmtime_func_call" wasmtimeFuncCall :: Ptr WasmFuncT -> Ptr WasmValVecT -> Ptr WasmValVecT -> Ptr (Ptr WasmTrapT) -> IO (Ptr WasmtimeErrorT)
foreign import ccall "wasmtime_instance_new" wasmtimeInstanceNew :: Ptr WasmStoreT -> Ptr WasmModuleT -> Ptr WasmExternVecT -> Ptr (Ptr WasmInstanceT) -> Ptr (Ptr WasmTrapT) -> IO (Ptr WasmtimeErrorT)
foreign import ccall "wasmtime_module_new" wasmtimeModuleNew :: Ptr WasmEngineT -> Ptr WasmByteVecT -> Ptr (Ptr WasmModuleT) -> IO (Ptr WasmtimeErrorT)
foreign import ccall "wasmtime_wat2wasm" wasmtimeWat2Wasm :: Ptr WasmByteVecT -> Ptr WasmByteVecT -> IO (Ptr WasmtimeErrorT)
--wasmModuleValidate :: Ptr WasmStoreT -> Ptr WasmByteVecT -> IO Bool
