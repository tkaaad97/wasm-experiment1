{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Wasmtime.Raw
    ( WasmEngineT
    , WasmByteVecT(..)
    , WasmExternVecT(..)
    , WasmValT(..)
    , WasmValVecT(..)
    , WasmtimeErrorT
    , wasmByteVecDelete
    , wasmByteVecNewUninitialized
    , wasmEngineNew
    , wasmExternAsFunc
    , wasmExternVecDelete
    , wasmInstanceExports
    , wasmStoreNew
    , wasmTrapDelete
    , wasmTrapMessage
    , wasmtimeErrorDelete
    , wasmtimeErrorMessage
    , wasmtimeFuncCall
    , wasmtimeInstanceNew
    , wasmtimeModuleNew
    ) where

import Control.Exception (throwIO)
import Data.Word (Word32, Word64, Word8)
import Foreign (Ptr, Storable(..), castPtr, plusPtr)

data WasmEngineT

data WasmByteVecT = WasmByteVecT !Word32 !(Ptr Word8)

data WasmExternT

data WasmExternVecT = WasmExternVecT !Word32 !(Ptr (Ptr WasmExternT))

newtype WasmValkindT = WasmValkindT Word8
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

data WasmValVecT = WasmValVecT !Word32 !(Ptr WasmValT)

data WasmStoreT

data WasmModuleT

data WasmInstanceT

data WasmFuncT

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
            | k == wasmValkindI32 = do
                d <- peek $ castPtr ptr `plusPtr` 8
                return (WasmValI32 d)
            | k == wasmValkindI64 = do
                d <- peek $ castPtr ptr `plusPtr` 8
                return (WasmValI64 d)
            | k == wasmValkindF32 = do
                d <- peek $ castPtr ptr `plusPtr` 8
                return (WasmValF32 d)
            | k == wasmValkindF64 = do
                d <- peek $ castPtr ptr `plusPtr` 8
                return (WasmValF64 d)
            | k == wasmValkindAnyRef = do
                d <- peek $ castPtr ptr `plusPtr` 8
                return (WasmValAnyRef d)
            | k == wasmValkindFuncRef = do
                d <- peek $ castPtr ptr `plusPtr` 8
                return (WasmValFuncRef d)
            | otherwise = throwIO . userError $ "Unknown kind: " ++ show k

    poke ptr (WasmValI32 a) = do
        poke (castPtr ptr) wasmValkindI32
        poke (castPtr ptr `plusPtr` 8) a
    poke ptr (WasmValI64 a) = do
        poke (castPtr ptr) wasmValkindI64
        poke (castPtr ptr `plusPtr` 8) a
    poke ptr (WasmValF32 a) = do
        poke (castPtr ptr) wasmValkindF32
        poke (castPtr ptr `plusPtr` 8) a
    poke ptr (WasmValF64 a) = do
        poke (castPtr ptr) wasmValkindF64
        poke (castPtr ptr `plusPtr` 8) a
    poke ptr (WasmValAnyRef a) = do
        poke (castPtr ptr) wasmValkindAnyRef
        poke (castPtr ptr `plusPtr` 8) a
    poke ptr (WasmValFuncRef a) = do
        poke (castPtr ptr) wasmValkindFuncRef
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

wasmValkindI32 :: WasmValkindT
wasmValkindI32 = WasmValkindT 0

wasmValkindI64 :: WasmValkindT
wasmValkindI64 = WasmValkindT 1

wasmValkindF32 :: WasmValkindT
wasmValkindF32 = WasmValkindT 2

wasmValkindF64 :: WasmValkindT
wasmValkindF64 = WasmValkindT 3

wasmValkindAnyRef :: WasmValkindT
wasmValkindAnyRef = WasmValkindT 128

wasmValkindFuncRef :: WasmValkindT
wasmValkindFuncRef = WasmValkindT 129

foreign import ccall "wasm_byte_vec_delete" wasmByteVecDelete :: Ptr WasmByteVecT -> IO ()
foreign import ccall "wasm_byte_vec_new_uninitialized" wasmByteVecNewUninitialized :: Ptr WasmByteVecT -> Word32 -> IO ()
foreign import ccall "wasm_engine_new" wasmEngineNew :: IO (Ptr WasmEngineT)
foreign import ccall "wasm_extern_as_func" wasmExternAsFunc :: Ptr WasmExternT -> IO (Ptr WasmFuncT)
foreign import ccall "wasm_extern_vec_delete" wasmExternVecDelete :: Ptr WasmExternVecT -> IO ()
foreign import ccall "wasm_instance_delete" wasmInstanceDelete :: Ptr WasmInstanceT -> IO ()
foreign import ccall "wasm_instance_exports" wasmInstanceExports :: Ptr WasmInstanceT -> Ptr WasmExternVecT -> IO ()
foreign import ccall "wasm_module_delete" wasmModuleDelete :: Ptr WasmModuleT -> IO ()
foreign import ccall "wasm_store_delete" wasmStoreDelete :: Ptr WasmStoreT -> IO ()
foreign import ccall "wasm_store_new" wasmStoreNew :: Ptr WasmEngineT -> IO (Ptr WasmStoreT)
foreign import ccall "wasm_trap_delete" wasmTrapDelete :: Ptr WasmTrapT -> IO ()
foreign import ccall "wasm_trap_message" wasmTrapMessage :: Ptr WasmTrapT -> Ptr WasmByteVecT -> IO ()
foreign import ccall "wasmtime_error_delete" wasmtimeErrorDelete :: Ptr WasmtimeErrorT -> IO ()
foreign import ccall "wasmtime_error_message" wasmtimeErrorMessage :: Ptr WasmtimeErrorT -> Ptr WasmByteVecT -> IO ()
foreign import ccall "wasmtime_func_call" wasmtimeFuncCall :: Ptr WasmFuncT -> Ptr WasmValVecT -> Ptr WasmValVecT -> IO (Ptr WasmtimeErrorT)
foreign import ccall "wasmtime_instance_new" wasmtimeInstanceNew :: Ptr WasmStoreT -> Ptr WasmModuleT -> Ptr WasmExternVecT -> Ptr (Ptr WasmInstanceT) -> Ptr (Ptr WasmTrapT) -> IO (Ptr WasmtimeErrorT)
foreign import ccall "wasmtime_module_new" wasmtimeModuleNew :: Ptr WasmEngineT -> Ptr WasmByteVecT -> Ptr (Ptr WasmModuleT) -> IO (Ptr WasmtimeErrorT)
foreign import ccall "wasmtime_wat2wasm" wasmtimeWat2Wasm :: Ptr WasmByteVecT -> Ptr WasmByteVecT -> IO (Ptr WasmtimeErrorT)
--wasmModuleValidate :: Ptr WasmStoreT -> Ptr WasmByteVecT -> IO Bool
