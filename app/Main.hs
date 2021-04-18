module Main where

import Control.Exception (throwIO)
import Control.Monad (unless)
import qualified Data.ByteString as ByteString (length, readFile)
import qualified Data.ByteString.Unsafe as ByteString (unsafeUseAsCString)
import qualified Foreign
import Wasmtime.Raw

main :: IO ()
main = do
    engine <- wasmEngineNew
    unless (engine /= Foreign.nullPtr) $
        throwIO . userError $ "wasmEngineNew failed"

    putStrLn "engine new"

    store <- wasmStoreNew engine
    unless (store /= Foreign.nullPtr) $
        throwIO . userError $ "wasmStoreNew failed"

    putStrLn "store new"

    source <- ByteString.readFile "add.wasm"

    putStrLn "read source"

    byteVec <- Foreign.malloc
    wasmByteVecNewUninitialized byteVec (fromIntegral (ByteString.length source))

    ByteString.unsafeUseAsCString source $ \sp -> do
        WasmByteVecT len dp <- Foreign.peek byteVec
        Foreign.copyBytes (Foreign.castPtr dp) sp (fromIntegral len)

    (err, module_) <- Foreign.alloca $ \pp -> do
        err <- wasmtimeModuleNew engine byteVec pp
        module_ <- Foreign.peek pp
        return (err, module_)

    putStrLn "module new"

    externVec <- Foreign.malloc
    Foreign.poke externVec (WasmExternVecT 0 Foreign.nullPtr)

    (err2, module_) <-
        Foreign.alloca $ \pp ->
        Foreign.alloca $ \trap -> do
            err2 <- wasmtimeInstanceNew store module_ externVec pp trap
            instance_ <- Foreign.peek pp
            return (err2, instance_)
    putStrLn "instance new"
    return ()
