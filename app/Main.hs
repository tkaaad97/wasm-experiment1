module Main where

import Control.Exception (throwIO)
import Control.Monad (unless)
import qualified Data.ByteString as ByteString (length, readFile)
import qualified Data.ByteString.Unsafe as ByteString (unsafeUseAsCString)
import qualified Foreign
import qualified Foreign.C.String as Foreign (peekCStringLen)
import Wasmtime.Raw

printWasmtimeError :: Foreign.Ptr WasmtimeErrorT -> IO ()
printWasmtimeError err
    | err /= Foreign.nullPtr =
        Foreign.alloca $ \p -> do
            wasmtimeErrorMessage err p
            WasmByteVecT len mp <- Foreign.peek p
            str <- Foreign.peekCStringLen (Foreign.castPtr mp, fromIntegral len)
            putStrLn str
            wasmByteVecDelete p
    | otherwise = return ()

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
    printWasmtimeError err

    putStrLn "module new"

    externVec <- Foreign.malloc
    Foreign.poke externVec (WasmExternVecT 0 Foreign.nullPtr)

    (err2, instance_) <-
        Foreign.alloca $ \pp ->
        Foreign.alloca $ \trap -> do
            err2 <- wasmtimeInstanceNew store module_ externVec pp trap
            instance_ <- Foreign.peek pp
            return (err2, instance_)
    putStrLn "instance new"
    printWasmtimeError err2

    func <- Foreign.alloca $ \exportVec -> do
        wasmInstanceExports instance_ exportVec
        WasmExternVecT _ exports <- Foreign.peek exportVec
        wasmExternAsFunc =<< Foreign.peek exports

    putStrLn "export func"

    (err3, result) <-
        Foreign.withArray [WasmValI32 1, WasmValI32 2] $ \paramVecPtr ->
        Foreign.with (WasmValVecT 2 paramVecPtr) $ \params ->
        Foreign.allocaArray 1 $ \resultVecPtr ->
        Foreign.with (WasmValVecT 1 resultVecPtr) $ \results -> do
            err3 <- wasmtimeFuncCall func params results
            result <- Foreign.peek resultVecPtr
            return (err3, result)
    printWasmtimeError err3
    putStrLn $ "result: " ++ show result
