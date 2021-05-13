module Main where

import Control.Monad (unless)
import qualified Data.ByteString as ByteString (readFile)
import Data.Word (Word32)
import Foreign (Ptr)
import qualified Foreign
import qualified Foreign.C.String as Foreign (newCString, peekCStringLen)
import Foreign.C.Types (CChar)
import Wasmtime
import Wasmtime.Raw as Wasmtime

foreign import ccall "printf" printfI32I32 :: Ptr () -> Word32 -> Word32 -> IO ()

callback :: Ptr CChar -> Ptr WasmValVecT -> Ptr WasmValVecT -> IO (Ptr WasmTrapT)
callback mem paramVecPtr _ = do
    WasmValVecT _ p <- Foreign.peek paramVecPtr
    WasmValI32 off <- Foreign.peek p
    WasmValI32 a <- Foreign.peekElemOff p 1
    WasmValI32 b <- Foreign.peekElemOff p 2
    printfI32I32 (mem `Foreign.plusPtr` fromIntegral off) a b
    return Foreign.nullPtr

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
main =
    withWasmEngine $ \engine ->
    withWasmStore engine $ \store -> do
        source <- ByteString.readFile "add.wasm"
        withWasmtimeModule engine source printWasmtimeError $ \module_ -> do
            mem <- Foreign.newCString "0123456789\0first: %d, second: %d\n\0aaaa"
            withWasmtimeInstance store module_ [(FuncType [wasmValKindI32, wasmValKindI32, wasmValKindI32] [], callback mem)] printWasmtimeError $ \instance_ -> do

                func <- Foreign.alloca $ \exportVec -> do
                    wasmInstanceExports instance_ exportVec
                    WasmExternVecT _ exports <- Foreign.peek exportVec
                    wasmExternAsFunc =<< Foreign.peek exports

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
