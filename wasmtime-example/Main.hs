module Main where

import qualified Data.ByteString as ByteString (readFile)
import Data.Word (Word32, Word8)
import Foreign (Ptr)
import qualified Foreign
import Wasmtime
import Wasmtime.Raw as Wasmtime

foreign import ccall "printf" printfI32I32 :: Ptr () -> Word32 -> Word32 -> IO ()

callback :: Ptr (Ptr Word8) -> Ptr WasmValVecT -> Ptr WasmValVecT -> IO (Ptr WasmTrapT)
callback memPtr paramVecPtr _ = do
    WasmValVecT _ p <- Foreign.peek paramVecPtr
    WasmValI32 off <- Foreign.peek p
    WasmValI32 a <- Foreign.peekElemOff p 1
    WasmValI32 b <- Foreign.peekElemOff p 2
    mem <- Foreign.peek memPtr
    printfI32I32 (mem `Foreign.plusPtr` fromIntegral off) a b

    return Foreign.nullPtr

printWasmtimeError :: Foreign.Ptr WasmtimeErrorT -> IO ()
printWasmtimeError = (putStrLn =<<) . getWasmtimeErrorMessage

main :: IO ()
main =
    withWasmEngine $ \engine ->
    withWasmStore engine $ \store -> do
        source <- wat2Wasm =<< ByteString.readFile "example.wat"
        withWasmtimeModule engine source printWasmtimeError $ \module_ ->
            Foreign.alloca $ \memPtr ->
            withWasmtimeInstance store module_ [(FuncType [wasmValKindI32, wasmValKindI32, wasmValKindI32] [], callback memPtr)] printWasmtimeError $ \instance_ ->
                withWasmInstanceExports instance_ $ \exports -> do
                    mem <- getExportMemory exports 0
                    memData <- wasmMemoryData mem
                    Foreign.poke memPtr memData
                    func <- getExportFunc exports 1
                    withWasmtimeFuncCall func [WasmValI32 1, WasmValI32 2] printWasmtimeError (const $ putStrLn "trap") $ \results ->
                        putStrLn $ "results: " ++ show results

    where
    getExportFunc xs idx
        | length xs > idx = wasmExternAsFunc (xs !! idx)
        | otherwise = error "export index out of bounds"
    getExportMemory xs idx
        | length xs > idx = wasmExternAsMemory (xs !! idx)
        | otherwise = error "export index out of bounds"
