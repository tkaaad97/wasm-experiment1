module Main where

import Data.Bits ((.&.))
import qualified Data.ByteString as ByteString (readFile)
import Data.Word (Word32, Word8)
import Foreign (Ptr)
import qualified Foreign
import SampleLang.Compile (compileWat)
import System.Process (callCommand)
import Wasmtime
import Wasmtime.Raw as Wasmtime

foreign import ccall "printf" printf :: Ptr Word8 -> IO ()
foreign import ccall "printf" printfI :: Ptr Word8 -> Word32 -> IO ()
foreign import ccall "printf" printfSI :: Ptr Word8 -> Ptr Word8 -> Word32 -> IO ()
foreign import ccall "printf" printfSII :: Ptr Word8 -> Ptr Word8 -> Word32 -> Word32 -> IO ()
foreign import ccall "printf" printfSS :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall "printf" printfSSS :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()

printf' :: Ptr (Ptr Word8) -> Ptr WasmValVecT -> Ptr WasmValVecT -> IO (Ptr WasmTrapT)
printf' memPtr paramVecPtr _ = do
    WasmValVecT _ p <- Foreign.peek paramVecPtr
    WasmValI64 offLen <- Foreign.peek p
    let off = offLen .&. 0xFFFF
    mem <- Foreign.peek memPtr
    printf (mem `Foreign.plusPtr` fromIntegral off)
    return Foreign.nullPtr

printfI' :: Ptr (Ptr Word8) -> Ptr WasmValVecT -> Ptr WasmValVecT -> IO (Ptr WasmTrapT)
printfI' memPtr paramVecPtr _ = do
    WasmValVecT _ p <- Foreign.peek paramVecPtr
    WasmValI64 offLen <- Foreign.peek p
    WasmValI32 a  <- Foreign.peekElemOff p 1
    let off = offLen .&. 0xFFFF
    mem <- Foreign.peek memPtr
    printfI (mem `Foreign.plusPtr` fromIntegral off) a
    return Foreign.nullPtr

printfSI' :: Ptr (Ptr Word8) -> Ptr WasmValVecT -> Ptr WasmValVecT -> IO (Ptr WasmTrapT)
printfSI' memPtr paramVecPtr _ = do
    WasmValVecT _ p <- Foreign.peek paramVecPtr
    WasmValI64 offLen <- Foreign.peek p
    WasmValI64 offLen2 <- Foreign.peekElemOff p 1
    WasmValI32 a  <- Foreign.peekElemOff p 2
    let off = offLen .&. 0xFFFF
        off2 = offLen2 .&. 0xFFFF
    mem <- Foreign.peek memPtr
    printfSI (mem `Foreign.plusPtr` fromIntegral off) (mem `Foreign.plusPtr` fromIntegral off2) a
    return Foreign.nullPtr

printfSII' :: Ptr (Ptr Word8) -> Ptr WasmValVecT -> Ptr WasmValVecT -> IO (Ptr WasmTrapT)
printfSII' memPtr paramVecPtr _ = do
    WasmValVecT _ p <- Foreign.peek paramVecPtr
    WasmValI64 offLen <- Foreign.peek p
    WasmValI64 offLen2 <- Foreign.peekElemOff p 1
    WasmValI32 a  <- Foreign.peekElemOff p 2
    WasmValI32 b  <- Foreign.peekElemOff p 3
    let off = offLen .&. 0xFFFF
        off2 = offLen2 .&. 0xFFFF
    mem <- Foreign.peek memPtr
    printfSII (mem `Foreign.plusPtr` fromIntegral off) (mem `Foreign.plusPtr` fromIntegral off2) a b
    return Foreign.nullPtr

printfSS' :: Ptr (Ptr Word8) -> Ptr WasmValVecT -> Ptr WasmValVecT -> IO (Ptr WasmTrapT)
printfSS' memPtr paramVecPtr _ = do
    WasmValVecT _ p <- Foreign.peek paramVecPtr
    WasmValI64 offLen <- Foreign.peek p
    WasmValI64 offLena <- Foreign.peekElemOff p 1
    WasmValI64 offLenb  <- Foreign.peekElemOff p 2
    let off = offLen .&. 0xFFFF
        offa = offLena .&. 0xFFFF
        offb = offLenb .&. 0xFFFF
    mem <- Foreign.peek memPtr
    printfSS (mem `Foreign.plusPtr` fromIntegral off) (mem `Foreign.plusPtr` fromIntegral offa) (mem `Foreign.plusPtr` fromIntegral offb)
    return Foreign.nullPtr

printfSSS' :: Ptr (Ptr Word8) -> Ptr WasmValVecT -> Ptr WasmValVecT -> IO (Ptr WasmTrapT)
printfSSS' memPtr paramVecPtr _ = do
    WasmValVecT _ p <- Foreign.peek paramVecPtr
    WasmValI64 offLen <- Foreign.peek p
    WasmValI64 offLena <- Foreign.peekElemOff p 1
    WasmValI64 offLenb  <- Foreign.peekElemOff p 2
    WasmValI64 offLenc  <- Foreign.peekElemOff p 3
    let off = offLen .&. 0xFFFF
        offa = offLena .&. 0xFFFF
        offb = offLenb .&. 0xFFFF
        offc = offLenc .&. 0xFFFF
    mem <- Foreign.peek memPtr
    printfSSS (mem `Foreign.plusPtr` fromIntegral off) (mem `Foreign.plusPtr` fromIntegral offa) (mem `Foreign.plusPtr` fromIntegral offb) (mem `Foreign.plusPtr` fromIntegral offc)
    return Foreign.nullPtr

printWasmTrap :: Foreign.Ptr WasmTrapT -> IO ()
printWasmTrap = (putStrLn =<<) . getWasmTrapMessage

printWasmtimeError :: Foreign.Ptr WasmtimeErrorT -> IO ()
printWasmtimeError = (putStrLn =<<) . getWasmtimeErrorMessage

main :: IO ()
main =
    withWasmEngine $ \engine ->
    withWasmStore engine $ \store -> do
        callCommand "gcc -E -P -o native-test/code/test1.i native-test/code/test1.c"
        compileWat "native-test/code/test1.i" "native-test/code/test1.wat"
        wasmSource <- wat2Wasm =<< ByteString.readFile "native-test/code/test1.wat"
        withWasmtimeModule engine wasmSource printWasmtimeError $ \module_ ->
            Foreign.alloca $ \memPtr ->
            withWasmtimeInstance store module_ (callbacks memPtr) printWasmtimeError $ \instance_ ->
                withWasmInstanceExports instance_ $ \exports -> do
                    mem <- getExportMemory exports 0
                    memData <- wasmMemoryData mem
                    Foreign.poke memPtr memData
                    func <- getExportFunc exports 1
                    withWasmtimeFuncCall func [] printWasmtimeError printWasmTrap $ \results ->
                        putStrLn $ "results: " ++ show results
    where
    getExportFunc xs idx
        | length xs > idx = wasmExternAsFunc (xs !! idx)
        | otherwise = error "export index out of bounds"
    getExportMemory xs idx
        | length xs > idx = wasmExternAsMemory (xs !! idx)
        | otherwise = error "export index out of bounds"

    callbacks memPtr =
        [ (FuncType [wasmValKindI64] [], printf' memPtr)
        , (FuncType [wasmValKindI64, wasmValKindI32] [], printfI' memPtr)
        , (FuncType [wasmValKindI64, wasmValKindI64, wasmValKindI32] [], printfSI' memPtr)
        , (FuncType [wasmValKindI64, wasmValKindI64, wasmValKindI32, wasmValKindI32] [], printfSII' memPtr)
        , (FuncType [wasmValKindI64, wasmValKindI64, wasmValKindI64] [], printfSS' memPtr)
        , (FuncType [wasmValKindI64, wasmValKindI64, wasmValKindI64, wasmValKindI64] [], printfSSS' memPtr)
        ]
