module Wasmtime
    ( FuncType(..)
    , WasmByteT
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
    , WasmTrapT
    , WasmValKindT
    , WasmValT
    , WasmValTypeT
    , WasmValTypeVecT(..)
    , WasmValVecT(..)
    , WasmtimeErrorT
    , newCallbackFunPtr
    , deleteCallbackFunPtr
    , newWasmFuncType
    , withWasmEngine
    , withWasmStore
    , withWasmtimeModule
    , withWasmtimeInstance
    , withWasmInstanceExport
    , withWasmtimeFuncCall
    , wasmValVecToList
    ) where

import Control.Exception (bracket)
import Control.Monad (unless, when, zipWithM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (length)
import qualified Data.ByteString.Unsafe as ByteString (unsafeUseAsCString)
import Foreign (FunPtr, Ptr)
import qualified Foreign
import Wasmtime.Raw (WasmByteT, WasmByteVecT(..), WasmEngineT, WasmExternT,
                     WasmExternVecT(..), WasmFuncCallbackT, WasmFuncT,
                     WasmFuncTypeT, WasmInstanceT, WasmModuleT, WasmRefT,
                     WasmStoreT, WasmTrapT, WasmValKindT, WasmValT,
                     WasmValTypeT, WasmValTypeVecT(..), WasmValVecT(..),
                     WasmtimeErrorT)
import qualified Wasmtime.Raw as Raw

data FuncType = FuncType ![WasmValKindT] ![WasmValKindT]
    deriving (Show, Eq)

foreign import ccall "wrapper" newCallbackFunPtr :: (Ptr WasmValVecT -> Ptr WasmValVecT -> IO (Ptr WasmTrapT)) -> IO (FunPtr (Ptr WasmValVecT -> Ptr WasmValVecT -> IO (Ptr WasmTrapT)))

deleteCallbackFunPtr :: FunPtr (Ptr WasmValVecT -> Ptr WasmValVecT -> IO (Ptr WasmTrapT)) -> IO ()
deleteCallbackFunPtr = Foreign.freeHaskellFunPtr

newWasmFuncType :: FuncType -> IO (Ptr WasmFuncTypeT)
newWasmFuncType (FuncType paramKinds resultKinds) =
    withWasmValTypeVec paramKinds $ \paramVecPtr ->
    withWasmValTypeVec resultKinds $ \resultVecPtr ->
        Raw.wasmFuncTypeNew paramVecPtr resultVecPtr

withWasmValTypeVec :: [WasmValKindT] -> (Ptr WasmValTypeVecT -> IO a) -> IO a
withWasmValTypeVec kinds f = do
    valTypes <- mapM Raw.wasmValTypeNew kinds
    Foreign.withArray valTypes $ \valTypePtr ->
        Foreign.alloca $ \valTypeVecPtr -> do
            Raw.wasmValTypeVecNew valTypeVecPtr (fromIntegral (length kinds)) valTypePtr
            r <- f valTypeVecPtr
            mapM_ Raw.wasmValTypeDelete valTypes
            Raw.wasmValTypeVecDelete valTypeVecPtr
            return r

withWasmEngine :: (Ptr WasmEngineT -> IO a) -> IO a
withWasmEngine = bracket Raw.wasmEngineNew Raw.wasmEngineDelete

withWasmStore :: Ptr WasmEngineT -> (Ptr WasmStoreT -> IO a) -> IO a
withWasmStore engine = bracket (Raw.wasmStoreNew engine) Raw.wasmStoreDelete

withWasmtimeModule :: Ptr WasmEngineT -> ByteString -> (Ptr WasmtimeErrorT -> IO a) -> (Ptr WasmModuleT -> IO a) -> IO a
withWasmtimeModule engine source errorHandler handler = bracket before after mid
    where
    before =
        Foreign.alloca $ \byteVec ->
        ByteString.unsafeUseAsCString source $ \sp ->
        Foreign.alloca $ \pp -> do
            Raw.wasmByteVecNew byteVec (fromIntegral (ByteString.length source)) (Foreign.castPtr sp)
            err <- Raw.wasmtimeModuleNew engine byteVec pp
            Raw.wasmByteVecDelete byteVec
            module_ <- Foreign.peek pp
            if err == Foreign.nullPtr
                then return (Right module_)
                else return (Left err)

    after (Right module_)
        | module_ /= Foreign.nullPtr = Raw.wasmModuleDelete module_
        | otherwise = return ()

    after (Left err)
        | err /= Foreign.nullPtr = Raw.wasmtimeErrorDelete err
        | otherwise = return ()

    mid (Right module_) = handler module_
    mid (Left err)      = errorHandler err

withWasmtimeInstance :: Ptr WasmStoreT -> Ptr WasmModuleT -> [(FuncType, Ptr WasmValVecT -> Ptr WasmValVecT -> IO (Ptr WasmTrapT))] -> (Ptr WasmtimeErrorT -> IO a) -> (Ptr WasmInstanceT -> IO a) -> IO a
withWasmtimeInstance store module_ imports errorHandler handler = bracket before after mid
    where
    before = do
        let funcTypes = map fst imports
        wasmFuncTypes <- mapM newWasmFuncType funcTypes
        funPtrs <- mapM (newCallbackFunPtr . snd) imports
        funcs <- zipWithM (Raw.wasmFuncNew store) wasmFuncTypes funPtrs
        externs <- mapM Raw.wasmFuncAsExtern funcs
        mapM_ Raw.wasmFuncTypeDelete wasmFuncTypes

        Foreign.alloca $ \importVec ->
            Foreign.alloca $ \pp ->
            Foreign.alloca $ \trap -> do
                Foreign.withArray externs $ Raw.wasmExternVecNew importVec (fromIntegral (length imports))
                err <- Raw.wasmtimeInstanceNew store module_ importVec pp trap
                Raw.wasmExternVecDelete importVec
                mapM_ Raw.wasmFuncDelete funcs
                instance_ <- Foreign.peek pp
                if err == Foreign.nullPtr
                    then return (Right (instance_, funPtrs))
                    else return (Left (err, funPtrs))

    after (Right (instance_, funPtrs)) = do
        unless (instance_ == Foreign.nullPtr) $ Raw.wasmInstanceDelete instance_
        mapM_ deleteCallbackFunPtr funPtrs

    after (Left (err, funPtrs)) = do
        unless (err == Foreign.nullPtr) $ Raw.wasmtimeErrorDelete err
        mapM_ deleteCallbackFunPtr funPtrs

    mid (Right (instance_, _)) = handler instance_
    mid (Left (err, _))        = errorHandler err

withWasmInstanceExport :: Ptr WasmInstanceT -> Int -> (Maybe (Ptr WasmFuncT) -> IO a) -> IO a
withWasmInstanceExport instance_ idx handler =
    Foreign.alloca $ \exportVec -> do
        Raw.wasmInstanceExports instance_ exportVec
        WasmExternVecT size exports <- Foreign.peek exportVec
        maybeExport <- if idx >= 0 && idx < fromIntegral size
            then fmap Just . Raw.wasmExternAsFunc =<< Foreign.peekElemOff exports idx
            else return Nothing
        r <- handler maybeExport
        Raw.wasmExternVecDelete exportVec
        return r

withWasmtimeFuncCall :: Ptr WasmFuncT -> [WasmValT] -> (Ptr WasmtimeErrorT -> IO a) -> (Ptr WasmTrapT -> IO a) -> ([WasmValT] -> IO a) -> IO a
withWasmtimeFuncCall func params errHandler trapHandler handler = do
    resultArity <- Raw.wasmFuncResultArity func
    bracket (before resultArity) after mid
    where
    before resultArity =
        Foreign.alloca $ \paramVecPtr ->
        Foreign.withArray params $ \paramPtr ->
        Foreign.alloca $ \resultVecPtr ->
        Foreign.alloca $ \trapPtr -> do
            Raw.wasmValVecNew paramVecPtr (fromIntegral (length params)) paramPtr
            Raw.wasmValVecNewUninitialized resultVecPtr (fromIntegral resultArity)
            err <- Raw.wasmtimeFuncCall func paramVecPtr resultVecPtr trapPtr
            trap <- Foreign.peek trapPtr
            results <- if err == Foreign.nullPtr && trap == Foreign.nullPtr
                then wasmValVecToList resultVecPtr
                else return []
            Raw.wasmValVecDelete paramVecPtr
            Raw.wasmValVecDelete resultVecPtr
            return (err, trap, results)

    after (err, trap, _) = do
        when (err /= Foreign.nullPtr) $ Raw.wasmtimeErrorDelete err
        when (trap /= Foreign.nullPtr) $ Raw.wasmTrapDelete trap

    mid (err, trap, results)
        | err /= Foreign.nullPtr = errHandler err
        | trap /= Foreign.nullPtr = trapHandler trap
        | otherwise = handler results

wasmValVecToList :: Ptr WasmValVecT -> IO [WasmValT]
wasmValVecToList vecPtr = do
    WasmValVecT size p <- Foreign.peek vecPtr
    Foreign.peekArray (fromIntegral size) p
