module Main where

import Lib
import Wasmtime.Raw (wasmEngineNew)

main :: IO ()
main = do
    engine <- wasmEngineNew
    return ()
