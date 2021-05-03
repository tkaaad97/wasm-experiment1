module Main where

import Control.Exception (throwIO)
import SampleLang.Compile (compileWat)
import System.Environment (getArgs)
import System.FilePath ((-<.>))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputPath] -> compileWat inputPath (inputPath -<.> "wat")
        _           -> throwIO . userError $ "wrong argument"
