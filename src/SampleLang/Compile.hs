module SampleLang.Compile
    ( compileWat
    ) where

import Control.Exception (throwIO)
import qualified Data.ByteString as ByteString (readFile, writeFile)
import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)
import SampleLang.CodeGen.Wasm (gen)
import SampleLang.Parser (parseProgram)
import SampleLang.Resolve (resolve)
import Wasm.TextPrinter (printText)


compileWat :: FilePath -> FilePath -> IO ()
compileWat inputPath outputPath = do
    source <- Text.decodeUtf8 <$> ByteString.readFile inputPath

    parsed <- either (throwIO . userError) return $ parseProgram source
    resolved <- either (throwIO . userError) return $ resolve parsed
    wasm <- either (throwIO . userError) return $ gen resolved
    let code = printText wasm
    ByteString.writeFile outputPath (Text.encodeUtf8 code)
