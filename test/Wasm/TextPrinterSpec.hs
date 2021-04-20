{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Wasm.TextPrinterSpec
    ( spec
    ) where

import qualified Data.ByteString as ByteString (readFile, writeFile)
import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)
import qualified Data.Vector as Vector
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (<.>), (</>))
import Test.Hspec
import Wasm.TextPrinter
import Wasm.Types

modules :: [(String, Module)]
modules =
    [ ( "empty-module"
      , Module mempty mempty mempty
      ),
      ( "module1"
      , Module
        (Vector.singleton (FuncType (ResultType [NumType I32, NumType I32]) (ResultType [NumType I32])))
        (Vector.singleton (Func 0 [] [LocalGet 0, LocalGet 1, I32Binary IAdd]))
        (Vector.singleton (Export "add" (ExportFunc 0)))
      )
    ]

spec :: Spec
spec =
    describe "printText" $ mapM_ (uncurry $ printTextSpec basePath) modules
    where
    basePath = takeDirectory (takeDirectory __FILE__) </> "snapshot"

printTextSpec :: FilePath -> String -> Module -> SpecWith ()
printTextSpec basePath name m =
    it name $ do
        snapshotExists <- doesFileExist path
        if snapshotExists
            then do
                snapshot <- fmap Text.decodeUtf8 . ByteString.readFile $ path
                printText m `shouldBe` snapshot
            else do
                createDirectoryIfMissing True (takeDirectory path)
                ByteString.writeFile path . Text.encodeUtf8 . printText $ m
                fail ("snapshot does not exist. new file created. file: " ++ path)
        where
        path = basePath </> "text" </> name <.> "wat"
