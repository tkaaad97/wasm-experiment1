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
        (Vector.singleton (FuncType (ResultType $ Vector.fromList [NumType I32, NumType I32]) (ResultType $ Vector.fromList [NumType I32])))
        (Vector.singleton (Func 0 mempty $ Vector.fromList [LocalGet 0, LocalGet 1, I32Binary IAdd]))
        (Vector.singleton (Export "add" (ExportFunc 0)))
      ),
      ( "module-has-block"
      , Module
        (Vector.singleton (FuncType (ResultType mempty) (ResultType . Vector.singleton $ NumType I32)))
        (Vector.singleton (Func 0 mempty $ Vector.fromList [Block (BlockTypeFuncType (FuncType (ResultType mempty) (ResultType . Vector.singleton $ NumType I32))) (Vector.fromList [I32Const 0, Block (BlockTypeFuncType (FuncType (ResultType mempty) (ResultType . Vector.singleton $ NumType I32))) (Vector.fromList [I32Const 0, I32Const 1, I32Binary IAdd]), I32Binary IAdd])]))
        (Vector.singleton (Export "blockfunc" (ExportFunc 0)))
      ),
      ( "module-has-if"
      , Module
        (Vector.singleton (FuncType (ResultType . Vector.fromList $ [NumType I32, NumType I32]) (ResultType . Vector.singleton $ NumType I32)))
        (Vector.singleton (Func 0 mempty $ Vector.fromList [LocalGet 0, LocalGet 1, I32Relation IEq, If (BlockTypeFuncType (FuncType (ResultType mempty) (ResultType . Vector.singleton $ NumType I32))) (Vector.singleton (I32Const 0)) (Vector.singleton (I32Const 100))]))
        (Vector.singleton (Export "iffunc" (ExportFunc 0)))
      ),
      ( "module-loop-br"
      , Module
        (Vector.singleton (FuncType (ResultType . Vector.singleton $ NumType I32) (ResultType . Vector.singleton $ NumType I32)))
        (Vector.singleton (Func 0 (Vector.fromList [NumType I32, NumType I32]) . Vector.fromList $
            [ Block BlockTypeEmpty . Vector.fromList $
                [ Loop BlockTypeEmpty . Vector.fromList $
                    [ LocalGet 0
                    , LocalGet 1
                    , I32Relation LeS
                    , BrIf 0
                    , LocalGet 2
                    , LocalGet 1
                    , I32Binary IAdd
                    , LocalSet 2
                    , LocalGet 1
                    , I32Const 1
                    , LocalSet 1
                    , Br 1
                    ]
                ]
            , LocalGet 2
            , Return
            ])
        )
        (Vector.singleton (Export "sum" (ExportFunc 0)))
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
