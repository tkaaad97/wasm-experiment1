module SampleLang.CodeGen.Wasm
    ( genExpr
    , genStatement
    , genFunction
    , gen
    ) where

import Data.Bits ((.|.))
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (drop, foldM', fromList, imap, map, mapM,
                                        singleton)
import qualified SampleLang.Ast.Resolved as R
import SampleLang.Ast.Types
import qualified SampleLang.Resolve as R (getExprType)
import VectorBuilder.Builder (Builder)
import qualified VectorBuilder.Builder as Builder (empty, foldable, singleton)
import qualified VectorBuilder.Vector as Builder (build)
import qualified Wasm.Types as Wasm

data WasmFunc = WasmFunc !Text !Wasm.FuncType !(Vector Wasm.ValType) !(Vector Wasm.Instr)

gen :: R.Program -> Either String Wasm.Module
gen (R.Program funcs strs _) = do
    wasmFuncVec <- Vector.mapM genFunction funcs
    let typeVec = Vector.map (\(WasmFunc _ type_ _ _) -> type_) wasmFuncVec
        funcVec = Vector.imap (\i (WasmFunc _ _ locals instrVec) -> Wasm.Func (fromIntegral i) locals instrVec) wasmFuncVec
        exportVec = Vector.imap (\i (WasmFunc name _ _ _) -> Wasm.Export name (Wasm.ExportFunc (fromIntegral i))) wasmFuncVec
        dataVec = Vector.map (\(s, offLen) -> Wasm.DataSegment s . Just . fromIntegral $ offLen .|. 0xFFFF) strs
        wasm = Wasm.Module
            { Wasm.moduleFuncs = funcVec
            , Wasm.moduleTypes = typeVec
            , Wasm.moduleExports = exportVec
            , Wasm.moduleDatas = dataVec
            }
    return wasm

genExpr :: R.Expr -> Either String (Builder Wasm.Instr)
genExpr (R.ExprUnary _ op e)            = genUnOp op (R.getExprType e) e
genExpr (R.ExprBinary _ op l r)         = genBinOp op (R.getExprType l) l r
genExpr (R.ExprAssign _ l r)            = genAssign l r
genExpr (R.ExprConstant _ a)            = return (genConstant a)
genExpr (R.ExprReference _ a)           = return (genReference a)
genExpr (R.ExprFunctionCall _ idx args) = genFunctionCall idx args
genExpr (R.ExprStringLiteral offLen)    = return (Builder.singleton (Wasm.I64Const offLen))

genDeclOrExpr :: Either R.Declaration R.Expr -> Either String (Builder Wasm.Instr)
genDeclOrExpr (Left (R.Declaration _ _ Nothing)) = return Builder.empty
genDeclOrExpr (Left (R.Declaration _ lvalue (Just initializer))) = genAssign lvalue initializer
genDeclOrExpr (Right e)                            = genExpr e

genUnOp :: UnOp -> Type' -> R.Expr -> Either String (Builder Wasm.Instr)
genUnOp Negate TypeInt e = do
    a <- genExpr e
    return $ Builder.singleton (Wasm.I32Const 0) <> a <> Builder.singleton (Wasm.I32Binary Wasm.ISub)
genUnOp Negate TypeDouble e = do
    a <- genExpr e
    return $ a <> Builder.singleton (Wasm.F64Unary Wasm.Neg)
genUnOp Negate type_ e = Left $ "codegen error. Negate " ++ show type_ ++ " " ++ show e
genUnOp Not TypeBool e = do
    a <- genExpr e
    return $ a <> Builder.singleton (Wasm.I32Const 0) <> Builder.singleton (Wasm.I32Relation Wasm.IEq)
genUnOp Not type_ e = Left $ "codegen error. Not " ++ show type_ ++ " " ++ show e
genUnOp Increment TypeInt (R.ExprReference _ (ReferenceLocal (LocalVarIdx idx) _)) =
    return $ Builder.foldable [Wasm.LocalGet (fromIntegral idx), Wasm.I32Const 1, Wasm.I32Binary Wasm.IAdd, Wasm.LocalTee (fromIntegral idx)]
genUnOp Increment TypeInt (R.ExprReference _ (ReferenceGlobal (GlobalVarIdx idx) _)) =
    return $ Builder.foldable [Wasm.GlobalGet (fromIntegral idx), Wasm.I32Const 1, Wasm.I32Binary Wasm.IAdd, Wasm.GlobalSet (fromIntegral idx), Wasm.GlobalGet (fromIntegral idx)]
genUnOp Increment type_ e = Left $ "codegen error. Increment " ++ show type_ ++ " " ++ show e
genUnOp Decrement TypeInt (R.ExprReference _ (ReferenceLocal (LocalVarIdx idx) _)) =
    return $ Builder.foldable [Wasm.LocalGet (fromIntegral idx), Wasm.I32Const 1, Wasm.I32Binary Wasm.ISub, Wasm.LocalTee (fromIntegral idx)]
genUnOp Decrement TypeInt (R.ExprReference _ (ReferenceGlobal (GlobalVarIdx idx) _)) =
    return $ Builder.foldable [Wasm.GlobalGet (fromIntegral idx), Wasm.I32Const 1, Wasm.I32Binary Wasm.ISub, Wasm.GlobalSet (fromIntegral idx), Wasm.GlobalGet (fromIntegral idx)]
genUnOp Decrement type_ e =  Left $ "codegen error. Decrement" ++ show type_ ++ " " ++ show e

genBinOp :: BinOp -> Type' -> R.Expr -> R.Expr -> Either String (Builder Wasm.Instr)
genBinOp Add TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Binary Wasm.IAdd)
genBinOp Add TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Binary Wasm.Add)
genBinOp Add type_ l r = Left $ "codegen error. Add " ++ show type_ ++ " " ++ show l ++ " " ++ show r
genBinOp Sub TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Binary Wasm.ISub)
genBinOp Sub TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Binary Wasm.Sub)
genBinOp Sub type_ l r = Left $ "codegen error. Sub " ++ show type_ ++ " " ++ show l ++ " " ++ show r
genBinOp Mul TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Binary Wasm.IMul)
genBinOp Mul TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Binary Wasm.Mul)
genBinOp Mul type_ l r = Left $ "codegen error. Mul " ++ show type_ ++ " " ++ show l ++ " " ++ show r
genBinOp Div TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Binary Wasm.DivS)
genBinOp Div TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Binary Wasm.Div)
genBinOp Div type_ l r = Left $ "codegen error. Div " ++ show type_ ++ " " ++ show l ++ " " ++ show r
genBinOp Equ TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Relation Wasm.IEq)
genBinOp Equ TypeBool l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Relation Wasm.IEq)
genBinOp Equ TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Relation Wasm.FEq)
genBinOp Equ type_ l r = Left $ "codegen error. Equ " ++ show type_ ++ " " ++ show l ++ " " ++ show r
genBinOp Neq TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Relation Wasm.INe)
genBinOp Neq TypeBool l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Relation Wasm.INe)
genBinOp Neq TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Relation Wasm.FNe)
genBinOp Neq type_ l r = Left $ "codegen error. Neq " ++ show type_ ++ " " ++ show l ++ " " ++ show r
genBinOp Lt TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Relation Wasm.LtS)
genBinOp Lt TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Relation Wasm.Lt)
genBinOp Lt type_ l r = Left $ "codegen error. Lt " ++ show type_ ++ " " ++ show l ++ " " ++ show r
genBinOp Le TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Relation Wasm.LeS)
genBinOp Le TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Relation Wasm.Le)
genBinOp Le type_ l r = Left $ "codegen error. Le " ++ show type_ ++ " " ++ show l ++ " " ++ show r
genBinOp Gt TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Relation Wasm.GtS)
genBinOp Gt TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Relation Wasm.Gt)
genBinOp Gt type_ l r = Left $ "codegen error. Gt " ++ show type_ ++ " " ++ show l ++ " " ++ show r
genBinOp Ge TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Relation Wasm.GeS)
genBinOp Ge TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Relation Wasm.Ge)
genBinOp Ge type_ l r = Left $ "codegen error. Ge " ++ show type_ ++ " " ++ show l ++ " " ++ show r

genAssign :: LValue -> R.Expr -> Either String (Builder Wasm.Instr)
genAssign (LValueLocal (LocalVarIdx idx) _) e = do
    a <- genExpr e
    return $ a <> Builder.singleton (Wasm.LocalTee (fromIntegral idx))
genAssign (LValueGlobal (GlobalVarIdx idx) _) e = do
    a <- genExpr e
    return $ a <> Builder.foldable [Wasm.GlobalSet (fromIntegral idx), Wasm.GlobalGet (fromIntegral idx)]

genConstant :: Constant -> Builder Wasm.Instr
genConstant (ConstInt a)      = Builder.singleton (Wasm.I32Const (fromIntegral a))
genConstant (ConstBool False) = Builder.singleton (Wasm.I32Const 0)
genConstant (ConstBool True)  = Builder.singleton (Wasm.I32Const 1)
genConstant (ConstDouble a)   = Builder.singleton (Wasm.F64Const a)

genReference :: Reference -> Builder Wasm.Instr
genReference (ReferenceLocal (LocalVarIdx idx) _) = Builder.singleton (Wasm.LocalGet (fromIntegral idx))
genReference (ReferenceGlobal (GlobalVarIdx idx) _) = Builder.singleton (Wasm.GlobalGet (fromIntegral idx))
genReference (ReferenceFunction (FunctionIdx idx) _) = Builder.singleton (Wasm.RefFunc (fromIntegral idx))

genFunctionCall :: FunctionIdx -> Vector R.Expr -> Either String (Builder Wasm.Instr)
genFunctionCall (FunctionIdx idx) args = do
    xs <- Vector.foldM' (\acc e -> (acc <>) <$> genExpr e) Builder.empty args
    return $ xs <> Builder.singleton (Wasm.Call (fromIntegral idx))

genStatement :: Int -> R.Statement -> Either String (Builder Wasm.Instr)
genStatement _ (R.StatementIf cond body1 body2) = do
    condCode <- genExpr cond
    body1Code <- Builder.build <$> Vector.foldM' (\acc e -> (acc <>) <$> genStatement 0 e) Builder.empty body1
    body2Code <- Builder.build <$> Vector.foldM' (\acc e -> (acc <>) <$> genStatement 0 e) Builder.empty body2
    return (condCode <> Builder.singleton (Wasm.If Wasm.BlockTypeEmpty body1Code body2Code))
genStatement _ (R.StatementWhile cond body) = do
    condCode <- genExpr cond
    bodyCode <- Vector.foldM' (\acc e -> (acc <>) <$> genStatement 1 e) Builder.empty body
    return . Builder.singleton . Wasm.Block Wasm.BlockTypeEmpty .
        Vector.singleton . Wasm.Loop Wasm.BlockTypeEmpty . Builder.build $
            condCode
            <>
            Builder.foldable [ Wasm.I32Test Wasm.Eqz, Wasm.BrIf 1 ]
            <>
            bodyCode
            <>
            Builder.singleton (Wasm.Br 0)
genStatement _ (R.StatementFor pre cond post body) = do
    preCode <- genDeclOrExpr pre
    condCode <- genExpr cond
    bodyCode <- Vector.foldM' (\acc e -> (acc <>) <$> genStatement 1 e) Builder.empty body
    postCode <- genExpr post
    return . Builder.singleton . Wasm.Block Wasm.BlockTypeEmpty . Builder.build $
        preCode
        <>
        Builder.singleton Wasm.Drop
        <>
        (Builder.singleton . Wasm.Loop Wasm.BlockTypeEmpty . Builder.build $
            condCode
            <>
            Builder.foldable [ Wasm.I32Test Wasm.Eqz, Wasm.BrIf 1 ]
            <>
            bodyCode
            <>
            postCode
            <>
            Builder.singleton Wasm.Drop
            <>
            Builder.singleton (Wasm.Br 0)
        )
genStatement _ (R.StatementExpr e) = (<> Builder.singleton Wasm.Drop) <$> genExpr e
genStatement _ (R.StatementDecl (R.Declaration _ _ Nothing)) = return Builder.empty
genStatement _ (R.StatementDecl (R.Declaration _ lvalue (Just initializer))) = genAssign lvalue initializer
genStatement breakLabel R.StatementBreak = return (Builder.singleton (Wasm.Br (fromIntegral breakLabel)))
genStatement _ (R.StatementReturn Nothing) = return (Builder.singleton Wasm.Return)
genStatement _ (R.StatementReturn (Just e)) = (<> Builder.singleton Wasm.Return) <$> genExpr e

genFunction :: R.Function -> Either String WasmFunc
genFunction (R.Function name funcType locals body) = do
    builder <- Vector.foldM' (\acc e -> (acc <>) <$> genStatement 0 e) Builder.empty body
    let instrVec = Builder.build builder
        FunctionType params _ = funcType
        localVec = genLocalVec (Vector.drop (length params) locals)
        type_ = genFunctionType funcType
    return (WasmFunc name type_ localVec instrVec)

genLocalVec :: Vector LocalVar -> Vector Wasm.ValType
genLocalVec = Vector.map $ \(LocalVar _ type_) -> toValType type_

genFunctionType :: FunctionType -> Wasm.FuncType
genFunctionType (FunctionType params result) = Wasm.FuncType (Wasm.ResultType paramVec) (Wasm.ResultType resultVec)
    where
    paramVec = Vector.fromList . map (\(Parameter _ type_) -> toValType type_) $ params
    resultVec
        | result /= TypeVoid = Vector.singleton . toValType $ result
        | otherwise = mempty

toValType :: Type' -> Wasm.ValType
toValType TypeVoid       = Wasm.NumType Wasm.I32
toValType TypeInt        = Wasm.NumType Wasm.I32
toValType TypeBool       = Wasm.NumType Wasm.I32
toValType TypeDouble     = Wasm.NumType Wasm.F64
toValType TypeString     = Wasm.NumType Wasm.I64
toValType TypeFunction{} = Wasm.RefType Wasm.FuncRef
