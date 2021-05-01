module SampleLang.CodeGen.Wasm
    (
    ) where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (foldM')
import qualified SampleLang.Ast.Resolved as R
import SampleLang.Ast.Types
import VectorBuilder.Builder (Builder)
import qualified VectorBuilder.Builder as Builder (empty, foldable, singleton,
                                                   vector)
import qualified Wasm.Types as Wasm

data WasmFunc = WasmFunc !Text !Wasm.FuncType !(Vector Wasm.ValType) !(Vector Wasm.Instr)

genExpr :: R.Expr -> Either String (Builder Wasm.Instr)
genExpr (R.ExprUnary type_ op e)        = genUnOp op type_ e
genExpr (R.ExprBinary type_ op l r)     = genBinOp op type_ l r
genExpr (R.ExprAssign _ l r)            = genAssign l r
genExpr (R.ExprConstant _ a)            = return (genConstant a)
genExpr (R.ExprReference _ a)           = return (genReference a)
genExpr (R.ExprFunctionCall _ idx args) = genFunctionCall idx args

genUnOp :: UnOp -> Type' -> R.Expr -> Either String (Builder Wasm.Instr)
genUnOp Negate TypeInt e = do
    a <- genExpr e
    return $ Builder.singleton (Wasm.I32Const 0) <> a <> Builder.singleton (Wasm.I32Binary Wasm.ISub)
genUnOp Negate TypeDouble e = do
    a <- genExpr e
    return $ a <> Builder.singleton (Wasm.F64Unary Wasm.Neg)
genUnOp Negate type_ e = Left $ "error. Negate" ++ show type_ ++ " " ++ show e
genUnOp Not TypeBool e = do
    a <- genExpr e
    return $ a <> Builder.singleton (Wasm.I32Const 0) <> Builder.singleton (Wasm.I32Relation Wasm.IEq)
genUnOp Not type_ e = Left $ "error. Not" ++ show type_ ++ " " ++ show e
genUnOp Increment TypeInt e = do
    a <- genExpr e
    return $ a <> Builder.singleton (Wasm.I32Const 1) <> Builder.singleton (Wasm.I32Binary Wasm.IAdd)
genUnOp Increment type_ e = Left $ "error. Increment" ++ show type_ ++ " " ++ show e
genUnOp Decrement TypeInt e = do
    a <- genExpr e
    return $ a <> Builder.singleton (Wasm.I32Const 1) <> Builder.singleton (Wasm.I32Binary Wasm.ISub)
genUnOp Decrement type_ e =  Left $ "error. Decrement" ++ show type_ ++ " " ++ show e

genBinOp :: BinOp -> Type' -> R.Expr -> R.Expr -> Either String (Builder Wasm.Instr)
genBinOp Add TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Binary Wasm.IAdd)
genBinOp Add TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Binary Wasm.Add)
genBinOp Add type_ l r = Left $ "error. Add" ++ show type_ ++ " " ++ show l ++ " " ++ show r
genBinOp Sub TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Binary Wasm.ISub)
genBinOp Sub TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Binary Wasm.Sub)
genBinOp Sub type_ l r = Left $ "error. Sub" ++ show type_ ++ " " ++ show l ++ " " ++ show r
genBinOp Mul TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Binary Wasm.IMul)
genBinOp Mul TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Binary Wasm.Mul)
genBinOp Mul type_ l r = Left $ "error. Mul" ++ show type_ ++ " " ++ show l ++ " " ++ show r
genBinOp Div TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Binary Wasm.DivS)
genBinOp Div TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Binary Wasm.Div)
genBinOp Div type_ l r = Left $ "error. Div" ++ show type_ ++ " " ++ show l ++ " " ++ show r
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
genBinOp Equ type_ l r = Left $ "error. Equ" ++ show type_ ++ " " ++ show l ++ " " ++ show r
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
genBinOp Neq type_ l r = Left $ "error. Neq" ++ show type_ ++ " " ++ show l ++ " " ++ show r
genBinOp Lt TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Relation Wasm.LtS)
genBinOp Lt TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Relation Wasm.Lt)
genBinOp Lt type_ l r = Left $ "error. Lt" ++ show type_ ++ " " ++ show l ++ " " ++ show r
genBinOp Le TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Relation Wasm.LeS)
genBinOp Le TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Relation Wasm.Le)
genBinOp Le type_ l r = Left $ "error. Le" ++ show type_ ++ " " ++ show l ++ " " ++ show r
genBinOp Gt TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Relation Wasm.GtS)
genBinOp Gt TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Relation Wasm.Gt)
genBinOp Gt type_ l r = Left $ "error. Gt" ++ show type_ ++ " " ++ show l ++ " " ++ show r
genBinOp Ge TypeInt l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.I32Relation Wasm.GeS)
genBinOp Ge TypeDouble l r = do
    l' <- genExpr l
    r' <- genExpr r
    return $ l' <> r' <> Builder.singleton (Wasm.F64Relation Wasm.Ge)
genBinOp Ge type_ l r = Left $ "error. Ge" ++ show type_ ++ " " ++ show l ++ " " ++ show r

genAssign :: LValue -> R.Expr -> Either String (Builder Wasm.Instr)
genAssign (LValueLocal (LocalVarIdx idx) _) e = do
    a <- genExpr e
    return $ a <> Builder.singleton (Wasm.LocalSet (fromIntegral idx))
genAssign (LValueGlobal (GlobalVarIdx idx) _) e = do
    a <- genExpr e
    return $ a <> Builder.singleton (Wasm.GlobalSet (fromIntegral idx))

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
