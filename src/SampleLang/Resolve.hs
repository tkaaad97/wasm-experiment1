module SampleLang.Resolve
    ( resolve
    , resolveFunction
    , resolveExpr
    , resolveStatement
    , resolveLValue
    ) where

import Control.Monad (mplus, unless)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList, lookup, size)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList, imap, length, map, mapM,
                                        toList)
import qualified SampleLang.Ast.Parsed as P
import qualified SampleLang.Ast.Resolved as R
import SampleLang.Ast.Types

resolve :: P.Ast -> Either String R.Program
resolve ast = do
    let gvars = pickGlobalVars ast
        gvarVec = Vector.fromList gvars
        gvarMap = Map.fromList . Vector.toList . Vector.imap (\i (GlobalVar name _) -> (name, GlobalVarIdx i)) $ gvarVec
    unless (Map.size gvarMap == Vector.length gvarVec) $ Left "global variable name duplication"

    let funcs = pickFunctionDefinitions ast
        funcVec = Vector.fromList funcs
        funcMap = Map.fromList . Vector.toList . Vector.imap (\i (P.FunctionDefinition name _ _) -> (name, FunctionIdx i)) $ funcVec
    unless (Map.size funcMap == Vector.length funcVec) $ Left "function name duplication"

    resolvedFuncVec <- Vector.mapM (resolveFunction funcMap gvarMap) funcVec
    return (R.Program resolvedFuncVec gvarVec)

resolveFunction :: Map Text FunctionIdx -> Map Text GlobalVarIdx -> P.FunctionDefinition -> Either String R.Function
resolveFunction funcMap gvarMap funcDef = do
    body' <- Vector.fromList <$> mapM (resolveStatement funcMap gvarMap lvarMap) body
    return (R.Function name funcType localVarVec body')
    where
    P.FunctionDefinition name funcType body = funcDef
    localVarVec = pickLocalVars funcDef
    lvarMap = Map.fromList . Vector.toList . Vector.imap (\i (LocalVar name _) -> (name, LocalVarIdx i)) $ localVarVec

resolveExpr :: Map Text FunctionIdx -> Map Text GlobalVarIdx -> Map Text LocalVarIdx -> P.Expr -> Either String R.Expr
resolveExpr funcMap gvarMap lvarMap (P.ExprUnary op a) =
    R.ExprUnary op <$> resolveExpr funcMap gvarMap lvarMap a
resolveExpr funcMap gvarMap lvarMap (P.ExprBinary op a b) =
    R.ExprBinary op <$> resolveExpr funcMap gvarMap lvarMap a <*> resolveExpr funcMap gvarMap lvarMap b
resolveExpr funcMap gvarMap lvarMap (P.ExprAssign a b) =
    R.ExprAssign <$> resolveLValue gvarMap lvarMap a <*> resolveExpr funcMap gvarMap lvarMap b
resolveExpr funcMap gvarMap lvarMap (P.ExprIndexAccess a b) =
    R.ExprIndexAccess <$> resolveLValue gvarMap lvarMap a <*> resolveExpr funcMap gvarMap lvarMap b
resolveExpr _ _ _ (P.ExprConstant a) = return (R.ExprConstant a)
resolveExpr _ gvarMap lvarMap (P.ExprReference a) =
    R.ExprReference <$> resolveLValue gvarMap lvarMap (P.ExprReference a)
resolveExpr funcMap gvarMap lvarMap (P.ExprFunctionCall name args) = do
    funcIdx <- maybe (Left $ "function not found: " ++ Text.unpack name) return $ Map.lookup name funcMap
    args' <- Vector.fromList <$> mapM (resolveExpr funcMap gvarMap lvarMap) args
    return (R.ExprFunctionCall funcIdx args')

resolveLValue :: Map Text GlobalVarIdx -> Map Text LocalVarIdx -> P.Expr -> Either String LValue
resolveLValue gvarMap lvarMap (P.ExprReference name) = do
    let maybeLocal = LValueLocal <$> Map.lookup name lvarMap
        maybeGlobal = LValueGlobal <$> Map.lookup name gvarMap
    maybe (Left $ "not found: " ++ Text.unpack name) return (maybeLocal `mplus` maybeGlobal)
resolveLValue _ _ e = Left $ "not LValue: " ++ show e

resolveStatement :: Map Text FunctionIdx -> Map Text GlobalVarIdx -> Map Text LocalVarIdx -> P.Statement -> Either String R.Statement
resolveStatement funcMap gvarMap lvarMap (P.StatementIf cond body) =
    R.StatementIf <$>
        resolveExpr funcMap gvarMap lvarMap cond <*>
        (Vector.fromList <$> mapM (resolveStatement funcMap gvarMap lvarMap) body)
resolveStatement funcMap gvarMap lvarMap (P.StatementFor (Left pre) cond post body) =
    R.StatementFor (Left pre) <$>
        resolveExpr funcMap gvarMap lvarMap cond <*>
        resolveExpr funcMap gvarMap lvarMap post <*>
        (Vector.fromList <$> mapM (resolveStatement funcMap gvarMap lvarMap) body)
resolveStatement funcMap gvarMap lvarMap (P.StatementFor (Right pre) cond post body) =
    R.StatementFor <$>
        (Right <$> resolveExpr funcMap gvarMap lvarMap pre) <*>
        resolveExpr funcMap gvarMap lvarMap cond <*>
        resolveExpr funcMap gvarMap lvarMap post <*>
        (Vector.fromList <$> mapM (resolveStatement funcMap gvarMap lvarMap) body)
resolveStatement funcMap gvarMap lvarMap (P.StatementWhile cond body) =
    R.StatementWhile <$>
        resolveExpr funcMap gvarMap lvarMap cond <*>
        (Vector.fromList <$> mapM (resolveStatement funcMap gvarMap lvarMap) body)
resolveStatement funcMap gvarMap lvarMap (P.StatementExpr e) =
    R.StatementExpr <$>
        resolveExpr funcMap gvarMap lvarMap e
resolveStatement _ _ _ (P.StatementDecl a) =
    return (R.StatementDecl a)
resolveStatement funcMap gvarMap lvarMap (P.StatementReturn e) =
    R.StatementReturn <$> resolveExpr funcMap gvarMap lvarMap e

pickFunctionDefinitions :: P.Ast -> [P.FunctionDefinition]
pickFunctionDefinitions (P.Ast xs) = mapMaybe isFuncDef xs
    where
    isFuncDef (P.Decl _)    = Nothing
    isFuncDef (P.FuncDef a) = Just a

pickGlobalVars :: P.Ast -> [GlobalVar]
pickGlobalVars (P.Ast xs) = mapMaybe isDecl xs
    where
    isDecl (P.Decl (Parameter name type_)) = Just (GlobalVar name type_)
    isDecl (P.FuncDef _)                   = Nothing

pickLocalVars :: P.FunctionDefinition -> Vector LocalVar
pickLocalVars (P.FunctionDefinition _ funcType statements) =
    Vector.fromList (paramLocals ++ concatMap pick statements)
    where
    FunctionType params _ = funcType
    paramLocals = map parameterToLocalVar params
    pick (P.StatementIf _ xs)                = concatMap pick xs
    pick (P.StatementFor (Left param) _ _ _) = [parameterToLocalVar param]
    pick P.StatementFor{}                    = []
    pick (P.StatementWhile _ xs)             = concatMap pick xs
    pick (P.StatementExpr _)                 = []
    pick (P.StatementDecl param)             = [parameterToLocalVar param]
    pick (P.StatementReturn _)               = []
    parameterToLocalVar (Parameter name type_) = LocalVar name type_
