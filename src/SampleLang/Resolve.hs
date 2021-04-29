module SampleLang.Resolve
    (
    ) where

import Control.Monad (mplus)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (lookup)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import qualified SampleLang.Ast.Parsed as P
import qualified SampleLang.Ast.Resolved as R
import SampleLang.Ast.Types

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
resolveLValue gvarMap lvarMap e = Left $ "not LValue: " ++ show e

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
    Vector.fromList . concatMap pick $ statements
    where
    FunctionType params _ = funcType
    pick (P.StatementIf _ xs)                = concatMap pick xs
    pick (P.StatementFor (Left param) _ _ _) = [parameterToLocalVar param]
    pick P.StatementFor{}                    = []
    pick (P.StatementWhile _ xs)             = concatMap pick xs
    pick (P.StatementExpr _)                 = []
    pick (P.StatementDecl param)             = [parameterToLocalVar param]
    pick (P.StatementReturn _)               = []
    parameterToLocalVar (Parameter name type_) = LocalVar name type_
