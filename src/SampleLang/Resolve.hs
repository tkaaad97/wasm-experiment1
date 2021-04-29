module SampleLang.Resolve
    (
    ) where

import Data.Maybe (mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import qualified SampleLang.Ast.Parsed as P
import qualified SampleLang.Ast.Resolved as R
import SampleLang.Ast.Types

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
