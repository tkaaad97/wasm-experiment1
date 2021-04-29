module SampleLang.Ast.Parsed
    ( Type'(..)
    , FunctionType(..)
    , Parameter(..)
    , Constant(..)
    , Ast(..)
    , DeclOrFuncDef(..)
    , Expr(..)
    , UnOp(..)
    , BinOp(..)
    , Statement(..)
    , FunctionDefinition(..)
    ) where

import Data.Text (Text)
import SampleLang.Ast.Types

newtype Ast = Ast [DeclOrFuncDef]
    deriving (Show, Eq)

data DeclOrFuncDef =
    Decl !Parameter |
    FuncDef !FunctionDefinition
    deriving (Show, Eq)

data Expr =
    ExprUnary !UnOp !Expr |
    ExprBinary !BinOp !Expr !Expr |
    ExprAssign !Expr !Expr |
    ExprIndexAccess !Expr !Expr |
    ExprConstant !Constant |
    ExprReference !Text |
    ExprFunctionCall !Text ![Expr]
    deriving (Show, Eq)

type DeclOrExpr = Either Parameter Expr

data Statement =
    StatementIf !Expr ![Statement] |
    StatementFor !DeclOrExpr !Expr !Expr ![Statement] |
    StatementWhile !Expr ![Statement] |
    StatementExpr !Expr |
    StatementDecl !Parameter |
    StatementReturn !Expr
    deriving (Show, Eq)

data FunctionDefinition =
    FunctionDefinition !Text !FunctionType ![Statement]
    deriving (Show, Eq)
