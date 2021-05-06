module SampleLang.Ast.Parsed
    ( Type'(..)
    , FunctionType(..)
    , Parameter(..)
    , Constant(..)
    , Ast(..)
    , Declaration(..)
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

data Declaration = Declaration !Parameter !(Maybe Expr)
    deriving (Show, Eq)

data DeclOrFuncDef =
    Decl !Declaration |
    FuncDef !FunctionDefinition
    deriving (Show, Eq)

data Expr =
    ExprUnary !UnOp !Expr |
    ExprBinary !BinOp !Expr !Expr |
    ExprAssign !Expr !Expr |
    ExprConstant !Constant |
    ExprReference !Text |
    ExprFunctionCall !Text ![Expr]
    deriving (Show, Eq)

type DeclOrExpr = Either Declaration Expr

data Statement =
    StatementIf !Expr ![Statement] ![Statement] |
    StatementFor !DeclOrExpr !Expr !Expr ![Statement] |
    StatementWhile !Expr ![Statement] |
    StatementExpr !Expr |
    StatementDecl !Declaration |
    StatementReturn !(Maybe Expr) |
    StatementBreak
    deriving (Show, Eq)

data FunctionDefinition =
    FunctionDefinition !Text !FunctionType ![Statement]
    deriving (Show, Eq)
