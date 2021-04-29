module SampleLang.Ast.Parsed
    ( Type'(..)
    , FunctionType(..)
    , Parameter(..)
    , Constant(..)
    , Expr(..)
    , UnOp(..)
    , BinOp(..)
    , RelOp(..)
    , Statement(..)
    , FunctionDefinition(..)
    ) where

import Data.Text (Text)
import SampleLang.Ast.Types

data Expr =
    ExprUnOp !UnOp |
    ExprBinOp !BinOp |
    ExprRelOp !RelOp |
    ExprAssign !Expr !Expr |
    ExprIndexAccess !Text !Expr |
    ExprConstant !Constant |
    ExprReference !Text |
    ExprFunctionCall !Text
    deriving (Show, Eq)

data UnOp =
    Negate !Expr |
    Not !Expr |
    Increment !Expr |
    Decrement !Expr
    deriving (Show, Eq)

data BinOp =
    Add !Expr !Expr |
    Sub !Expr !Expr |
    Mul !Expr !Expr |
    Div !Expr !Expr
    deriving (Show, Eq)

data RelOp =
    Equ !Expr !Expr |
    Neq !Expr !Expr |
    Lt !Expr !Expr |
    Le !Expr !Expr |
    Gt !Expr !Expr |
    Ge !Expr !Expr
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
