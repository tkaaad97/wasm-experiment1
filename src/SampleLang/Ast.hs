module SampleLang.Ast
    ( Expr(..)
    , Type'(..)
    , FunctionType(..)
    , Parameter(..)
    , UnOp(..)
    , BinOp(..)
    , RelOp(..)
    , ConstExpr(..)
    , Statement(..)
    , Declaration(..)
    , FunctionDefinition(..)
    ) where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Word (Word32)

data Expr =
    Unary !UnOp |
    Binary !BinOp |
    Relation !RelOp |
    IndexAccess !Text !Expr |
    Constant !ConstExpr |
    Reference !Text |
    FunctionCall !Text
    deriving (Show, Eq)

data Type' =
    TypeUInt |
    TypeInt |
    TypeBool |
    TypeDouble |
    TypeArray !Int !Type' |
    TypeFunction !FunctionType
    deriving (Show, Eq)

data FunctionType = FunctionType ![Parameter] !Type'
    deriving (Show, Eq)

data Parameter = Parameter !Text !Type'
    deriving (Show, Eq)

data UnOp =
    UnaryNegate !Expr |
    UnaryNot !Expr |
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

data ConstExpr =
    ConstInt !Int32 |
    ConstUInt !Word32 |
    ConstBool !Bool |
    ConstDouble !Double
    deriving (Show, Eq)

data Statement =
    StatementIf !Expr ![Statement] |
    StatementFor !Expr !Expr !Expr ![Statement] |
    StatementWhile !Expr ![Statement] |
    StatementExpr !Expr |
    StatementDecl !Declaration |
    StatementReturn !Expr
    deriving (Show, Eq)

data Declaration =
    Declaration !Text !Expr
    deriving (Show, Eq)

data FunctionDefinition =
    FunctionDefinition !Text !FunctionType ![Statement]
    deriving (Show, Eq)
