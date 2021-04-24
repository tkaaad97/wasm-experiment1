module SampleLang.Ast
    ( Expr(..)
    , Type'(..)
    , FunctionType(..)
    , Parameter(..)
    , Unary(..)
    , Binary(..)
    , Relation(..)
    , Constant(..)
    , Statement(..)
    , Declaration(..)
    , FunctionDefinition(..)
    ) where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Word (Word32)

data Expr =
    ExprUnary !Unary |
    ExprBinary !Binary |
    ExprRelation !Relation |
    ExprIndexAccess !Text !Expr |
    ExprConstant !Constant |
    ExprReference !Text |
    ExprFunctionCall !Text
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

data Unary =
    Negate !Expr |
    Not !Expr |
    Increment !Expr |
    Decrement !Expr
    deriving (Show, Eq)

data Binary =
    Add !Expr !Expr |
    Sub !Expr !Expr |
    Mul !Expr !Expr |
    Div !Expr !Expr
    deriving (Show, Eq)

data Relation =
    Equ !Expr !Expr |
    Neq !Expr !Expr |
    Lt !Expr !Expr |
    Le !Expr !Expr |
    Gt !Expr !Expr |
    Ge !Expr !Expr
    deriving (Show, Eq)

data Constant =
    ConstInt !Integer |
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
