module SampleLang.Ast.Parsed
    ( Expr(..)
    , Type'(..)
    , FunctionType(..)
    , Parameter(..)
    , UnOp(..)
    , BinOp(..)
    , RelOp(..)
    , Constant(..)
    , Statement(..)
    , FunctionDefinition(..)
    ) where

import Data.Text (Text)
import Data.Word (Word64)

data Expr =
    ExprUnOp !UnOp |
    ExprBinOp !BinOp |
    ExprRelOp !RelOp |
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

data Constant =
    ConstInt !Word64 |
    ConstBool !Bool |
    ConstDouble !Double
    deriving (Show, Eq)

data Statement =
    StatementIf !Expr ![Statement] |
    StatementFor !Expr !Expr !Expr ![Statement] |
    StatementWhile !Expr ![Statement] |
    StatementExpr !Expr |
    StatementDecl !Parameter |
    StatementReturn !Expr
    deriving (Show, Eq)

data FunctionDefinition =
    FunctionDefinition !Text !FunctionType ![Statement]
    deriving (Show, Eq)
