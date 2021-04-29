module SampleLang.Ast.Resolved
    ( Expr(..)
    , UnOp(..)
    , BinOp(..)
    , RelOp(..)
    , LValue(..)
    , Statement(..)
    , Function(..)
    ) where

import Data.Text (Text)
import Data.Vector (Vector)
import SampleLang.Ast.Types

data Expr =
    ExprUnOp !Type' !UnOp |
    ExprBinOp !Type' !BinOp |
    ExprRelOp !Type' !RelOp |
    ExprAssign !Type' !LValue !Expr |
    ExprIndexAccess !Type' !LValue !Expr |
    ExprConstant !Type' !Constant |
    ExprReference !Type' !LValue |
    ExprFunctionCall !Type' !FunctionIdx !(Vector Expr)
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

data LValue =
    LocalVar !LocalVarIdx |
    GlobalVar !GlobalVarIdx
    deriving (Show, Eq)

type DeclOrExpr = Either Parameter Expr

data Statement =
    StatementIf !Expr !(Vector Statement) |
    StatementFor !DeclOrExpr !Expr !Expr !(Vector Statement) |
    StatementWhile !Expr !(Vector Statement) |
    StatementExpr !Expr |
    StatementDecl !Parameter |
    StatementReturn !Expr
    deriving (Show, Eq)

data Function = Function
    { functionName   :: !Text
    , functionType   :: !FunctionType
    , functionLocals :: !(Vector LocalVar)
    , functionBody   :: !(Vector Statement)
    } deriving (Show, Eq)
