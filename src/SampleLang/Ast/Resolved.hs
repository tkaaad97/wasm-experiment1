module SampleLang.Ast.Resolved
    ( Expr(..)
    , UnOp(..)
    , BinOp(..)
    , LValue(..)
    , Statement(..)
    , Function(..)
    , Program(..)
    ) where

import Data.Text (Text)
import Data.Vector (Vector)
import SampleLang.Ast.Types

data Expr =
    ExprUnary !UnOp !Expr |
    ExprBinary !BinOp !Expr !Expr |
    ExprAssign !LValue !Expr |
    ExprIndexAccess !LValue !Expr |
    ExprConstant !Constant |
    ExprReference !LValue |
    ExprFunctionCall !FunctionIdx !(Vector Expr)
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

data Program = Program
    { programFunctions  :: !(Vector Function)
    , programGlobalVars :: !(Vector GlobalVar)
    } deriving (Show, Eq)
