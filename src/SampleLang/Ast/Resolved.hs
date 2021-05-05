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
    ExprUnary !Type' !UnOp !Expr |
    ExprBinary !Type' !BinOp !Expr !Expr |
    ExprAssign !Type' !LValue !Expr |
    ExprConstant !Type' !Constant |
    ExprReference !Type' !Reference |
    ExprFunctionCall !Type' !FunctionIdx !(Vector Expr)
    deriving (Show, Eq)

type DeclOrExpr = Either Parameter Expr

data Statement =
    StatementIf !Expr !(Vector Statement) !(Vector Statement) |
    StatementFor !DeclOrExpr !Expr !Expr !(Vector Statement) |
    StatementWhile !Expr !(Vector Statement) |
    StatementExpr !Expr |
    StatementDecl !Parameter |
    StatementReturn !(Maybe Expr) |
    StatementBreak
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
