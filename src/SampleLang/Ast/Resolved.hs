module SampleLang.Ast.Resolved
    ( Expr(..)
    , Declaration(..)
    , UnOp(..)
    , BinOp(..)
    , LValue(..)
    , Statement(..)
    , Function(..)
    , Program(..)
    ) where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word64)
import SampleLang.Ast.Types

data Expr =
    ExprUnary !Type' !UnOp !Expr |
    ExprBinary !Type' !BinOp !Expr !Expr |
    ExprAssign !Type' !LValue !Expr |
    ExprConstant !Type' !Constant |
    ExprReference !Type' !Reference |
    ExprFunctionCall !Type' !FunctionIdx !(Vector Expr) |
    ExprStringLiteral !Word64
    deriving (Show, Eq)

data Declaration = Declaration !Parameter !LValue !(Maybe Expr)
    deriving (Show, Eq)

type DeclOrExpr = Either Declaration Expr

data Statement =
    StatementIf !Expr !(Vector Statement) !(Vector Statement) |
    StatementFor !DeclOrExpr !Expr !Expr !(Vector Statement) |
    StatementWhile !Expr !(Vector Statement) |
    StatementExpr !Expr |
    StatementDecl !Declaration |
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
    { programExternalFunctions :: !(Vector (Text, FunctionType))
    , programFunctions         :: !(Vector Function)
    , programStringLiterals    :: !(Vector (Text, Word64))
    , programGlobalVars        :: !(Vector (GlobalVar, Maybe Expr))
    } deriving (Show, Eq)
