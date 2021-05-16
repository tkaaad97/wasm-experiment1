module SampleLang.Ast.Types
    ( Type'(..)
    , UnOp(..)
    , BinOp(..)
    , FunctionType(..)
    , Parameter(..)
    , Constant(..)
    , FunctionIdx(..)
    , GlobalVarIdx(..)
    , LocalVarIdx(..)
    , StringIdx(..)
    , GlobalVar(..)
    , LocalVar(..)
    , LValue(..)
    , Reference(..)
    , getResultType
    , getLValueType
    , getReferenceType
    , getConstantType
    , isFunctionType
    ) where

import Data.Int (Int32)
import Data.Text (Text)

data Type' =
    TypeVoid |
    TypeInt |
    TypeBool |
    TypeDouble |
    TypeString |
    TypeFunction !FunctionType
    deriving (Show, Eq)

data UnOp =
    Negate |
    Not |
    Increment |
    Decrement
    deriving (Show, Eq)

data BinOp =
    Add |
    Sub |
    Mul |
    Div |
    Equ |
    Neq |
    Lt |
    Le |
    Gt |
    Ge
    deriving (Show, Eq)

data FunctionType = FunctionType ![Parameter] !Type'
    deriving (Show, Eq)

data Parameter = Parameter !Text !Type'
    deriving (Show, Eq)

data Constant =
    ConstInt !Int32 |
    ConstBool !Bool |
    ConstDouble !Double
    deriving (Show, Eq)

newtype FunctionIdx = FunctionIdx
    { unFunctionIdx :: Int
    } deriving (Show, Eq)

newtype GlobalVarIdx = GlobalVarIdx
    { unGVarIdx :: Int
    } deriving (Show, Eq)

newtype LocalVarIdx = LocalVarIdx
    { unLocalVarIdx :: Int
    } deriving (Show, Eq)

newtype StringIdx = StringIdx
    { unStringIdx :: Int
    } deriving (Show, Eq)

data GlobalVar = GlobalVar !Text !Type'
    deriving (Show, Eq)

data LocalVar = LocalVar !Text !Type'
    deriving (Show, Eq)

data LValue =
    LValueLocal !LocalVarIdx !Type' |
    LValueGlobal !GlobalVarIdx !Type'
    deriving (Show, Eq)

data Reference =
    ReferenceLocal !LocalVarIdx !Type' |
    ReferenceGlobal !GlobalVarIdx !Type' |
    ReferenceFunction !FunctionIdx !FunctionType
    deriving (Show, Eq)

getResultType :: FunctionType -> Type'
getResultType (FunctionType _ resultType) = resultType

getLValueType :: LValue -> Type'
getLValueType (LValueLocal _ type_)  = type_
getLValueType (LValueGlobal _ type_) = type_

getReferenceType :: Reference -> Type'
getReferenceType (ReferenceLocal _ type_)       = type_
getReferenceType (ReferenceGlobal _ type_)      = type_
getReferenceType (ReferenceFunction _ funcType) = TypeFunction funcType

getConstantType :: Constant -> Type'
getConstantType ConstInt{}    = TypeInt
getConstantType ConstBool{}   = TypeBool
getConstantType ConstDouble{} = TypeDouble

isFunctionType :: Type' -> Bool
isFunctionType TypeFunction{} = True
isFunctionType _              = False
