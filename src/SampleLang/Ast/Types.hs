module SampleLang.Ast.Types
    ( Type'(..)
    , FunctionType(..)
    , Parameter(..)
    , Constant(..)
    , FunctionIdx(..)
    , GlobalVarIdx(..)
    , LocalVarIdx(..)
    , GlobalVar(..)
    , LocalVar(..)
    ) where

import Data.Text (Text)
import Data.Word (Word64)

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

data Constant =
    ConstInt !Word64 |
    ConstBool !Bool |
    ConstDouble !Double
    deriving (Show, Eq)

newtype FunctionIdx = FunctionIdx
    { unFunctionIdx :: Int
    } deriving (Show, Eq)

newtype GlobalVarIdx = GVarIdx
    { unGVarIdx :: Int
    } deriving (Show, Eq)

newtype LocalVarIdx = LocalVarIdx
    { unLocalVarIdx :: Int
    } deriving (Show, Eq)

data GlobalVar = GlobalVar !Text !Type'
    deriving (Show, Eq)

data LocalVar = LocalVar !Text !Type'
    deriving (Show, Eq)