module SampleLang.Resolve
    ( resolve
    , resolveFunction
    , resolveExpr
    , resolveStatement
    , resolveLValue
    , getExprType
    ) where

import Control.Monad (mplus, unless)
import Data.Bits ((.&.), (.|.))
import qualified Data.Bits as Bits (shift)
import qualified Data.ByteString as ByteString (length)
import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList, lookup, size)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import Data.Traversable (mapAccumL)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (find, fromList, imap, last, length, map,
                                        mapM, null, toList)
import Data.Word (Word64)
import qualified SampleLang.Ast.Parsed as P
import qualified SampleLang.Ast.Resolved as R
import SampleLang.Ast.Types

getExprType :: R.Expr -> Type'
getExprType (R.ExprUnary type_ _ _)        = type_
getExprType (R.ExprBinary type_ _ _ _)     = type_
getExprType (R.ExprAssign type_ _ _)       = type_
getExprType (R.ExprConstant type_ _)       = type_
getExprType (R.ExprReference type_ _)      = type_
getExprType (R.ExprFunctionCall type_ _ _) = type_
getExprType (R.ExprStringLiteral _)        = TypeString

resolve :: P.Ast -> Either String R.Program
resolve ast = do
    let funcs = pickFunctionDefinitions ast
        funcVec = Vector.fromList funcs
        funcMap = Map.fromList . Vector.toList . Vector.imap (\i (P.FunctionDefinition name funcType _) -> (name, (FunctionIdx i, funcType))) $ funcVec
    unless (Map.size funcMap == Vector.length funcVec) $ Left "function name duplication"

    let gvars = pickGlobalVars ast
        gvarMap = Map.fromList . Vector.toList . Vector.imap (\i (GlobalVar name type_) -> (name, (GlobalVarIdx i, type_))) . Vector.fromList $ map fst gvars
    unless (Map.size gvarMap == length gvars) $ Left "global variable name duplication"

    let strVec = pickStringLiterals ast
        strMap = Map.fromList . Vector.toList . Vector.map (\(str, offLen) -> (str, offLen)) $ strVec

    resolvedFuncVec <- Vector.mapM (resolveFunction funcMap strMap gvarMap) funcVec

    gvarVec <- Vector.mapM (resolveGVar funcMap strMap gvarMap) . Vector.fromList $ gvars
    return (R.Program resolvedFuncVec strVec gvarVec)

resolveGVar :: Map Text (FunctionIdx, FunctionType) -> Map Text Word64 -> Map Text (GlobalVarIdx, Type') -> (GlobalVar, Maybe P.Expr) -> Either String (GlobalVar, Maybe R.Expr)
resolveGVar _ _ _ (gvar, Nothing) = return (gvar, Nothing)
resolveGVar funcMap strMap gvarMap (gvar, Just initializer) = (,) gvar . Just <$> resolveExpr funcMap strMap gvarMap mempty initializer

resolveFunction :: Map Text (FunctionIdx, FunctionType) -> Map Text Word64 -> Map Text (GlobalVarIdx, Type') -> P.FunctionDefinition -> Either String R.Function
resolveFunction funcMap strMap gvarMap funcDef = do
    body' <- Vector.fromList <$> mapM (resolveStatement funcMap strMap gvarMap lvarMap) body
    checkReturnType name (getResultType funcType) body'
    return (R.Function name funcType localVarVec body')
    where
    P.FunctionDefinition name funcType body = funcDef
    localVarVec = pickLocalVars funcDef
    lvarMap = Map.fromList . Vector.toList . Vector.imap (\i (LocalVar vname type_) -> (vname, (LocalVarIdx i, type_))) $ localVarVec

checkFunctionCallType :: Text -> FunctionType -> Vector R.Expr -> Either String ()
checkFunctionCallType name (FunctionType params _) args = do
    unless (Vector.length args == length params) . Left $ "wrong argument number. function: " ++ Text.unpack name
    mapM_ checkArgumentType (zip params . Vector.toList $ args)
    where
    checkArgumentType (Parameter pname type_, e)
        | getExprType e == type_ = return ()
        | otherwise = Left $ "wrong argument type. function: " ++ Text.unpack name ++ " argument: " ++ Text.unpack pname

checkReturnType :: Text -> Type' -> Vector R.Statement -> Either String ()
checkReturnType name TypeVoid body
    | Vector.null body = return ()
    | isJust . Vector.find invalidReturn $ body = Left $ "function " ++ Text.unpack name ++ ": invalid return type"
    | otherwise = return ()
    where
    invalidReturn (R.StatementReturn Nothing)  = False
    invalidReturn (R.StatementReturn (Just _)) = True
    invalidReturn _                            = False
checkReturnType name type_ body
    | Vector.null body = Left $ "function " ++ Text.unpack name ++ ": no return statement"
    | isJust . Vector.find invalidReturn $ body = Left $ "function " ++ Text.unpack name ++ ": invalid return type"
    | otherwise = checkStatementReturnType name (Vector.last body) type_
    where
    invalidReturn (R.StatementReturn Nothing) = True
    invalidReturn (R.StatementReturn (Just expr))
        | getExprType expr == type_ = False
        | otherwise = True
    invalidReturn _ = False

checkStatementReturnType :: Text -> R.Statement -> Type' -> Either String ()
checkStatementReturnType name (R.StatementIf _ body1 body2) type_ = checkReturnType name type_ body1 >> checkReturnType name type_ body2
checkStatementReturnType name (R.StatementFor _ _ _ body) type_ = checkReturnType name type_ body
checkStatementReturnType name (R.StatementWhile _ body) type_ = checkReturnType name type_ body
checkStatementReturnType name R.StatementExpr{} _ = Left $ "function " ++ Text.unpack name ++ ": no return statement"
checkStatementReturnType name R.StatementDecl{} _ = Left $ "function " ++ Text.unpack name ++ ": no return statement"
checkStatementReturnType name (R.StatementReturn Nothing) _ = Left $ "function " ++ Text.unpack name ++ ": no return statement"
checkStatementReturnType name (R.StatementReturn (Just e)) type_
    | getExprType e /= type_ = Left $ "function " ++ Text.unpack name ++ ": invalid return type"
    | otherwise = return ()
checkStatementReturnType name R.StatementBreak _ = Left $ "function " ++ Text.unpack name ++ ": no return statement"

resolveExpr :: Map Text (FunctionIdx, FunctionType) -> Map Text Word64 -> Map Text (GlobalVarIdx, Type') -> Map Text (LocalVarIdx, Type') -> P.Expr -> Either String R.Expr
resolveExpr funcMap strMap gvarMap lvarMap (P.ExprUnary op a) = do
    e <- resolveExpr funcMap strMap gvarMap lvarMap a
    type_ <- resolveUnOpType op (getExprType e)
    return (R.ExprUnary type_ op e)
resolveExpr funcMap strMap gvarMap lvarMap (P.ExprBinary op a b) = do
    l <- resolveExpr funcMap strMap gvarMap lvarMap a
    r <- resolveExpr funcMap strMap gvarMap lvarMap b
    type_ <- resolveBinOpType op (getExprType l) (getExprType r)
    return (R.ExprBinary type_ op l r)
resolveExpr funcMap strMap gvarMap lvarMap (P.ExprAssign a b) = do
    l <- resolveLValue gvarMap lvarMap a
    r <- resolveExpr funcMap strMap gvarMap lvarMap b
    let ltype = getLValueType l
        rtype = getExprType r
    unless (ltype == rtype) . Left $ "wrong type assignment: " ++ show ltype ++ " = " ++ show rtype
    return (R.ExprAssign ltype l r)
resolveExpr _ _ _ _ (P.ExprConstant a) = return (R.ExprConstant (getConstantType a) a)
resolveExpr funcMap _ gvarMap lvarMap (P.ExprReference name) = do
    ref <- resolveReference funcMap gvarMap lvarMap name
    return (R.ExprReference (getReferenceType ref) ref)
resolveExpr funcMap strMap gvarMap lvarMap (P.ExprFunctionCall name args) = do
    (funcIdx, funcType) <- maybe (Left $ "function not found: " ++ Text.unpack name) return $ Map.lookup name funcMap
    args' <- Vector.fromList <$> mapM (resolveExpr funcMap strMap gvarMap lvarMap) args
    checkFunctionCallType name funcType args'
    return (R.ExprFunctionCall (getResultType funcType) funcIdx args')
resolveExpr _ strMap _ _ (P.ExprStringLiteral s) = do
    offLen <- maybe (Left $ "failed to resolve string literalx: " ++ Text.unpack s) return $ Map.lookup s strMap
    return (R.ExprStringLiteral offLen)

resolveUnOpType :: UnOp -> Type' -> Either String Type'
resolveUnOpType Negate TypeInt = return TypeInt
resolveUnOpType Negate TypeDouble = return TypeDouble
resolveUnOpType Negate a = Left $ "invalid argument type for Negate: " ++ show a
resolveUnOpType Not TypeBool = return TypeBool
resolveUnOpType Not a = Left $ "invalid argument type for Not: " ++ show a
resolveUnOpType Increment TypeInt = return TypeInt
resolveUnOpType Increment a = Left $ "invalid argument type for Increment: " ++ show a
resolveUnOpType Decrement TypeInt = return TypeInt
resolveUnOpType Decrement a = Left $ "invalid argument type for Decrement: " ++ show a

resolveBinOpType :: BinOp -> Type' -> Type' -> Either String Type'
resolveBinOpType Add TypeInt TypeInt = return TypeInt
resolveBinOpType Add TypeDouble TypeDouble = return TypeDouble
resolveBinOpType Add a b = Left $ "invalid argument type for Add: " ++ show a ++ ", " ++ show b
resolveBinOpType Sub TypeInt TypeInt = return TypeInt
resolveBinOpType Sub TypeDouble TypeDouble = return TypeDouble
resolveBinOpType Sub a b = Left $ "invalid argument type for Sub: " ++ show a ++ ", " ++ show b
resolveBinOpType Mul TypeInt TypeInt = return TypeInt
resolveBinOpType Mul TypeDouble TypeDouble = return TypeDouble
resolveBinOpType Mul a b = Left $ "invalid argument type for Mul: " ++ show a ++ ", " ++ show b
resolveBinOpType Div TypeInt TypeInt = return TypeInt
resolveBinOpType Div TypeDouble TypeDouble = return TypeDouble
resolveBinOpType Div a b = Left $ "invalid argument type for Div: " ++ show a ++ ", " ++ show b
resolveBinOpType Equ TypeInt TypeInt = return TypeBool
resolveBinOpType Equ TypeBool TypeBool = return TypeBool
resolveBinOpType Equ TypeDouble TypeDouble = return TypeBool
resolveBinOpType Equ a b = Left $ "invalid argument type for Equ: " ++ show a ++ ", " ++ show b
resolveBinOpType Neq TypeInt TypeInt = return TypeBool
resolveBinOpType Neq TypeBool TypeBool = return TypeBool
resolveBinOpType Neq TypeDouble TypeDouble = return TypeBool
resolveBinOpType Neq a b = Left $ "invalid argument type for Neq: " ++ show a ++ ", " ++ show b
resolveBinOpType Lt TypeInt TypeInt = return TypeBool
resolveBinOpType Lt TypeDouble TypeDouble = return TypeBool
resolveBinOpType Lt a b = Left $ "invalid argument type for Lt: " ++ show a ++ ", " ++ show b
resolveBinOpType Le TypeInt TypeInt = return TypeBool
resolveBinOpType Le TypeDouble TypeDouble = return TypeBool
resolveBinOpType Le a b = Left $ "invalid argument type for Le: " ++ show a ++ ", " ++ show b
resolveBinOpType Gt TypeInt TypeInt = return TypeBool
resolveBinOpType Gt TypeDouble TypeDouble = return TypeBool
resolveBinOpType Gt a b = Left $ "invalid argument type for Gt: " ++ show a ++ ", " ++ show b
resolveBinOpType Ge TypeInt TypeInt = return TypeBool
resolveBinOpType Ge TypeDouble TypeDouble = return TypeBool
resolveBinOpType Ge a b = Left $ "invalid argument type for Ge: " ++ show a ++ ", " ++ show b

resolveLValue :: Map Text (GlobalVarIdx, Type') -> Map Text (LocalVarIdx, Type') -> P.Expr -> Either String LValue
resolveLValue gvarMap lvarMap (P.ExprReference name) = do
    let maybeLocal = uncurry LValueLocal <$> Map.lookup name lvarMap
        maybeGlobal = uncurry LValueGlobal <$> Map.lookup name gvarMap
    maybe (Left $ "not found: " ++ Text.unpack name) return (maybeLocal `mplus` maybeGlobal)
resolveLValue _ _ e = Left $ "not LValue: " ++ show e

resolveLValueLocal :: Map Text (LocalVarIdx, Type') -> Text -> Either String LValue
resolveLValueLocal lvarMap name = do
    let maybeLocal = uncurry LValueLocal <$> Map.lookup name lvarMap
    maybe (Left $ "not found: " ++ Text.unpack name) return maybeLocal

resolveReference :: Map Text (FunctionIdx, FunctionType) -> Map Text (GlobalVarIdx, Type') -> Map Text (LocalVarIdx, Type') -> Text -> Either String Reference
resolveReference funcMap gvarMap lvarMap name = do
    let maybeLocal = uncurry ReferenceLocal <$> Map.lookup name lvarMap
        maybeGlobal = uncurry ReferenceGlobal <$> Map.lookup name gvarMap
        maybeFunction = uncurry ReferenceFunction <$> Map.lookup name funcMap
    maybe (Left $ "not found: " ++ Text.unpack name) return (maybeLocal `mplus` maybeGlobal `mplus` maybeFunction)

resolveLocalDeclaration :: Map Text (FunctionIdx, FunctionType) -> Map Text Word64 -> Map Text (GlobalVarIdx, Type') -> Map Text (LocalVarIdx, Type') -> P.Declaration -> Either String R.Declaration
resolveLocalDeclaration funcMap strMap gvarMap lvarMap (P.Declaration param initializer) = do
    ini <- sequence $ resolveExpr funcMap strMap gvarMap lvarMap <$> initializer
    let Parameter pname _ = param
    lvalue <- resolveLValueLocal lvarMap pname
    return (R.Declaration param lvalue ini)

resolveStatement :: Map Text (FunctionIdx, FunctionType) -> Map Text Word64 -> Map Text (GlobalVarIdx, Type') -> Map Text (LocalVarIdx, Type') -> P.Statement -> Either String R.Statement
resolveStatement funcMap strMap gvarMap lvarMap (P.StatementIf cond body1 body2) =
    R.StatementIf <$>
        resolveExpr funcMap strMap gvarMap lvarMap cond <*>
        (Vector.fromList <$> mapM (resolveStatement funcMap strMap gvarMap lvarMap) body1) <*>
        (Vector.fromList <$> mapM (resolveStatement funcMap strMap gvarMap lvarMap) body2)
resolveStatement funcMap strMap gvarMap lvarMap (P.StatementFor (Left pre) cond post body) =
    R.StatementFor . Left <$>
        resolveLocalDeclaration funcMap strMap gvarMap lvarMap pre <*>
        resolveExpr funcMap strMap gvarMap lvarMap cond <*>
        resolveExpr funcMap strMap gvarMap lvarMap post <*>
        (Vector.fromList <$> mapM (resolveStatement funcMap strMap gvarMap lvarMap) body)
resolveStatement funcMap strMap gvarMap lvarMap (P.StatementFor (Right pre) cond post body) =
    R.StatementFor <$>
        (Right <$> resolveExpr funcMap strMap gvarMap lvarMap pre) <*>
        resolveExpr funcMap strMap gvarMap lvarMap cond <*>
        resolveExpr funcMap strMap gvarMap lvarMap post <*>
        (Vector.fromList <$> mapM (resolveStatement funcMap strMap gvarMap lvarMap) body)
resolveStatement funcMap strMap gvarMap lvarMap (P.StatementWhile cond body) =
    R.StatementWhile <$>
        resolveExpr funcMap strMap gvarMap lvarMap cond <*>
        (Vector.fromList <$> mapM (resolveStatement funcMap strMap gvarMap lvarMap) body)
resolveStatement funcMap strMap gvarMap lvarMap (P.StatementExpr e) =
    R.StatementExpr <$>
        resolveExpr funcMap strMap gvarMap lvarMap e
resolveStatement funcMap strMap gvarMap lvarMap (P.StatementDecl declaration) = R.StatementDecl <$> resolveLocalDeclaration funcMap strMap gvarMap lvarMap declaration
resolveStatement _ _ _ _ P.StatementBreak = return R.StatementBreak
resolveStatement _ _ _ _ (P.StatementReturn Nothing) =
    return (R.StatementReturn Nothing)
resolveStatement funcMap strMap gvarMap lvarMap (P.StatementReturn (Just e)) =
    R.StatementReturn . Just <$> resolveExpr funcMap strMap gvarMap lvarMap e

pickFunctionDefinitions :: P.Ast -> [P.FunctionDefinition]
pickFunctionDefinitions (P.Ast xs) = mapMaybe isFuncDef xs
    where
    isFuncDef (P.Decl _)    = Nothing
    isFuncDef (P.FuncDef a) = Just a

pickGlobalVars :: P.Ast -> [(GlobalVar, Maybe P.Expr)]
pickGlobalVars (P.Ast xs) = mapMaybe isDecl xs
    where
    isDecl (P.Decl (P.Declaration (Parameter name type_) initializer)) = Just (GlobalVar name type_, initializer)
    isDecl (P.FuncDef _)                                               = Nothing

pickLocalVars :: P.FunctionDefinition -> Vector LocalVar
pickLocalVars (P.FunctionDefinition _ funcType statements) =
    Vector.fromList (paramLocals ++ concatMap pick statements)
    where
    FunctionType params _ = funcType
    paramLocals = map parameterToLocalVar params
    pick (P.StatementIf _ xs ys)                               = concatMap pick (xs ++ ys)
    pick (P.StatementFor (Left (P.Declaration param _)) _ _ _) = [parameterToLocalVar param]
    pick P.StatementFor{}                                      = []
    pick (P.StatementWhile _ xs)                               = concatMap pick xs
    pick (P.StatementExpr _)                                   = []
    pick (P.StatementDecl (P.Declaration param _))             = [parameterToLocalVar param]
    pick (P.StatementReturn _)                                 = []
    pick P.StatementBreak                                      = []
    parameterToLocalVar (Parameter name type_) = LocalVar name type_

pickStringLiterals :: P.Ast -> Vector (Text, Word64)
pickStringLiterals (P.Ast xs) = snd . mapAccumL f 0 . Vector.fromList . nubOrd . catMaybes . concatMap pick $ xs
    where
    f off s =
        let len = textByteSize s :: Word64
        in (off + len, (s, Bits.shift len 32 .|. (off .&. 0xFFFF)))
    textByteSize = fromIntegral . ByteString.length . Text.encodeUtf8

    pick (P.Decl (P.Declaration _ ini))              = maybe [] pickE ini
    pick (P.FuncDef (P.FunctionDefinition _ _ body)) = concatMap pickS body

    pickS (P.StatementIf cond body1 body2) =
        pickE cond ++ concatMap pickS (body1 ++ body2)
    pickS (P.StatementFor (Left (P.Declaration _ ini)) cond post body) =
        maybe [] pickE ini ++ concatMap pickE [cond, post] ++ concatMap pickS body
    pickS (P.StatementFor (Right pre) cond post body) =
        concatMap pickE [pre, cond, post] ++ concatMap pickS body
    pickS (P.StatementWhile cond body)
        = pickE cond ++ concatMap pickS body
    pickS (P.StatementExpr e) = pickE e
    pickS (P.StatementDecl (P.Declaration _ ini)) = maybe [] pickE ini
    pickS (P.StatementReturn e) = maybe [] pickE e
    pickS P.StatementBreak = []

    pickE P.ExprUnary{}               = []
    pickE P.ExprBinary{}              = []
    pickE (P.ExprAssign _ e)          = pickE e
    pickE P.ExprConstant{}            = []
    pickE P.ExprReference{}           = []
    pickE (P.ExprFunctionCall _ args) = concatMap pickE args
    pickE (P.ExprStringLiteral s)     = [Just s]
