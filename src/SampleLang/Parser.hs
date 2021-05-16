{-# LANGUAGE OverloadedStrings #-}
module SampleLang.Parser
    ( expr
    , statement
    , program
    , parseProgram
    ) where

import Control.Monad (void)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as Text (pack)
import Data.Void (Void)
import SampleLang.Ast.Parsed
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as Parser (Parsec, between, choice,
                                            errorBundlePretty, many, manyTill,
                                            option, optional, parse, satisfy,
                                            try)
import qualified Text.Megaparsec.Char as Char (char, space)
import qualified Text.Megaparsec.Char.Lexer as Lexer (charLiteral, decimal,
                                                      float, lexeme, symbol)

type Parser = Parser.Parsec Void Text

symbol :: Text -> Parser ()
symbol = void . Lexer.symbol Char.space

{-# INLINE parens #-}
parens :: Parser a -> Parser a
parens = Parser.between (symbol "(") (symbol ")")

{-# INLINE braces #-}
braces :: Parser a -> Parser a
braces = Parser.between (symbol "{") (symbol "}")

addSub :: Parser Expr
addSub = Lexer.lexeme Char.space $ do
    e <- mulDiv
    xs <- Parser.many $ Parser.choice [add, sub]
    return $ foldl' (\a (op, b) -> ExprBinary op a b) e xs
    where
    add = do
        symbol "+"
        e <- mulDiv
        return (Add, e)
    sub = do
        symbol "-"
        e <- mulDiv
        return (Sub, e)

mulDiv :: Parser Expr
mulDiv = Lexer.lexeme Char.space $ do
    e <- unary
    xs <- Parser.many $ Parser.choice [mul, div_]
    return $ foldl' (\a (op, b) -> ExprBinary op a b) e xs
    where
    mul = do
        symbol "*"
        e <- unary
        return (Mul, e)
    div_ = do
        symbol "/"
        e <- unary
        return (Div, e)

unary :: Parser Expr
unary =
    Lexer.lexeme Char.space $
    Parser.try negate_ <|>
    Parser.try not_ <|>
    Parser.try increment <|>
    Parser.try decrement <|>
    postfix
    where
    negate_ = symbol "-" *> (ExprUnary Negate <$> expr)
    not_ = symbol "!" *> (ExprUnary Not <$> expr)
    increment = symbol "++" *> (ExprUnary Increment <$> expr)
    decrement = symbol "--" *> (ExprUnary Decrement <$> expr)

postfix :: Parser Expr
postfix =
    Lexer.lexeme Char.space
    primary

primary :: Parser Expr
primary =
    Lexer.lexeme Char.space $
    Parser.try constant <|>
    Parser.try referenceOrFunctionCall <|>
    Parser.try stringLiteral <|>
    parens expr

constant :: Parser Expr
constant =
    Lexer.lexeme Char.space $
    Parser.try (ExprConstant . ConstInt <$> integer) <|>
    Parser.try (ExprConstant . ConstBool <$> bool) <|>
    ExprConstant . ConstDouble <$> double

integer :: Parser Int32
integer =
    Lexer.lexeme Char.space Lexer.decimal

bool :: Parser Bool
bool = symbol "true" $> True <|> symbol "false" $> False

double :: Parser Double
double = Lexer.float

referenceOrFunctionCall :: Parser Expr
referenceOrFunctionCall = do
    name <- identifier
    Parser.option (ExprReference name) (functionCall name)
    where
    functionCall name =
        ExprFunctionCall name <$> parens (Parser.try arguments <|> return [])
    arguments = do
        x <- expr
        xs <- Parser.many (symbol "," *> expr)
        return (x : xs)

stringLiteral :: Parser Expr
stringLiteral = do
    str <- Char.char '"' >> Parser.manyTill Lexer.charLiteral (Char.char '"')
    return (ExprStringLiteral . Text.pack $ str)

identifierStartChar :: Parser Char
identifierStartChar = Parser.satisfy (\a -> isAsciiUpper a || isAsciiLower a || a == '_')

identifierChar :: Parser Char
identifierChar = Parser.satisfy (\a -> isAsciiUpper a || isAsciiLower a || isDigit a || a == '_')

identifier :: Parser Text
identifier =
    Lexer.lexeme Char.space $ do
    x <- identifierStartChar
    xs <- Parser.many identifierChar
    return (Text.pack (x : xs))

expr :: Parser Expr
expr =
    Lexer.lexeme Char.space assignment

assignment :: Parser Expr
assignment =
    Lexer.lexeme Char.space $ do
        a <- equality
        Parser.option a (symbol "=" *> (ExprAssign a <$> assignment))

equality :: Parser Expr
equality =
    Lexer.lexeme Char.space $ do
    a <- relational
    Parser.option a $ do
        op <- Parser.choice
            [ symbol "==" $> Equ
            , symbol "/=" $> Neq]
        ExprBinary op a <$> relational

relational :: Parser Expr
relational =
    Lexer.lexeme Char.space $ do
        a <- addSub
        Parser.option a $ do
            op <- Parser.choice
                [ symbol "<" $> Lt
                , symbol "<=" $> Le
                , symbol ">" $> Gt
                , symbol ">=" $> Ge]
            ExprBinary op a <$> addSub

voidType :: Parser Type'
voidType = symbol "void" $> TypeVoid

intType :: Parser Type'
intType = symbol "int" $> TypeInt

boolType :: Parser Type'
boolType = symbol "bool" $> TypeBool

doubleType :: Parser Type'
doubleType = symbol "double" $> TypeDouble

stringType :: Parser Type'
stringType = symbol "string" $> TypeString

primitiveType :: Parser Type'
primitiveType =
    Lexer.lexeme Char.space $
    Parser.try voidType <|>
    Parser.try intType <|>
    Parser.try boolType <|>
    Parser.try doubleType <|>
    stringType

unvoidPrimitiveType :: Parser Type'
unvoidPrimitiveType =
    Lexer.lexeme Char.space $
    Parser.try intType <|>
    Parser.try boolType <|>
    Parser.try doubleType <|>
    stringType

parameter :: Parser Parameter
parameter =
    Lexer.lexeme Char.space $ do
    t <- unvoidPrimitiveType
    name <- identifier
    -- todo function type
    return (Parameter name t)

declarationWithInitializer :: Parser Declaration
declarationWithInitializer =
    Lexer.lexeme Char.space $ do
    t <- unvoidPrimitiveType
    name <- identifier
    initializer <- Parser.optional (symbol "=" *> expr)
    return (Declaration (Parameter name t) initializer)

statement :: Parser Statement
statement =
    Lexer.lexeme Char.space $
    Parser.try ifStatement <|>
    Parser.try forStatement <|>
    Parser.try whileStatement <|>
    Parser.try declarationStatement <|>
    Parser.try exprStatement <|>
    Parser.try breakStatement <|>
    returnStatement
    where
    ifStatement = do
        symbol "if"
        cond <- parens expr
        body1 <- braces (Parser.many statement)
        body2 <- Parser.try elsePart <|> return []
        return (StatementIf cond body1 body2)
    elsePart = do
        symbol "else"
        Parser.try ((:[]) <$> ifStatement) <|> braces (Parser.many statement)
    forStatement = do
        symbol "for"
        symbol "("
        pre <- Left <$> Parser.try declarationWithInitializer <|> Right <$> expr
        symbol ";"
        cond <- expr
        symbol ";"
        post <- expr
        symbol ")"
        body <- braces (Parser.many statement)
        return (StatementFor pre cond post body)
    whileStatement = do
        symbol "while"
        cond <- parens expr
        body <- braces (Parser.many statement)
        return (StatementWhile cond body)
    declarationStatement =
        StatementDecl <$> declarationWithInitializer <* symbol ";"
    exprStatement =
        StatementExpr <$> expr <* symbol ";"
    breakStatement =
        symbol "break" $> StatementBreak <* symbol ";"
    returnStatement =
        symbol "return" *> (StatementReturn <$> Parser.optional expr) <* symbol ";"

functionDeclarationOrDefinition :: Parser DeclOrFuncDef
functionDeclarationOrDefinition =
    Lexer.lexeme Char.space $ do
    returnType <- primitiveType
    name <- identifier
    params <- parens (Parser.try parameters <|> return [])
    maybeBody <- (symbol ";" $> Nothing) <|> (fmap Just . braces $ Parser.many statement)
    let funcType = FunctionType params returnType
    case maybeBody of
        Just body -> return (FuncDef (FunctionDefinition name funcType body))
        Nothing -> return (Decl (Declaration (Parameter name (TypeFunction funcType)) Nothing))
    where
    parameters = do
        x <- parameter
        xs <- Parser.many (symbol "," *> parameter)
        return (x : xs)

program :: Parser Ast
program =
    fmap Ast . Lexer.lexeme Char.space . Parser.many $
        Parser.try globalVarDecl <|> functionDeclarationOrDefinition
    where
    globalVarDecl = Decl <$> (declarationWithInitializer <* symbol ";")

parseProgram :: Text -> Either String Ast
parseProgram input = either (Left . Parser.errorBundlePretty) return $ Parser.parse program "Ast" input
