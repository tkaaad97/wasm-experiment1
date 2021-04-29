{-# LANGUAGE OverloadedStrings #-}
module SampleLang.Parser
    ( expr
    , statement
    ) where

import Control.Applicative ()
import Control.Monad (void)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text (pack)
import Data.Word (Word64)
import SampleLang.Ast.Parsed
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as Parser (Parsec, between, choice, many,
                                            satisfy, try)
import qualified Text.Megaparsec.Char as Char (alphaNumChar, space)
import qualified Text.Megaparsec.Char.Lexer as Lexer (decimal, float, lexeme,
                                                      symbol)

type Parser = Parser.Parsec String Text

symbol :: Text -> Parser ()
symbol = void . Lexer.symbol Char.space

parens = Parser.between (symbol "(") (symbol ")")

braces = Parser.between (symbol "{") (symbol "}")

addSub :: Parser Expr
addSub = Lexer.lexeme Char.space $ do
    e <- mulDiv
    xs <- Parser.many $ Parser.choice [add, sub]
    return $ foldl' (\a f -> ExprBinary (f a)) e xs
    where
    add = do
        symbol "+"
        e <- mulDiv
        return (`Add` e)
    sub = do
        symbol "-"
        e <- mulDiv
        return (`Sub` e)

mulDiv :: Parser Expr
mulDiv = Lexer.lexeme Char.space $ do
    e <- unary
    xs <- Parser.many $ Parser.choice [mul, div_]
    return $ foldl' (\a f -> ExprBinary (f a)) e xs
    where
    mul = do
        symbol "*"
        e <- unary
        return (`Mul` e)
    div_ = do
        symbol "/"
        e <- unary
        return (`Div` e)

unary :: Parser Expr
unary =
    Lexer.lexeme Char.space $
    Parser.try negate <|>
    Parser.try not <|>
    Parser.try increment <|>
    Parser.try decrement <|>
    postfix
    where
    negate = symbol "-" *> (ExprUnary . Negate <$> expr)
    not = symbol "!" *> (ExprUnary . Not <$> expr)
    increment = symbol "++" *> (ExprUnary . Increment <$> expr)
    decrement = symbol "--" *> (ExprUnary . Decrement <$> expr)

postfix :: Parser Expr
postfix =
    Lexer.lexeme Char.space $
    primary

primary :: Parser Expr
primary =
    Lexer.lexeme Char.space $
    Parser.try constant <|>
    --functionCall <|>
    Parser.try reference <|>
    parens expr

constant :: Parser Expr
constant =
    Lexer.lexeme Char.space $
    Parser.try (ExprConstant . ConstInt <$> integer) <|>
    Parser.try (ExprConstant . ConstBool <$> bool) <|>
    (ExprConstant . ConstDouble <$> double)

integer :: Parser Word64
integer =
    Lexer.lexeme Char.space Lexer.decimal

bool :: Parser Bool
bool = symbol "true" $> True <|> symbol "false" $> False

double :: Parser Double
double = Lexer.float

reference :: Parser Expr
reference = ExprReference <$> identifier

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
    Lexer.lexeme Char.space $
    -- todo
    equality

equality :: Parser Expr
equality =
    Lexer.lexeme Char.space $
    Parser.try equ <|> relational
    where
    equ = do
        a <- relational
        op <- Parser.choice
            [ symbol "==" $> Equ
            , symbol "/=" $> Neq]
        b <- relational
        return (ExprRelational (op a b))

relational :: Parser Expr
relational =
    Lexer.lexeme Char.space $
    Parser.try rel <|> addSub
    where
    rel = do
        a <- addSub
        op <- Parser.choice
            [ symbol "<" $> Lt
            , symbol "<=" $> Le
            , symbol ">" $> Gt
            , symbol ">=" $> Ge]
        b <- addSub
        return (ExprRelational (op a b))

primitiveType :: Parser Type'
primitiveType =
    Lexer.lexeme Char.space $
    Parser.try uintType <|>
    Parser.try intType <|>
    Parser.try boolType <|>
    doubleType
    where
    uintType = symbol "unsigned" >> symbol "int" >> return TypeUInt
    intType = symbol "int" $> TypeInt
    boolType = symbol "bool" $> TypeBool
    doubleType = symbol "double" $> TypeDouble

declaration :: Parser Parameter
declaration =
    Lexer.lexeme Char.space $ do
    t <- primitiveType
    name <- identifier
    -- todo function type
    -- todo initializeder
    return (Parameter name t)

statement :: Parser Statement
statement =
    Lexer.lexeme Char.space $
    Parser.try ifStatement <|>
--    forStatement <|>
--    whileStatement <|>
    Parser.try declarationStatement <|>
    Parser.try exprStatement <|>
    returnStatement
    where
    ifStatement = do
        symbol "if"
        cond <- parens expr
        body <- braces (Parser.many statement)
        return (StatementIf cond body)
    declarationStatement =
        StatementDecl <$> declaration <* symbol ";"
    exprStatement =
        StatementExpr <$> expr <* symbol ";"
    returnStatement =
        symbol "return" *> (StatementReturn <$> expr) <* symbol ";"

functionDefinition :: Parser FunctionDefinition
functionDefinition =
    Lexer.lexeme Char.space $ do
    returnType <- primitiveType
    name <- identifier
    params <- parens (Parser.many declaration)
    body <- braces (Parser.many statement)
    let funcType = FunctionType params returnType
    return (FunctionDefinition name funcType body)

--program :: Parser Program
