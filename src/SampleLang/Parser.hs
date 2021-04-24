{-# LANGUAGE OverloadedStrings #-}
module SampleLang.Parser
    ( expr
    ) where

import Control.Monad (void)
import Data.Char (isAlpha)
import Data.Foldable (foldl')
import Data.Text (Text)
import qualified Data.Text as Text (pack)
import SampleLang.Ast
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as Parser (Parsec, between, choice, many,
                                            satisfy)
import qualified Text.Megaparsec.Char as Char (alphaNumChar, space)
import qualified Text.Megaparsec.Char.Lexer as Lexer (decimal, float, lexeme,
                                                      signed, symbol)

type Parser = Parser.Parsec String Text

symbol :: Text -> Parser ()
symbol = void . Lexer.symbol Char.space

parens = Parser.between (symbol "(") (symbol ")")

addSub :: Parser Expr
addSub = do
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
mulDiv = do
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
    negate <|>
    not <|>
    increment <|>
    decrement <|>
    postfix
    where
    negate = symbol "-" *> (ExprUnary . Negate <$> expr)
    not = symbol "!" *> (ExprUnary . Not <$> expr)
    increment = symbol "++" *> (ExprUnary . Increment <$> expr)
    decrement = symbol "--" *> (ExprUnary . Decrement <$> expr)

postfix :: Parser Expr
postfix =
    primary

primary :: Parser Expr
primary =
    constant <|>
    --functionCall <|>
    reference <|>
    parens expr

constant :: Parser Expr
constant =
    (ExprConstant . ConstInt <$> integer) <|>
    (ExprConstant . ConstBool <$> bool) <|>
    (ExprConstant . ConstDouble <$> double)

integer :: Parser Integer
integer =
    Lexer.signed Char.space (Lexer.lexeme Char.space Lexer.decimal) <|>
    Lexer.lexeme Char.space Lexer.decimal

bool :: Parser Bool
bool = symbol "true" *> return True <|> symbol "false" *> return False

double :: Parser Double
double = Lexer.float

reference :: Parser Expr
reference = do
    x <- alphabet
    xs <- Parser.many Char.alphaNumChar
    let ident = Text.pack (x : xs)
    return (ExprReference ident)

alphabet :: Parser Char
alphabet = Parser.satisfy isAlpha

expr :: Parser Expr
expr =
    -- todo
    assignment

assignment :: Parser Expr
assignment =
    -- todo
    relational

relational :: Parser Expr
relational =
    -- todo
    addSub

--statement :: Parser Statement

--block :: Parser [Statement]

--functionDefinition :: Parser FunctionDefinition

--program :: Parser Program
