module Pipi where

{-
Define a type Program to represent a small imperative programming language with the following constructs:

<program> ::= <stmt-list>

<stmt-list> ::= (<stmt> ';')+

<stmt> ::= <id> '=' <expr>
        | 'if' <expr> '{' <stmt-list> '}' 'else' '{' <stmt-list> '}'
        | 'while' <expr> '{' <stmt-list> '}'
        | 'print' <expr>

<expr> ::= <id>
        | <digit>+
        | <expr> '+' <expr>
        | <expr> '-' <expr>
-}

type Program = [Stmt]

data Stmt
    = Assign String Expr
    | If Expr [Stmt] [Stmt]
    | While Expr [Stmt]
    | Print Expr
    deriving Show

data Expr
    = Ident String
    | Number Int
    | Add Expr Expr
    | Sub Expr Expr
    deriving Show


