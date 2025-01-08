This repo contains various iterations of my own programming language implemented using the Racket language.
All versions have a parser to generate the abstract syntax and an interpreter to evaluate the abstract syntax

Asgn3:
  - Uses substitution to handle function applications and variable references
  - Supports basic biniary operations and functions
  - Specification:
    <fd> ::= {fundef id {params ...} => Expr}
    <Expr> ::= Num
            | Bool
            | Id
            | Binop
            | {id Expr ...}
            | {ifleq0? Expr Expr Expr}
    <Binop> ::= {+ Expr Expr}
            | {- Expr Expr}
            | {* Expr Expr}
            | {/ Expr Expr}
    <Num> ::= Real
    <Id> ::= Symbol
    <Bool> ::= Boolean
    
Asgn4: 
  - Supports a set of primitives and uses an environment to handle functions and variable references
  - Primitives: + - * / <= equal? true false error
  - Functions are now created without names and are being used as values instead. Function definitions now in the form of lambda expressions
  - Specification:
     <Expr> ::= Num
            | Bool
            | Id
            | Str
            | {Id ... => Expr}
            | {Expr Expr ...}
            | {if ExprC ExprC ExprC}
    <Num> ::= Real
    <Id> ::= Symbol
    <Bool> ::= Boolean
    <Str> ::= String
