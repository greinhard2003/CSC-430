This repo contains various iterations of my own programming language implemented using the Racket language.
All versions have a parser to generate the abstract syntax and an interpreter to evaluate the abstract syntax

Asgn3:
  - Uses substitution to handle function applications and variable references
  - Supports basic biniary operations and functions
  - Specification:

    <fd> ::= {fundef id {params ...} => ExprC}
    <ExprC> ::= Num
            | Bool
            | Id
            | Binop
            | {id ExprC ...}
            | {ifleq0? ExprC ExprC ExprC}
    <Binop> ::= {+ ExprC ExprC}
            | {- ExprC ExprC}
            | {* ExprC ExprC}
            | {/ ExprC ExprC}
    <Num> ::= Real
    <Id> ::= Symbol
    
Asgn4: 
   
