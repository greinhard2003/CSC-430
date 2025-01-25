This repo contains various iterations of my own programming language implemented using the Racket language.  
All versions have a parser to generate the abstract syntax and an interpreter to evaluate the abstract syntax.  
The concrete syntax uses rackets symbols to allow for easier parsing.  
Rackets match function made handling various types extremely simple and fast, making it an ideal language to implement my programming language.  

Asgn3:
  - Uses substitution to handle function applications and variable references
  - Supports basic biniary operations and functions
  - Specification:  
    \<fd\> ::= {fundef id {params ...} => Expr}  
    \<Expr\> ::= Num  
    &emsp; | Bool  
    &emsp; | Id  
    &emsp; | Binop  
    &emsp; | {id Expr ...}  
    &emsp; | {ifleq0? Expr Expr Expr}  
     \<Binop\> ::= {+ Expr Expr}  
    &emsp; | {- Expr Expr}  
    &emsp; | {* Expr Expr}  
    &emsp; | {/ Expr Expr}  
    \<Num\> ::= Real  
    \<Id\> ::= Symbol  
    \<Bool\> ::= Boolean  
    
Asgn4: 
  - Supports a set of primitives and uses an environment to handle functions and variable references
  - Primitives: + - * / <= equal? true false error
  - Functions are now created without names and are being used as values instead. Function definitions now in the form of lambda expressions
  - Specification:  
     \<Expr\> ::= Num  
    &emsp; | Bool  
    &emsp; | Id  
    &emsp; | Str  
    &emsp; | {Id ... => Expr}  
    &emsp; | {Expr Expr ...}  
    &emsp; | {if ExprC ExprC ExprC}  
    \<Num\> ::= Real  
    \<Id\> ::= Symbol  
    \<Bool\> ::= Boolean  
    \<Str\> ::= String  

Asgn5:  
  - Uses the same framework as Asgn4 but allows for side effects. Bind and seq can be used to create full programs. Seq executes a list of expressions and returns the last value in the list. Bind can be used for creating variables or defining functions in a way similar to other programming languages.
  - New primitives:
  - println
  - read-num
  - read-str
  - ++
  - Bind
  - Seq

Asgn6:
  -Now uses a store to hold variable values. Supports variable mutation in addition to arrays and basic array functions.
  -Variable mutation operator is :=
  - New primitives:
  - make-array
  - array
  - aref
  - aset!
  - substring

Asgn7: 
  -No longer supports arrays. Now contains a type checking system to prevent from type related errors. Type checking occurs between parsing and interpreting.
  - New Primitives:
  - num-eq?
  - str-eq?
