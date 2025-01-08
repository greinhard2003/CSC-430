#lang typed/racket
(require typed/rackunit)


;;Fully implemented
;;Garrett Reinhard & Jared Hammett


(struct fd ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)

(define-type ExprC (U num binop id app ifleq0?))


(struct num ([n : Real]) #:transparent)
(struct binop ([op : Symbol][l : ExprC] [r : ExprC]) #:transparent)
(struct id ([s : Symbol]) #:transparent)
(struct app ([fun : Symbol] [args : (Listof ExprC)]) #:transparent)
(struct ifleq0? ([exp : ExprC] [then : ExprC] [else : ExprC]) #:transparent)

;takes in a symbol representing a binary arithmetic operator and two
;Arith params and returns a Binop with the corresponding operator
(define (mapop [op : Symbol] [l : Real] [r : Real]) : Real
  (match op
    ['add (+  l r)]
    ['sub (- l r)]
    ['mult (* l r)]
    ['div (/ l r)]
    [other (error 'badop "AAQZ : invalid operator, given ~e" exp)]))

(check-equal? (mapop 'add 7 7) 14)
(check-equal? (mapop 'sub 7 7) 0)
(check-equal? (mapop 'mult 7 7) 49)
(check-equal? (mapop 'div 7 7) 1)
(check-exn (regexp (regexp-quote "badop")) (lambda () (mapop 'notanop 7 7)))

;List for testing
(define fdlst (list (fd 'main '() (app 'step (list (num 7))))
                    (fd 'step (list 'x) (binop 'add (id 'x) (num 1)))
                    (fd 'prod (list 'x 'y) (binop 'mult (id 'x) (id 'y)))
                    (fd 'double '(x)  (binop 'mult (id 'x) (id 'x)))
                    (fd 'test '(x) (binop 'add (num 3) (num 4)))
                    (fd 'testifleq0? '(x) (ifleq0? (id 'x) (id 'x) (binop 'sub (id 'x) (num 1))))))


;takes in a symbol and looks up a fundef from a table of fundefs and
;returns the corresponding fundef
(define (get-fd [n : Symbol] [lst : (Listof fd)]) : fd
  (cond
    [(empty? lst) (error 'get-fd "AAQZ : function not found")]
    [(equal? n (fd-name (first lst))) (first lst)]
    [else (get-fd n (rest lst))]))

(check-equal? (get-fd 'test (list (fd 'test (list 'x) (binop 'add (num 7) (num 7)))))
              (fd 'test (list 'x) (binop 'add (num 7) (num 7))))
(check-equal? (get-fd 'test (list (fd 'something (list 'x) (num 0))
                                  (fd 'test (list 'x) (binop 'add (num 7) (num 7)))))
              (fd 'test (list 'x) (binop 'add (num 7) (num 7))))
(check-exn (regexp (regexp-quote "get-fd")) (lambda () (get-fd 'something '())))

(fd 'step '(x) (binop 'add (id 'x) (num 1)))
(fd 'prod '(x y)  (binop 'mult (id 'x) (id 'y)))
(fd 'double '(x)  (binop 'mult (id 'x) (id 'x)))


;takes in what we want to replace the name with, name of what is being substituted
;and the expression we want to do it in
(define (subst [what : ExprC] [var : Symbol] [in : ExprC]) : ExprC
  (match in
    [(num n) in]
    [(id s) (cond [(symbol=? s var) what] [else in])]
    [(app f args) (app f (substhelper what var args))]
    [(binop op l r) (binop op (subst what var l) (subst what var r))]
    [(ifleq0? exp then else) (ifleq0? (subst what var exp)
                                      (subst what var then) (subst what var else))]))


;helper function for subst to help with lists of args
;takes in the what is to be substituted, the var, and the list of args
(define (substhelper [what : ExprC] [var : Symbol] [lst : (Listof ExprC)]) : (Listof ExprC)
  (match lst
    ['() '()]
    [(cons f r) (cons (subst what var f) (substhelper what var r))]))

(check-equal? (substhelper (num 8) 'x (list (id 'x) (id 'y) (id 'z)))
              (list (num 8) (id 'y) (id 'z)))
(check-equal? (substhelper (num 8) 'x '()) '())


(check-equal? (subst (num 8) 'x (app 'test (list (id 'x)))) (app 'test (list (num 8))))
(check-equal? (subst (num 8) 'x (num 7)) (num 7))
(check-equal? (subst (num 8) 'y (app 'test (list (id 'x)))) (app 'test (list (id 'x))))
(check-equal? (subst (num 8) 'x (binop 'add (num 7) (id 'x))) (binop 'add (num 7) (num 8)))
(check-equal? (subst (num 8) 'x (app 'test (list (id 'y) (id 'z) (id 'x))))
              (app 'test (list (id 'y) (id 'z) (num 8))))
(check-equal? (subst (num 8) 'x (app 'test '())) (app 'test '()))
(check-equal? (subst (num 8) 'x (app 'testifleq0? (list (id 'x))))
              (app 'testifleq0? (list (num 8))))


;takes in a list of ExprC : vals, list of Symbols : args and an ExprC : body and calls
;subst on each val in the list 
(define (interphelper [vals : (Listof ExprC)] [args : (Listof Symbol)] [body : ExprC]) : ExprC
  (match vals
    ['() body]
    [(cons f r) (interphelper r (rest args) (subst f (first args) body))]))

(check-equal? (interphelper '() '() (binop 'add (num 7) (num 7)))
              (binop 'add (num 7) (num 7)))
(check-equal? (interphelper (list (num 7)) '(x) (binop 'add (num 7) (id 'x)))
              (binop 'add (num 7) (num 7)))
(check-equal? (interphelper (list (num 7)) '(y) (binop 'add (num 7) (id 'x)))
              (binop 'add (num 7) (id 'x)))


;takes in an ExprC and a list of FunDefC and returns the real values
(define (interp [e : ExprC] [fds : (Listof fd)]) : Real
  (match e
    [(num n) n]
    [(id s) (error 'interp "AAQZ : interp should not reach id : ~e" (id s))]
    [(app f a)
     (define func (get-fd f fds)) ; Get function from list
     (cond
       [(equal? (length a) (length (fd-args func)))
        ; Recursively evaluate with full function list
        ;checkedargs ensures that all arguments are valid arguments
        ;if check-args fails then error is raised inside of interp call
        (define checkedargs (check-args a fds))
        (interp (interphelper a (fd-args func) (fd-body func)) fds)]
       [else (error 'badargs "AAQZ : given : ~e arguments, expected : ~e"
                    (length a) (length (fd-args func)))])]
    [(binop op l r) (cond
                      [(equal? op 'div)
                       (define rinterped (interp r fds))
                       (if (equal? 0 rinterped)
                           (error 'dividebyzero "AAQZ :  cannot divide by 0, given : ~e" exp)
                           (mapop op (interp l fds) rinterped))]
                      [else (mapop op (interp l fds) (interp r fds))])]
    [(ifleq0? exp then else) (if (<= (interp exp fds) 0)
                                 (interp then fds)
                                 (interp else fds))]))

;takes in a list of arguments and fds and returns true if all arguments
;are valid expressions
;interp will signal error if invaid argument is encountered
(define (check-args [args : (Listof ExprC)] [fds : (Listof fd)]) : Boolean
  (match args
    ['() #t]
    [(cons f r) (and (real? (interp f fds)) (check-args r fds))]))

(check-equal? (check-args (list (num 7)) fdlst) #t)
(check-equal? (check-args (list (num 7) (num 8)) fdlst) #t)
(check-exn (regexp (regexp-quote "dividebyzero")) (lambda () (check-args (list (binop 'div (num 1) (num 0))) fdlst)))
(check-exn (regexp (regexp-quote "dividebyzero"))
           (lambda () (interp (app 'test (list (binop 'div (num 1) (num 0)))) fdlst)))


(check-exn (regexp (regexp-quote "dividebyzero")) (lambda () (interp (binop 'div (num 1) (num 0)) fdlst)))
(check-equal? (interp (num 8) fdlst) 8)
(check-equal? (interp (binop 'add (num 7) (num 7)) fdlst) 14)
(check-equal? (interp (binop 'sub (num 7) (num 7)) fdlst) 0)
(check-equal? (interp (binop 'mult (num 7) (num 7)) fdlst) 49)
(check-equal? (interp (binop 'div (num 7) (num 7)) fdlst) 1)
(check-exn (regexp (regexp-quote "interp")) (lambda () (interp (id 'x) fdlst)))
(check-equal? (interp (app 'step (list (num 7))) fdlst) 8)
(check-equal? (interp (app 'prod (list (num 8) (num 9))) fdlst) 72)
(check-exn (regexp (regexp-quote "badargs")) (lambda () (interp (app 'step '()) fdlst)))
(check-exn (regexp (regexp-quote "badargs"))
           (lambda () (interp (app 'step (list (num 8) (num 9))) fdlst)))
(check-equal? (interp (app 'testifleq0? (list (num 7))) fdlst) 6)
(check-equal? (interp (app 'testifleq0? (list (num 0))) fdlst) 0)

;interprets the fd named main
(define (interp-fns [fns : (Listof fd)]) : Real
  (interp (app 'main '()) fns))  ; 'main' starts the evaluation

(check-equal? (interp-fns fdlst) 8)

;takse in a Sexp and parses all arguments of function application
(define (parsehelp [lst : Sexp]) : (Listof ExprC)
  (match lst
    ['() '()]
    [(cons f r) (cons (parse f) (parsehelp r))]))


;Parses the concrete syntax into the abstract syntax
(define (parse [exp : Sexp]) : ExprC
  (match exp
    [(? real? n) (num n)]
    [(? symbol? s)
     (match s
       ['+ (error 'parseinvalidid "AAQZ :cannot use ~e as id, given : ~e" '+ exp)]
       ['- (error 'parseinvalidid "AAQZ :cannot use ~e as id, given : ~e" '- exp)]
       ['* (error 'parseinvalidid "AAQZ :cannot use ~e as id, given : ~e" '* exp)]
       ['/ (error 'parseinvalidid "AAQZ :cannot use ~e as id, given : ~e" '/ exp)]
       ['def (error 'parseinvalidid "AAQZ :cannot use ~e as id, given : ~e" 'def exp)]
       ['ifleq0? (error 'parseinvalidid "AAQZ :cannot use ~e as id, given : ~e" 'ifleq0? exp)]
       ['=> (error 'parseinvalidid "AAQZ :cannot use ~e as id, given : ~e" '=> exp)]
       [other (id s)])]
    [(list '+ l r) (binop 'add (parse l) (parse r))]
    [(list '- l r) (binop 'sub (parse l) (parse r))]
    [(list '* l r) (binop 'mult (parse l) (parse r))]
    [(list '/ l r) (binop 'div (parse l) (parse r))]
    [(list 'ifleq0? exp then else) (ifleq0? (parse exp) (parse then) (parse else))]
    [(list (? symbol? f) r ...)
     (match f
       ['+ (error 'parse "AAQZ :invalid number of arguments, given : ~e" exp)]
       ['- (error 'parse "AAQZ :invalid number of arguments, given : ~e" exp)]
       ['* (error 'parse "AAQZ :invalid number of arguments, given : ~e" exp)]
       ['/ (error 'parse "AAQZ :invalid number of arguments, given : ~e" exp)]
       ['ifleq0? (error 'parse "AAQZ :invalid number of arguments, given : ~e" exp)]
       ['def (error 'parseinvalidid "AAQZ :cannot use ~e as id, given: ~e" 'def exp)]
       ['=> (error 'parseinvalidid "AAQZ :cannot use ~e as id, given : ~e" '=> exp)]
       [other (app f (parsehelp r))])]))
 
(check-exn (regexp (regexp-quote "parse")) (lambda () (parse '(/ 7 7 7))))
(check-exn (regexp (regexp-quote "parse")) (lambda () (parse '(+ 7 7 7))))
(check-exn (regexp (regexp-quote "parse")) (lambda () (parse '(- 7 7 7))))
(check-exn (regexp (regexp-quote "parse")) (lambda () (parse '(* 7 7 7))))
(check-exn (regexp (regexp-quote "parse")) (lambda () (parse '(def 7 7 7))))
(check-exn (regexp (regexp-quote "parseinvalidid")) (lambda () (parse '(+  + 7))))
(check-exn (regexp (regexp-quote "parseinvalidid")) (lambda () (parse '(+  - 7))))
(check-exn (regexp (regexp-quote "parseinvalidid")) (lambda () (parse '(+  * 7))))
(check-exn (regexp (regexp-quote "parseinvalidid")) (lambda () (parse '(+  / 7))))
(check-exn (regexp (regexp-quote "parseinvalidid")) (lambda () (parse '(+  => 7))))
(check-exn (regexp (regexp-quote "parseinvalidid")) (lambda () (parse '(+  def 7))))
(check-exn (regexp (regexp-quote "parseinvalidid")) (lambda () (parse '(+  ifleq0? 7))))
(check-exn (regexp (regexp-quote "parse")) (lambda () (parse '(ifleq0? 7 7 7 7))))
(check-exn (regexp (regexp-quote "parseinvalidid")) (lambda () (parse '(=> 7 7 7))))

;takes in a list of symbols and returns true if a symbol is in the list
(define (contains? [lst : (Listof Symbol)] [sym : Symbol]) : Boolean
  (match lst
    ['() #f]
    [(cons val rest)
     (if (equal? val sym) #t (contains? rest sym))]))

(check-equal? (contains? '(a b c) 'c) #t)
(check-equal? (contains? '(a b c) 'f) #f)

(define invalidids '(+ / - * ifleq0? def =>))

;Takes in an Sexp and returns a parsed fd
(define (parse-fundef [exp : Sexp]) : fd
  (match exp
    [(list 'def (? symbol? funname) (list (? symbol? arg) '=> body))
     (if (contains? invalidids funname)
         (error 'parseinvalidid "AAQZ :cannot use ~e as id, given : ~e" funname exp)
         (fd funname (list arg) (parse body)))]
    [(list 'def (? symbol? funname) (list (list (? symbol? args) ...) '=> body))
     (cond
       [(contains? invalidids funname)
        (error 'parseinvalidid "AAQZ :cannot use ~e as id, given : ~e" funname exp)]
       [(equal? #f (check-duplicates args)) (fd funname (cast args (Listof Symbol)) (parse body))]
       [else (error 'parsedupargs "AAQZ : given duplicate arguments : ~e" args)])]
    [other (error 'parse-fundef "AAQZ : expected '(def g (args => body) got : ~e" exp)]))

(check-exn (regexp (regexp-quote "parse-fundef")) (lambda () (parse-fundef '(def g (() => 13 14)))))
(check-exn (regexp (regexp-quote "parseinvalidid")) (lambda () (parse-fundef '(def def (x => (+ x 1)) ))))
(check-exn (regexp (regexp-quote "parseinvalidid")) (lambda () (parse-fundef '(def + ((x y) => (+ y 1)) ))))
(check-exn (regexp (regexp-quote "parsedupargs")) (lambda () (parse-fundef '(def f ((x x) => (+ y 1)) ))))

(check-equal? (parse-fundef '(def testfunc ((x y z) => {* z {+ x y}})))
              (fd 'testfunc (list 'x 'y 'z) (binop 'mult (id 'z) (binop 'add (id 'x) (id 'y)))))

(check-equal? (parsehelp (list 1 2 3)) (list (num 1) (num 2) (num 3)))

(check-equal? (parse 1) (num 1))
(check-equal? (parse '(+ 1 2)) (binop 'add (num 1) (num 2)))
(check-equal? (parse '(- 1 2)) (binop 'sub (num 1) (num 2)))
(check-equal? (parse '(* 1 2)) (binop 'mult (num 1) (num 2)))
(check-equal? (parse '(/ 1 2)) (binop 'div (num 1) (num 2)))
(check-equal? (parse 'x) (id 'x))
(check-equal? (parse '(f 1)) (app 'f (list (num 1))))
(check-equal? (parse '(f 1 2 3)) (app 'f (list (num 1) (num 2) (num 3))))
(check-equal? (parse '(ifleq0? x x (- x 1))) (ifleq0? (id 'x) (id 'x) (binop 'sub (id 'x) (num 1))))


;Parses a whole program where one function definition is main returns a list of function definitions
(define (parse-prog [sexp : Sexp]) : (Listof fd)
  (match sexp
    ['() '()]
    [(cons l r) (cons (parse-fundef l) (parse-prog r))]))

;Test Cases
(check-equal? (parse-prog '((def add ((x y) => {+ x y}))))
              (list (fd 'add (list 'x 'y) (binop 'add (id 'x) (id 'y)))))
(check-equal? (parse-prog '((def testfunc ((x y z) => {* z {+ x y}}))
                            (def main (() => 7))))
              (list (fd 'testfunc (list 'x 'y 'z) 
                         (binop 'mult (id 'z) 
                                (binop 'add (id 'x) (id 'y))))
                    (fd 'main '() (num 7))))
(check-equal?
  (parse-prog '((def testfunc ((x y z) => {* z {+ x y}}))
                (def main (() => {testfunc 2 3 4}))))
  (list (fd 'testfunc (list 'x 'y 'z) 
             (binop 'mult (id 'z) 
                    (binop 'add (id 'x) (id 'y))))
        (fd 'main '() 
             (app 'testfunc (list (num 2) (num 3) (num 4))))))

;top interp combines our parsing and interpretting function calls into one function
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))


;Test Cases

(check-equal? (top-interp (quote ((def main (() => (+ 1 2)))))) 3)
(check-equal? (top-interp '((def step (x => {+ x 1}))
                            (def double (y => {* 2 y}))
                            (def main (() => (- (step 7) (double 10)))))) -12)
