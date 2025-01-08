#lang typed/racket
(require typed/rackunit)


;Fully implemented
;Jared Hammett and Garrett Reinhard


(define-type ExprC (U numC idC appC strC lamC boolC ifC))
(struct numC ([n : Real]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct appC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct strC ([str : String]) #:transparent)
(struct ifC ([check : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct lamC ([params : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct boolC ([b : Boolean]) #:transparent)

(define-type Value (U numV strV boolV cloV primV))
(struct numV ([n : Real]) #:transparent)
(struct strV ([str : String]) #:transparent)
(struct boolV ([b : Boolean]) #:transparent)
(struct primV ([op : Symbol]) #:transparent)
(struct cloV ([params : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)

(define-type Env (Listof binding))
(struct binding ([id : Symbol] [value : Value]) #:transparent)

(define topenv (list
                (binding '+ (primV '+))
                (binding '- (primV '-))
                (binding '/ (primV '/))
                (binding '* (primV '*))
                (binding '<= (primV '<=))
                (binding 'equal? (primV 'equal?))
                (binding 'true (boolV #t))
                (binding 'false (boolV #f))
                (binding 'error (primV 'error))))


;takes in any Value and serializes it
(define (serialize [val : Value]) : String
  (match val
    [(? cloV?) "#<procedure>"]
    [(? primV?) "#<primop>"]
    [(numV n) (~v n)]
    [(strV str) (~v str)]
    [(boolV b) (if b "true" "false")]))

;calls serialize on a list of values
(define (serialhelp [vals : (Listof Value)] [str : String]) : String
  (match vals
    ['() str]
    [(cons f r) (serialhelp r (string-append str (serialize f) "\n"))]))

(check-equal? (serialhelp (list (numV 7) (primV '+)) "") "7\n#<primop>\n")

(check-equal? (serialize (numV 7)) "7")
(check-equal? (serialize (primV '+)) "#<primop>")
(check-equal? (serialize (strV "test")) "\"test\"")
(check-equal? (serialize (strV "test")) "\"test\"")
(check-equal? (serialize (boolV #t)) "true")
(check-equal? (serialize (boolV #f)) "false")
(check-equal? (serialize (cloV '(test) (numC 7) topenv)) "#<procedure>")

;takes in an environment a parameter and its corresponding value and adds
;the binding to the env
(define (extendenv [env : Env] [param : Symbol] [val : Value]) : Env
  (cons (binding param val) env))

;applies extendenv to every param and val in a given list
(define (extendhelper [env : Env] [params : (Listof Symbol)] [vals : (Listof Value)]) : Env
  (match params
    ['() env]
    [other (extendhelper (extendenv env (first params) (first vals)) (rest params) (rest vals))]))

(check-equal? (extendhelper topenv '(a b) (list (numV 7) (numV 8)))
              (list
               (binding 'b (numV 8))
               (binding 'a (numV 7))
               (binding '+ (primV '+))
               (binding '- (primV '-))
               (binding '/ (primV '/))
               (binding '* (primV '*))
               (binding '<= (primV '<=))
               (binding 'equal? (primV 'equal?))
               (binding 'true (boolV #t))
               (binding 'false (boolV #f))
               (binding 'error (primV 'error))))

(define testenv (list
               (binding 'x (numV 7))
               (binding '+ (primV '+))
               (binding '- (primV '-))
               (binding '/ (primV '/))
               (binding '* (primV '*))
               (binding '<= (primV '<=))
               (binding 'equal? (primV 'equal?))
               (binding 'true (boolV #t))
               (binding 'false (boolV #f))
               (binding 'error (primV 'error))))

(check-equal? (extendenv topenv 'x (numV 7)) testenv)


;takes in an environment and returns true if a symbol is in the env
(define (get [env : Env] [sym : Symbol]) : Value
  (match env
    ['() (error 'get "AAQZ: variable undefined, given : ~e" sym)]
    [(cons val rest)
     (if (equal? (binding-id val) sym) (binding-value val) (get rest sym))]))

(check-exn (regexp (regexp-quote "get")) (lambda () (get topenv 'test)))
(check-equal? (get topenv '+) (primV '+))


;takes in a symbol from a primV and a list of arguments and performs
;the primitive operation on the arguments
(define (primsolve [op : Symbol] [args : (Listof Value)]) : Value
  (match op
    ['+ (cond
                 [(equal? (length args) 2)
                  (define a (first args))
                  (define b (second args))
                  (cond
                    [(and (numV? a) (numV? b)) (numV (+ (numV-n a) (numV-n b)))]
                    [else (error 'badargs "AAQZ : Invalid arguments for operation ~e given : ~e" op args)])]
                 [else (error 'numargs "AAQZ : expected 2 arguments given ~e" (length args))])]
    ['- (cond
                 [(equal? (length args) 2)
                  (define a (first args))
                  (define b (second args))
                  (cond
                    [(and (numV? a) (numV? b)) (numV (- (numV-n a) (numV-n b)))]
                    [else (error 'badargs "AAQZ : Invalid arguments for operation ~e given : ~e" op args)])]
                 [else (error 'numargs "AAQZ : expected 2 arguments given ~e" (length args))])]
    ['* (cond
                 [(equal? (length args) 2)
                  (define a (first args))
                  (define b (second args))
                  (cond
                    [(and (numV? a) (numV? b)) (numV (* (numV-n a) (numV-n b)))]
                    [else (error 'badargs "AAQZ : Invalid arguments for operation ~e given : ~e" op args)])]
                 [else (error 'numargs "AAQZ : expected 2 arguments given ~e" (length args))])]
    ['/ (cond
                 [(equal? (length args) 2)
                  (define a (first args))
                  (define b (second args))
                  (cond
                    [(and (numV? a) (numV? b))
                     (if (equal? (numV-n b) 0) (error 'dividebyzero "AAQZ : Cannot divide by zero given : ~e" b)
                         (numV (/ (numV-n a) (numV-n b))))]
                    [else (error 'badargs "AAQZ : Invalid arguments for operation ~e given : ~e" op args)])]
                 [else (error 'numargs "AAQZ : expected 2 arguments given ~e" (length args))])]
    ['<= (cond
                 [(equal? (length args) 2)
                  (define a (first args))
                  (define b (second args))
                  (cond
                    [(and (numV? a) (numV? b)) (boolV (<= (numV-n a) (numV-n b)))]
                    [else (error 'badargs "AAQZ : Invalid arguments for operation ~e given : ~e" op args)])]
                 [else (error 'numargs "AAQZ : expected 2 arguments given ~e" (length args))])]
    ['equal? (cond
                 [(equal? (length args) 2)
                  (define a (first args))
                  (define b (second args))
                  (cond
                    [(or (primV? a) (primV? b)) (boolV #f)]
                    [(or (cloV? a) (cloV? b)) (boolV #f)]
                    [else (boolV (equal? a b))])]
                 [else (error 'numargs "AAQZ : expected 2 arguments given ~e" (length args))])]
    ['error (error 'usersignal "user-error : ~e" (serialhelp args ""))]))


(check-equal? (primsolve '+ (list (numV 1) (numV 1))) (numV 2))
(check-equal? (primsolve '- (list (numV 1) (numV 1))) (numV 0))
(check-equal? (primsolve '* (list (numV 1) (numV 1))) (numV 1))
(check-equal? (primsolve '/ (list (numV 1) (numV 1))) (numV 1))
(check-equal? (primsolve '<= (list (numV 1) (numV 1))) (boolV #t))
(check-equal? (primsolve '<= (list (numV 3) (numV 1))) (boolV #f))
(check-equal? (primsolve '<= (list (numV 3) (numV 4))) (boolV #t))
(check-equal? (primsolve 'equal? (list (numV 4) (numV 4))) (boolV #t))
(check-equal? (primsolve 'equal? (list (numV 3) (primV '+))) (boolV #f))
(check-equal? (primsolve 'equal? (list (numV 3) (cloV '() (numC 7) topenv))) (boolV #f))
(check-exn (regexp (regexp-quote "badargs")) (lambda () (primsolve '+ (list (numV 7) (boolV #t)))))
(check-exn (regexp (regexp-quote "badargs")) (lambda () (primsolve '- (list (numV 7) (boolV #t)))))
(check-exn (regexp (regexp-quote "badargs")) (lambda () (primsolve '* (list (numV 7) (boolV #t)))))
(check-exn (regexp (regexp-quote "badargs")) (lambda () (primsolve '/ (list (numV 7) (boolV #t)))))
(check-exn (regexp (regexp-quote "badargs")) (lambda () (primsolve '<= (list (numV 7) (boolV #t)))))
(check-exn (regexp (regexp-quote "numargs")) (lambda () (primsolve '+ (list (numV 7)))))
(check-exn (regexp (regexp-quote "numargs")) (lambda () (primsolve '- (list (numV 7)))))
(check-exn (regexp (regexp-quote "numargs")) (lambda () (primsolve '* (list (numV 7)))))
(check-exn (regexp (regexp-quote "numargs")) (lambda () (primsolve '/ (list (numV 7)))))
(check-exn (regexp (regexp-quote "numargs")) (lambda () (primsolve '<= (list (numV 7)))))
(check-exn (regexp (regexp-quote "numargs")) (lambda () (primsolve 'equal? (list (numV 7)))))
(check-exn (regexp (regexp-quote "dividebyzero")) (lambda () (primsolve '/ (list (numV 7) (numV 0)))))
(check-exn (regexp (regexp-quote "usersignal")) (lambda () (primsolve 'error (list (numV 7)))))

;applies interp to a list of ExprC
(define (interphelper [lst : (Listof ExprC)] [env : Env]) : (Listof Value)
  (match lst
    ['() '()]
    [(cons f r) (cons (interp f env) (interphelper r env))]))

;takes in an ExprC and an Env and returns a Value
(define (interp [exp : ExprC] [env : Env]) : Value
  (match exp
    [(numC n) (numV n)]
    [(strC str) (strV str)]
    [(idC s) (get env s)]
    [(boolC b) (boolV b)]
    [(lamC params body) (cloV params body env)]
    [(ifC cond then else)
     (define interpcond (interp cond env))
     (if (boolV? interpcond) (if (boolV-b interpcond) (interp then env) (interp else env))
         (error 'badif "AAQZ : if condition not a boolean value, given : ~e" interpcond))]
    [(appC f a)
     (define funval (interp f env))
     (define argvals (interphelper a env))
     (match funval
       [(cloV params body cenv)
        (cond
          [(equal? (length params) (length argvals))
           (define newenv (extendhelper cenv params argvals))
        (interp body newenv)]
          [else (error 'numargs "AAQZ : expected ~e arguments, given ~e" (length params) (length argvals))])]
       [(primV op) (primsolve op argvals)]
       [else (error 'invalidfunc "AAQZ : invalid function definition, given ~e" funval)])]))

(check-equal? (interphelper (list (numC 7) (numC 7)) topenv) (list (numV 7) (numV 7)))

(check-equal? (interp (numC 1) topenv) (numV 1))
(check-equal? (interp (boolC #t) topenv) (boolV #t))
(check-equal? (interp (strC "test") topenv) (strV "test"))
(check-equal? (interp (idC 'x) testenv) (numV 7))
(check-equal? (interp (lamC '(a) (idC 'a)) topenv) (cloV '(a) (idC 'a) topenv))
(check-equal? (interp (appC (idC '+) (list (numC 7) (numC 7))) topenv) (numV 14))
(check-equal? (interp (appC (lamC '(x) (idC 'x))(list (numC 7))) topenv) (numV 7))
(check-equal? (interp (appC (lamC '(x y) (appC (idC '+) (list (idC 'x) (idC 'y))))
                            (list (numC 7) (numC 8))) topenv) (numV 15))
(check-equal? (interp (appC (lamC '(x y) (appC (idC '+) (list (numC 1) (numC 1))))
                            (list (numC 7) (numC 8))) topenv) (numV 2))
(check-equal? (interp (appC (lamC '(x y z) (appC (idC '*) (list (idC 'z)
                            (appC (idC '+) (list (idC 'x) (idC 'y))))))
                            (list (numC 7) (numC 8) (numC 2))) topenv) (numV 30))
(check-equal? (interp (appC (idC 'equal?) (list (boolC #t)
                                                (appC (idC '<=) (list (numC 3) (numC 4))))) topenv) (boolV #t))
(check-equal? (interp (ifC (boolC #t) (numC 1) (numC 0)) testenv) (numV 1))
(check-equal? (interp (ifC (boolC #f) (numC 1) (numC 0)) testenv) (numV 0))
(check-exn (regexp (regexp-quote "badif")) (lambda () (interp (ifC (numC 1) (numC 1) (numC 1)) topenv)))
(check-exn (regexp (regexp-quote "invalidfunc")) (lambda () (interp (appC (numC 1) (list (numC 1) (numC 1))) topenv)))

;returns true if an id is in the list of invalids
(define (invalidid? [id : Symbol] [invalids : (Listof Symbol)]) : Boolean
  (match invalids
    ['() #f]
    [(cons f r) (if (equal? id f) #t (invalidid? id r))]))

(check-equal? (invalidid? 'if '(if bind)) #t)
(check-equal? (invalidid? 'ab '(if bind)) #f)

;parses the concrete syntax to the abstract syntax
(define (parse [sexpr : Sexp]) : ExprC
  (define invalids '(if => = bind))
  (match sexpr
    [(list (? list? params) '=> body)
         (cond
           [(equal? #f (check-duplicates params)) (parse-lambda params body)]
           [else (error 'dupparams "AAQZ : function contains duplicate params, given ~e" params)])]
    [(? real? n) (numC n)]
    [(? symbol? s)
     (define isinvalid? (invalidid? s invalids))
     (cond
       [isinvalid? (error 'invalidid "AAQZ : Cannot use ~e as id" s)]
       [(equal? 'true s) (boolC #t)]
       [(equal? 'false s) (boolC #f)]
       [else (idC s)])]
    [(? string? s) (strC s)]
    [(list 'bind (list (? symbol? s) '= exps) ... body)
     (cond
       [(not (equal? (check-duplicates s) #f)) (error 'bind "AAQZ : variable redefined, given ~e" sexpr)]
       [else (appC (lamC (cast s (Listof Symbol))
                         (parse body)) (map parse (cast exps (Listof Sexp))))])]
    [(list 'if clauses ...)
     (cond
       [(equal? (length clauses) 3)(define parsed-cond (parse (first clauses)))
        (ifC parsed-cond (parse (second clauses)) (parse (third clauses)))]
       [else (error 'if "AAQZ : invalid if structure, expected : if {cond} {then} {else}, given : ~e" sexpr)])]
    [(list fun args ...) (appC (parse fun) (map parse args))]))


(define (parse-lambda [params : Sexp] [body : Sexp]) : ExprC
  (let ([parsed-params (match params
                        ;; Proper list of symbols
                        [(list param ...)
                         (map (lambda (x)
                                (match x
                                  [(? symbol? s) s]
                                  [other (error 'lambda
                                                "AAQZ : Invalid lambda expression, given : ~e" params)]))
                              param)])])
    (lamC parsed-params (parse body))))

;; Test parsing

(check-equal? (parse '(((a b) => (+ a b)) 7 9))
              (appC (lamC '(a b) (appC (idC '+) (list (idC 'a) (idC 'b)))) (list (numC 7) (numC 9))))
(check-equal? (parse 7) (numC 7))
(check-equal? (parse '"bruh") (strC "bruh"))
(check-equal? (parse '"world") (strC "world"))
(check-equal? (parse '((x) => (+ x 1)))
              (lamC (list 'x) (appC (idC '+) (list (idC 'x) (numC 1)))))
(check-equal? (parse '((x) => (<= x 1)))
              (lamC (list 'x) (appC (idC '<=) (list (idC 'x) (numC 1)))))
(check-equal? (parse '((x y) => (* x y)))
              (lamC (list 'x 'y) (appC (idC '*) (list (idC 'x) (idC 'y)))))
(check-equal? (parse '((a b c) => (concat a b c)))
              (lamC (list 'a 'b 'c) (appC (idC 'concat) (list (idC 'a) (idC 'b) (idC 'c)))))
(check-equal? (parse '(((x) => (+ x 1)) 2))
              (appC (lamC (list 'x) (appC (idC '+) (list (idC 'x) (numC 1))))
                     (list (numC 2))))
(check-equal? (parse '(if true 1 0)) (ifC (boolC #t) (numC 1) (numC 0)))
(check-equal? (parse '(if false "yes" "no")) (ifC (boolC #f) (strC "yes") (strC "no")))
(check-equal? (parse '(if (< 1 2) "true" "false"))
              (ifC (appC (idC '<) (list (numC 1) (numC 2))) (strC "true") (strC "false")))
(check-equal? (parse '{bind
                       [x = 1]
                       [y = 2]
                       [+ x y]})
              (appC (lamC '(x y) (appC (idC '+) (list (idC 'x) (idC 'y)))) (list (numC 1) (numC 2))))

(check-exn (regexp (regexp-quote "bind")) (lambda () (parse '(bind (z = (() => 3)) (z = 9) (z)))))
(check-exn (regexp (regexp-quote "invalidid")) (lambda () (parse '(+ if 1))))
(check-exn (regexp (regexp-quote "if")) (lambda () (parse '(if true 7))))

(check-equal? (parse '(+ 1 2))
              (appC (idC '+) (list (numC 1) (numC 2))))
(check-equal? (parse '(((x) => (+ x 1)) 2))
              (appC (lamC (list 'x) (appC (idC '+) (list (idC 'x) (numC 1))))
                     (list (numC 2))))
(check-exn (regexp (regexp-quote "lambda"))
           (lambda () (parse '((1 2) => (+ 1 2)))))

(check-exn (regexp (regexp-quote "dupparams"))
           (lambda () (parse '((x x) => 1))))

(check-exn (regexp (regexp-quote "lambda"))
           (lambda () (parse '((1 7) => (+ x y)))))
(check-exn (regexp (regexp-quote "lambda")) (lambda () (parse '((1) => x))))

;takes in an Sexp and parses, interps it, and serializes it
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) topenv)))


(check-equal? (top-interp '(+ 1 6)) "7")
(check-equal? (top-interp 7) "7")
(check-equal? (top-interp '(((x) => x) 7)) "7")
(check-equal? (top-interp '((x) => x)) "#<procedure>")
(check-exn (regexp (regexp-quote "numargs")) (lambda () (top-interp '((() => 7) 10))))










