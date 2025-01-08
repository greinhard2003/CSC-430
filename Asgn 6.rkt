#lang typed/racket
(require typed/rackunit)


;Fully Implemented

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

(define-type Ty (U num str bool arrow call))
(struct num () #:transparent)
(struct str () #:transparent)
(struct bool () #:transparent)
(struct arrow ([args : (Listof Ty)] [return : Ty])#:transparent)
(struct call ([fun : Ty] [args : (Listof Ty)])#:transparent)

(define-type Env (Listof binding))
(struct binding ([id : Symbol] [address : Integer]) #:transparent)

(define topsto (make-vector 2000))
(vector-fill! topsto (numV 0))
(vector-set! topsto 0 (numV 9))
(vector-set! topsto 1 (primV '+))
(vector-set! topsto 2 (primV '-))
(vector-set! topsto 3 (primV '/))
(vector-set! topsto 4 (primV '*))
(vector-set! topsto 5 (primV '<=))
(vector-set! topsto 6 (primV 'equal?))
(vector-set! topsto 7 (boolV #t))
(vector-set! topsto 8 (boolV #f))




(define topenv (list
                (binding '+ 1)
                (binding '- 2)
                (binding '/ 3)
                (binding '* 4)
                (binding '<= 5)
                (binding 'equal? 6)
                (binding 'true 7)
                (binding 'false 8)))


;takes in an Sexp and parses, interps it, and serializes it
(define (top-interp [s : Sexp]) : String
  (define sto (make-vector 2000))
  (vector-fill! sto (numV 0))
  (vector-set! sto 0 (numV 9))
  (vector-set! sto 1 (primV '+))
  (vector-set! sto 2 (primV '-))
  (vector-set! sto 3 (primV '/))
  (vector-set! sto 4 (primV '*))
  (vector-set! sto 5 (primV '<=))
  (vector-set! sto 6 (primV 'equal?))
  (vector-set! sto 7 (boolV #t))
  (vector-set! sto 8 (boolV #f))

  (serialize (interp (parse s) topenv sto)))


;takes in an ExprC and an Env and returns a Value
(define (interp [exp : ExprC] [env : Env] [sto : (Mutable-Vectorof Any)]) : Value
  (match exp
    [(numC n) (numV n)]
    [(strC str) (strV str)]
    [(idC s)
     (define address (get env s))
     (cast (vector-ref sto address) Value)]
    [(boolC b) (boolV b)]
    [(lamC params body) (cloV params body env)]
    [(ifC cond then else)
     (define interpcond (interp cond env sto))
     (if (boolV? interpcond) (if (boolV-b interpcond) (interp then env sto) (interp else env sto))
         (error 'badif "AAQZ : if condition not a boolean value, given : ~e" interpcond))]
    [(appC f a)
     (define funval (interp f env sto))
     (define argvals (interphelper a env sto))
     (match funval
       [(cloV params body cenv)
        (cond
          [(equal? (length params) (length argvals))
           ;need to allocate the amount of space needed in the store, 
           (define newenv (extendhelper cenv params argvals sto))
           (interp body newenv sto)]
          [else (error 'numargs "AAQZ : expected ~e arguments, given ~e" (length params) (length argvals))])]
       [(primV op) (primsolve op argvals sto)]
       [else (error 'invalidfunc "AAQZ : invalid function definition, given ~e" funval)])]))

;takes in a symbol from a primV and a list of arguments and performs
;the primitive operation on the arguments
(define (primsolve [op : Symbol] [args : (Listof Value)] [sto : (Mutable-Vectorof Any)]) : Value
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

;parses the concrete syntax to the abstract syntax
(define (parse [sexpr : Sexp]) : ExprC
  (define invalids '(if => = bind))
  (cond
    [(empty? sexpr) (error 'empty "AAQZ: Empty input provided to parse")]
    [else
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
       [(list fun args ...) (appC (parse fun) (map parse args))])]))


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

;returns true if an id is in the list of invalids
(define (invalidid? [id : Symbol] [invalids : (Listof Symbol)]) : Boolean
  (match invalids
    ['() #f]
    [(cons f r) (if (equal? id f) #t (invalidid? id r))]))

;applies interp to a list of ExprC
(define (interphelper [lst : (Listof ExprC)] [env : Env] [sto : (Mutable-Vectorof Any)]) : (Listof Value)
  (match lst
    ['() '()]
    [(cons f r) (cons (interp f env sto) (interphelper r env sto))]))

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



;takes in a value and store and updates the store to contain the value
;also updates the tracking value in index 0 to next open slot
;returns the allocated index that was originally stored in 0
(define (alloc [val : Value] [sto : (Mutable-Vectorof Any)]) : Integer
  (define allocated (vector-ref sto 0))
  (match allocated
    [(numV n) (vector-set! sto 0 (numV (+ n 1)))
              (vector-set! sto (cast n Integer) val)
              (cast n Integer)]
    [other (error 'alloc "Memory counter corrupted")]))



;takes in an environment a parameter and its corresponding value and adds
;the binding to the env
(define (extendenv [env : Env] [param : Symbol] [add : Integer]) : Env
  (cons (binding param add) env))

;applies extendenv to every param and val in a given list
(define (extendhelper [env : Env] [params : (Listof Symbol)] [argvals : (Listof Value)]
                      [sto : (Mutable-Vectorof Any)]) : Env
  (match params
    ['() env]
    [other (extendhelper (extendenv env (first params) (alloc (first argvals) sto)) (rest params) (rest argvals) sto)]))



;takes in an environment and returns true if a symbol is in the env
(define (get [env : Env] [sym : Symbol]) : Integer
  (match env
    ['() (error 'get "AAQZ: variable undefined, given : ~e" sym)]
    [(cons val rest)
     (if (equal? (binding-id val) sym) (binding-address val) (get rest sym))]))

(define (parse-type [exp : Sexp]) : Ty
  (match exp
    [(? real? n) (num)]
    [(? symbol? s)
     (match s
       ['true (bool)]
       ['false (bool)]
       [other (str)])]))






;TEST CASES
(define testenv (list
                 (binding '+ 1)
                 (binding '- 2)
                 (binding '/ 3)
                 (binding '* 4)
                 (binding '<= 5)
                 (binding 'equal? 6)
                 (binding 'true 7)
                 (binding 'false 8)))

(define testenv2 (list
                  (binding 'x 10)
                  (binding '+ 1)
                  (binding '- 2)
                  (binding '/ 3)
                  (binding '* 4)
                  (binding '<= 5)
                  (binding 'equal? 6)
                  (binding 'true 7)
                  (binding 'false 8)))

(define teststo2 (make-vector 15))
(vector-fill! teststo2 (numV 0))
(vector-set! teststo2 0 (numV 11))
(vector-set! teststo2 1 (primV '+))
(vector-set! teststo2 2 (primV '-))
(vector-set! teststo2 3 (primV '/))
(vector-set! teststo2 4 (primV '*))
(vector-set! teststo2 5 (primV '<=))
(vector-set! teststo2 6 (primV 'equal?))
(vector-set! teststo2 7 (boolV #t))
(vector-set! teststo2 8 (boolV #f))
(vector-set! teststo2 10 (numV 7))



(check-equal? (extendenv testenv 'x 10) testenv2)

;Function is working properly, doesnt reflect store values because
;it would force a bunch of other test cases to change 
(check-equal? (extendhelper testenv '(a b) (list (numV 10) (numV 11)) topsto)
              (list
               (binding 'b 10)
               (binding 'a 9)
               (binding '+ 1)
               (binding '- 2)
               (binding '/ 3)
               (binding '* 4)
               (binding '<= 5)
               (binding 'equal? 6)
               (binding 'true 7)
               (binding 'false 8)))

(define teststo (make-vector 10))

(vector-set! teststo 0 (numV 1))

(check-equal? (alloc (numV 1000) teststo) 1)
(check-equal? (vector-ref teststo 1) (numV 1000))
(check-equal? (alloc (numV 2000) teststo) 2)

(vector-set! teststo 0 'NotNumV)
(check-exn (regexp (regexp-quote "alloc"))(lambda () (alloc (numV 1) teststo)))



(check-exn (regexp (regexp-quote "get")) (lambda () (get topenv 'test)))
(check-equal? (get topenv '+) 1)

(check-equal? (serialhelp (list (numV 7) (primV '+)) "") "7\n#<primop>\n")
(check-equal? (serialize (numV 7)) "7")

(check-equal? (serialize (primV '+)) "#<primop>")
(check-equal? (serialize (strV "test")) "\"test\"")
(check-equal? (serialize (strV "test")) "\"test\"")
(check-equal? (serialize (boolV #t)) "true")
(check-equal? (serialize (boolV #f)) "false")
(check-equal? (serialize (cloV '(test) (numC 7) topenv)) "#<procedure>")



(check-equal? (primsolve '+ (list (numV 1) (numV 1)) topsto) (numV 2))
(check-equal? (primsolve '- (list (numV 1) (numV 1)) topsto) (numV 0))
(check-equal? (primsolve '* (list (numV 1) (numV 1)) topsto) (numV 1))
(check-equal? (primsolve '/ (list (numV 1) (numV 1)) topsto) (numV 1))
(check-equal? (primsolve '<= (list (numV 1) (numV 1)) topsto) (boolV #t))
(check-equal? (primsolve '<= (list (numV 3) (numV 1)) topsto) (boolV #f))
(check-equal? (primsolve '<= (list (numV 3) (numV 4)) topsto) (boolV #t))
(check-equal? (primsolve 'equal? (list (numV 4) (numV 4)) topsto) (boolV #t))
(check-equal? (primsolve 'equal? (list (numV 3) (primV '+)) topsto) (boolV #f))



(check-equal? (primsolve 'equal? (list (numV 3) (cloV '() (numC 7) topenv)) topsto) (boolV #f))
(check-exn (regexp (regexp-quote "badargs")) (lambda () (primsolve '+ (list (numV 7) (boolV #t)) topsto)))
(check-exn (regexp (regexp-quote "badargs")) (lambda () (primsolve '- (list (numV 7) (boolV #t)) topsto)))
(check-exn (regexp (regexp-quote "badargs")) (lambda () (primsolve '* (list (numV 7) (boolV #t)) topsto)))
(check-exn (regexp (regexp-quote "badargs")) (lambda () (primsolve '/ (list (numV 7) (boolV #t)) topsto)))
(check-exn (regexp (regexp-quote "badargs")) (lambda () (primsolve '<= (list (numV 7) (boolV #t)) topsto)))
(check-exn (regexp (regexp-quote "numargs")) (lambda () (primsolve '+ (list (numV 7)) topsto)))
(check-exn (regexp (regexp-quote "numargs")) (lambda () (primsolve '- (list (numV 7)) topsto)))
(check-exn (regexp (regexp-quote "numargs")) (lambda () (primsolve '* (list (numV 7)) topsto)))
(check-exn (regexp (regexp-quote "numargs")) (lambda () (primsolve '/ (list (numV 7)) topsto)))
(check-exn (regexp (regexp-quote "numargs")) (lambda () (primsolve '<= (list (numV 7)) topsto)))
(check-exn (regexp (regexp-quote "numargs")) (lambda () (primsolve 'equal? (list (numV 7)) topsto)))
(check-exn (regexp (regexp-quote "dividebyzero")) (lambda () (primsolve '/ (list (numV 7) (numV 0)) topsto)))
(check-exn (regexp (regexp-quote "usersignal")) (lambda () (primsolve 'error (list (numV 7)) topsto)))

(check-equal? (interp (idC 'x) testenv2 teststo2) (numV 7))

(check-equal? (interp (numC 1) topenv topsto) (numV 1))
(check-equal? (interp (boolC #t) topenv topsto) (boolV #t))
(check-equal? (interp (strC "test") topenv topsto) (strV "test"))

(check-equal? (interp (lamC '(a) (idC 'a)) topenv topsto) (cloV '(a) (idC 'a) topenv))
(check-equal? (interp (appC (idC '+) (list (numC 7) (numC 7))) topenv topsto) (numV 14))
(check-equal? (interp (appC (lamC '(x) (idC 'x))(list (numC 7))) topenv topsto) (numV 7))
(check-equal? (interp (appC (lamC '(x y) (appC (idC '+) (list (idC 'x) (idC 'y))))
                            (list (numC 7) (numC 8))) topenv topsto) (numV 15))
(check-equal? (interp (appC (lamC '(x y) (appC (idC '+) (list (numC 1) (numC 1))))
                            (list (numC 7) (numC 8))) topenv topsto) (numV 2))
(check-equal? (interp (appC (lamC '(x y z) (appC (idC '*) (list (idC 'z)
                                                                (appC (idC '+) (list (idC 'x) (idC 'y))))))
                            (list (numC 7) (numC 8) (numC 2))) topenv topsto) (numV 30))
(check-equal? (interp (appC (idC 'equal?) (list (boolC #t)
                                                (appC (idC '<=) (list (numC 3) (numC 4))))) topenv topsto) (boolV #t))
(check-equal? (interp (ifC (boolC #t) (numC 1) (numC 0)) testenv topsto) (numV 1))
(check-equal? (interp (ifC (boolC #f) (numC 1) (numC 0)) testenv topsto) (numV 0))
(check-exn (regexp (regexp-quote "badif")) (lambda () (interp (ifC (numC 1) (numC 1) (numC 1)) topenv topsto)))
(check-exn (regexp (regexp-quote "invalidfunc")) (lambda () (interp (appC (numC 1)
                                                                          (list (numC 1) (numC 1))) topenv topsto)))

(check-equal? (invalidid? 'if '(if bind)) #t)
(check-equal? (invalidid? 'ab '(if bind)) #f)


(check-exn (regexp (regexp-quote "empty")) (lambda () (parse '())))


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


(check-equal? (top-interp '(+ 1 6)) "7")
(check-equal? (top-interp 7) "7")
(check-equal? (top-interp '(((x) => x) 7)) "7")
(check-equal? (top-interp '((x) => x)) "#<procedure>")
(check-exn (regexp (regexp-quote "numargs")) (lambda () (top-interp '((() => 7) 10))))






