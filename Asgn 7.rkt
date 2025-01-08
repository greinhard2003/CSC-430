#lang typed/racket
(require typed/rackunit)

(define-type ExprC (U numC idC appC strC lamC boolC ifC recC))
(struct numC ([n : Real]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct appC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct strC ([str : String]) #:transparent)
(struct ifC ([check : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct lamC ([params : (Listof clause)] [body : ExprC]) #:transparent)
(struct boolC ([b : Boolean]) #:transparent)
(struct recC ([clause : rec-clause] [body : ExprC]) #:transparent)


(struct clause ([id : Symbol] [ty : Ty]) #:transparent)
(struct rec-clause ([id : Symbol] [ty : Ty] [expr : ExprC]) #:transparent)

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

(define-type TEnv (Listof Tbinding))
(struct Tbinding ([id : Symbol] [type : Ty]) #:transparent)

(define toptenv (list
                 (Tbinding '+ (arrow (list (num) (num)) (num)))
                 (Tbinding '- (arrow (list (num) (num)) (num)))
                 (Tbinding '* (arrow (list (num) (num)) (num)))
                 (Tbinding '/ (arrow (list (num) (num)) (num)))
                 (Tbinding '<= (arrow (list (num) (num)) (bool)))
                 (Tbinding 'num-eq? (arrow (list (num) (num)) (bool)))
                 (Tbinding 'str-eq? (arrow (list (str) (str)) (bool)))))

(define base-tenv (list
                   (Tbinding '+ (arrow (list (num) (num)) (num)))
                   (Tbinding '- (arrow (list (num) (num)) (num)))
                   (Tbinding '* (arrow (list (num) (num)) (num)))
                   (Tbinding '/ (arrow (list (num) (num)) (num)))
                   (Tbinding '<= (arrow (list (num) (num)) (bool)))
                   (Tbinding 'num-eq? (arrow (list (num) (num)) (bool)))
                   (Tbinding 'str-eq? (arrow (list (str) (str)) (bool)))))

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
                (binding 'false 8)
                (binding 'num-eq? 9)
                (binding 'str-eq? 10)))


;takes in an Sexp and parses, interps it, and serializes it
(define (top-interp [s : Sexp]) : String
  (define sto (make-vector 2000))
  (vector-fill! sto (numV 0))
  (vector-set! sto 0 (numV 11))
  (vector-set! sto 1 (primV '+))
  (vector-set! sto 2 (primV '-))
  (vector-set! sto 3 (primV '/))
  (vector-set! sto 4 (primV '*))
  (vector-set! sto 5 (primV '<=))
  (vector-set! sto 6 (primV 'equal?))
  (vector-set! sto 7 (boolV #t))
  (vector-set! sto 8 (boolV #f))
  (vector-set! sto 9 (primV 'num-eq?))
  (vector-set! sto 10 (primV 'str-eq?))
  (define parsed (parse s))
  (type-check parsed base-tenv)
  (serialize (interp parsed topenv sto)))


;takes in an ExprC and an Env and returns a Value
(define (interp [exp : ExprC] [env : Env] [sto : (Mutable-Vectorof Any)]) : Value
  (match exp
    [(numC n) (numV n)]
    [(strC str) (strV str)]
    [(idC s)
     (define address (get env s))
     (cast (vector-ref sto address) Value)]
    [(boolC b) (boolV b)]
    [(lamC clause body)
     (define ids (map (lambda (x) (clause-id x)) clause))
     (cloV ids body env)]
    [(ifC cond then else)
     (define interpcond (interp cond env sto))
     (if (boolV? interpcond) (if (boolV-b interpcond) (interp then env sto) (interp else env sto))
         (error 'badif "AAQZ : if condition not a boolean value, given : ~e" interpcond))]
    [(appC f a)
     (define funval (interp f env sto))
     (define argvals (interphelper a env sto))
     (match funval
       [(cloV params body cenv)
        ;need to allocate the amount of space needed in the store, 
        (define newenv (extendhelper cenv params argvals sto))
        (interp body newenv sto)]
       [(primV op) (primsolve op argvals sto)]
       [else (error 'invalidfunc "AAQZ : invalid function definition, given ~e" funval)])]
    [(recC rec-clause body)
     (define id (rec-clause-id rec-clause))
     (define ty (rec-clause-ty rec-clause))
     (define rec-expr (rec-clause-expr rec-clause))
     (define dummy-index (alloc (numV 0) sto)) ; Temporary placeholder
     (define extended-env (extendenv env id dummy-index))
     (define rec-val (interp rec-expr extended-env sto))
     (vector-set! sto dummy-index rec-val)
     (interp body extended-env sto)]))

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
    ['num-eq? (cond
                [(equal? (length args) 2)
                 (define a (first args))
                 (define b (second args))
                 (cond
                   [(and (numV? a) (numV? b)) (boolV (= (numV-n a) (numV-n b)))]
                   [else (error 'badargs "AAQZ : Invalid arguments for operation ~e given : ~e" 'num-eq? args)])]
                [else (error 'numargs "AAQZ : expected 2 arguments given ~e" (length args))])]
    ['str-eq? (cond
             [(equal? (length args) 2)
              (define a (first args))
              (define b (second args))
              (cond
                [(and (strV? a) (strV? b)) (boolV (string=? (strV-str a) (strV-str b)))]
                [else (error 'badargs "AAQZ : Invalid arguments for operation ~e given : ~e" 'str-eq? args)])]
             [else (error 'numargs "AAQZ : expected 2 arguments given ~e" (length args))])]
    ['error (error 'usersignal "user-error : ~e" (serialhelp args ""))]))

(define (checkids [ids : (Listof Symbol)] [invalids : (Listof Symbol)]) : Boolean
  (match ids
    ['() #f]
    [(cons f r) (or (invalidid? f invalids) (checkids r invalids))]))

;parse-type takes in a Sexp and returns the return type
(define (parse-type [exp : Sexp]) : Ty
  (match exp
    [(list single) (parse-type single)]
    [(list (? list? args) '-> ret)
     (define argtys (map parse-type args))
     (arrow
      argtys
      (parse-type ret))]
    [(list args ... '-> ret)
     (arrow (map parse-type (cast args (Listof Sexp))) (parse-type ret))]
    ['num (num)]
    ['str (str)]
    ['bool (bool)]
    [else (error 'parse-type "AAQZ: Invalid type expression ~e" exp)]))

;parses the concrete syntax to the abstract syntax
(define (parse [sexpr : Sexp]) : ExprC
  (define invalids '(if => : = bind))
  (cond
    [(empty? sexpr) (error 'empty "AAQZ: Empty input provided to parse")]
    [else
     (match sexpr
       [(list (list (list (? symbol? s) ': t) ...) '=> body)
        (define clauses (map (lambda (id ty) (clause (cast id Symbol) (parse-type (cast ty Sexp)))) s t))
        (define ids (map (lambda (c) (clause-id c)) clauses))
        (cond
          [(checkids ids invalids) (error 'invalidid "AAQZ : found invalid id, given ~e" ids)]
          [(not (equal? (check-duplicates ids) #f))
           (error 'dupparams "AAQZ : function contains duplicate params, given ~e" ids)]
          [else (lamC clauses (parse body))])]
       [(? real? n) (numC n)]
       [(? symbol? s)
        (define isinvalid? (invalidid? s invalids))
        (cond
          [isinvalid? (error 'invalidid "AAQZ : Cannot use ~e as id" s)]
          [(equal? 'true s) (boolC #t)]
          [(equal? 'false s) (boolC #f)]
          [else (idC s)])]
       [(? string? s) (strC s)]
       [(list 'bind (list (? symbol? s) ': t '= exps) ... body)
        (define clauses (map (lambda (id ty) (clause (cast id Symbol) (parse-type (cast ty Sexp)))) s t))
        (define ids (map (lambda (c) (clause-id c)) clauses))
        (cond
          [(not (equal? (check-duplicates s) #f)) (error 'bind "AAQZ : variable redefined, given ~e" sexpr)]
          [(checkids ids invalids) (error 'invalidid "AAQZ : found invalid id, given ~e" s)]
          [else (appC (lamC clauses
                            (parse body)) (map parse (cast exps (Listof Sexp))))])]
       [(list 'recbind (list (? symbol? id) ': ty '= exp) body)
        (recC (rec-clause (cast id Symbol)
                          (parse-type ty)
                          (parse exp))
              (parse body))]
       [(list 'if condition then else)
        (define parsed-cond (parse condition))
        (ifC parsed-cond (parse then) (parse else))]
       [(list fun args ...) (appC (parse fun) (map parse args))])]))

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

(define (type-check [e : ExprC] [tenv : TEnv]) : Ty
  (match e
    [(numC _) (num)]
    [(strC _) (str)]
    [(boolC _) (bool)]
    [(idC s)
     (define binding (find-type tenv s))
     (if binding
         (Tbinding-type binding)
         (error 'type-check "AAQZ: Unbound variable ~e" s))]
    [(ifC c t e)
     (define ctype (type-check c tenv))
     (define ttype (type-check t tenv))
     (define etype (type-check e tenv))
     (cond
       [(equal? (bool) ctype)
        (cond
          [(equal? etype ttype) ttype]
          [else (error 'type-check "AAQZ: then and else must be same type given ~e, ~e" ttype etype)])]
       [else (error 'type-check "AAQZ: Condition not boolean, given ~e" ctype)])]
    [(lamC clauses body)
     (define param-types
       (map (lambda (c) (clause-ty c))
            clauses))
     (define new-tenv (extend-tenv tenv clauses))
     (arrow param-types (type-check body new-tenv))]
    [(appC fun args)
     (define fun-ty (type-check fun tenv))
     (match fun-ty
       [(arrow param-types ret-ty)
        (when (not (equal? (length param-types) (length args)))
          (error 'type-check "AAQZ: Argument count mismatch: expected ~a, got ~a"
                 (length param-types) (length args)))
        (for ([arg args] [param-ty param-types])
          (define arg-ty (type-check arg tenv))
          (when (not (equal? arg-ty param-ty))
            (error 'type-check "AAQZ: Argument type mismatch: expected ~e, got ~e"
                   param-ty arg-ty)))
        ret-ty]
       [else (error 'type-check "AAQZ: Attempting to apply non-function ~e" fun-ty)])]

    [(recC rec-clause body)
     (define rec-id (rec-clause-id rec-clause))
     (define rec-ty (rec-clause-ty rec-clause))
     (define rec-expr (rec-clause-expr rec-clause))
     (define rec-tenv (extend-tenv tenv (list (clause rec-id rec-ty))))
     (define expr-ty (type-check rec-expr rec-tenv))
     (if (equal? expr-ty rec-ty) (type-check body rec-tenv) (error
                                                             'type-check
                                                             "AAQZ : Type mismatch, given ~e , ~e" rec-ty expr-ty))]))

(define (find-type [tenv : TEnv] [sym : Symbol]) : (U Tbinding #f)
  (for/or ([binding tenv])
    (if (equal? (Tbinding-id binding) sym) binding #f)))

(define (extend-tenv [tenv : TEnv] [bindings : (Listof clause)]) : TEnv
  (append (map (lambda ([b : clause])
                 (Tbinding (clause-id b) (clause-ty b)))
               bindings)
          tenv))

;primsolve tests
(check-exn
 (regexp "AAQZ : expected 2 arguments given")
 (lambda () (primsolve '+ (list (numV 1)) topsto)))
(check-exn
 (regexp "AAQZ : expected 2 arguments given")
 (lambda () (primsolve '- (list (numV 1)) topsto)))
(check-exn
 (regexp "AAQZ : expected 2 arguments given")
 (lambda () (primsolve '* (list (numV 1)) topsto)))
(check-exn
 (regexp "AAQZ : expected 2 arguments given")
 (lambda () (primsolve '/ (list (numV 1)) topsto)))
(check-exn
 (regexp "AAQZ : expected 2 arguments given")
 (lambda () (primsolve '<= (list (numV 1)) topsto)))
(check-exn
 (regexp "AAQZ : expected 2 arguments given")
 (lambda () (primsolve 'equal? (list (numV 1)) topsto)))
(check-equal? (primsolve 'num-eq? (list (numV 1) (numV 1)) topsto) (boolV #t))
(check-equal? (primsolve 'num-eq? (list (numV 1) (numV 2)) topsto) (boolV #f))
(check-exn (regexp "AAQZ : expected 2 arguments given")
           (lambda () (primsolve 'num-eq? (list (numV 1)) topsto)))
(check-exn (regexp "AAQZ : expected 2 arguments given")
           (lambda () (primsolve 'num-eq? (list (numV 1) (numV 2) (numV 3)) topsto)))
(check-exn (regexp "AAQZ : Invalid arguments for operation")
           (lambda () (primsolve 'num-eq? (list (numV 1) (strV "test")) topsto)))
(check-exn (regexp "AAQZ : Invalid arguments for operation")
           (lambda () (primsolve 'num-eq? (list (strV "test") (strV "test")) topsto)))
(check-equal? (primsolve 'str-eq? (list (strV "hello") (strV "hello")) topsto) (boolV #t))
(check-equal? (primsolve 'str-eq? (list (strV "hello") (strV "world")) topsto) (boolV #f))
(check-exn (regexp "AAQZ : expected 2 arguments given")
           (lambda () (primsolve 'str-eq? (list (strV "hello")) topsto)))
(check-exn (regexp "AAQZ : expected 2 arguments given")
           (lambda () (primsolve 'str-eq? (list (strV "hello") (strV "world") (strV "test")) topsto)))
(check-exn (regexp "AAQZ : Invalid arguments for operation")
           (lambda () (primsolve 'str-eq? (list (numV 1) (strV "test")) topsto)))
(check-exn (regexp "AAQZ : Invalid arguments for operation")
           (lambda () (primsolve 'str-eq? (list (numV 1) (numV 2)) topsto)))




; Extra case: More than 2 arguments
(check-exn
 (regexp "AAQZ : expected 2 arguments given")
 (lambda () (primsolve '+ (list (numV 1) (numV 2) (numV 3)) topsto)))

(check-exn
 (regexp "AAQZ : expected 2 arguments given")
 (lambda () (primsolve '- (list (numV 1) (numV 2) (numV 3)) topsto)))

(check-exn
 (regexp "AAQZ : expected 2 arguments given")
 (lambda () (primsolve '* (list (numV 1) (numV 2) (numV 3)) topsto)))

(check-exn
 (regexp "AAQZ : expected 2 arguments given")
 (lambda () (primsolve '/ (list (numV 1) (numV 2) (numV 3)) topsto)))

(check-exn
 (regexp "AAQZ : expected 2 arguments given")
 (lambda () (primsolve '<= (list (numV 1) (numV 2) (numV 3)) topsto)))

(check-exn
 (regexp "AAQZ : expected 2 arguments given")
 (lambda () (primsolve 'equal? (list (numV 1) (numV 2) (numV 3)) topsto)))

;find-type tests
(define test-tenv
  (list (Tbinding 'x (num))
        (Tbinding 'y (str))
        (Tbinding 'z (bool))))

(check-equal? (find-type test-tenv 'x) (Tbinding 'x (num)))
(check-equal? (find-type test-tenv 'y) (Tbinding 'y (str)))
(check-equal? (find-type test-tenv 'z) (Tbinding 'z (bool)))

;extend-tenv tests
(define initial-tenv
  (list (Tbinding 'x (num))
        (Tbinding 'y (str))))
(check-equal? 
 (extend-tenv initial-tenv (list (clause 'z (bool)))) 
 (list (Tbinding 'z (bool)) 
       (Tbinding 'x (num)) 
       (Tbinding 'y (str))))
(check-equal? 
 (extend-tenv initial-tenv (list (clause 'a (num)) (clause 'b (str)))) 
 (list (Tbinding 'a (num)) 
       (Tbinding 'b (str)) 
       (Tbinding 'x (num)) 
       (Tbinding 'y (str))))

;check-type tests


(check-equal? (type-check (numC 5) toptenv) (num))
(check-equal? (type-check (numC -10) toptenv) (num))
(check-equal? (type-check (boolC #t) toptenv) (bool))
(check-equal? (type-check (boolC #f) toptenv) (bool)) 
(check-equal? (type-check (strC "hello") toptenv) (str))
(check-equal? (type-check (strC "") toptenv) (str)) 
(define custom-tenv (extend-tenv toptenv (list (clause 'x (num)) (clause 'y (bool)))))
(check-equal? (type-check (idC 'x) custom-tenv) (num)) 
(check-equal? (type-check (idC 'y) custom-tenv) (bool)) 
(check-exn (regexp "AAQZ: Unbound variable") 
           (lambda () (type-check (idC 'z) custom-tenv)))
(check-equal? (type-check (appC (idC '+) (list (numC 3) (numC 4))) toptenv) (num))
(check-equal? (type-check (appC (idC '<=) (list (numC 5) (numC 10))) toptenv) (bool))
(check-exn (regexp "AAQZ: Argument type mismatch") 
           (lambda () (type-check (appC (idC '+) (list (strC "hello") (numC 5))) toptenv)))
(check-equal? (type-check (lamC (list (clause 'x (num))) (idC 'x)) 
                          (extend-tenv toptenv (list (clause 'x (num))))) 
              (arrow (list (num)) (num)))
(check-equal? (type-check (lamC (list (clause 'x (num)) (clause 'y (num))) (appC (idC '+) (list (idC 'x) (idC 'y)))) 
                          (extend-tenv toptenv (list (clause 'x (num)) (clause 'y (num))))) 
              (arrow (list (num) (num)) (num)))
(check-equal? (type-check (lamC (list (clause 'x (num)) (clause 'y (num)))
                                (appC (idC '+) (list (idC 'x) (idC 'y)))) toptenv) (arrow (list (num) (num)) (num)))
(check-equal? (type-check (ifC (boolC #t) (numC 1) (numC 2)) toptenv) (num))
(check-equal? (type-check (ifC (boolC #f) (strC "yes") (strC "no")) toptenv) (str))
(check-exn (regexp (regexp-quote "type-check")) (lambda ()
                                                  (type-check (ifC (boolC #t) (numC 8) (strC "test")) toptenv)))
(check-exn (regexp (regexp-quote "type-check")) (lambda ()
                                                  (type-check (ifC (numC 7) (numC 8) (numC 9)) toptenv)))
(check-exn (regexp "AAQZ: Argument count mismatch: expected 2, got 1")
           (lambda () 
             (type-check (appC (lamC (list (clause 'x (num))
                                           (clause 'y (num))) (appC (idC '+) (list (idC 'x) (idC 'y))))
                               (list (numC 5)))
                         (extend-tenv toptenv 
                                      (list (clause 'x (num)) 
                                            (clause 'y (num)))))))
(check-exn (regexp "AAQZ: Argument type mismatch:")
           (lambda ()
             (type-check (appC (lamC (list (clause 'x (num)) (clause 'y (bool))) (idC 'x))
                               (list (numC 5) (strC "hello")))
                         (extend-tenv toptenv 
                                      (list (clause 'x (num)) 
                                            (clause 'y (bool)))))))
(check-exn (regexp "AAQZ: Attempting to apply non-function")
           (lambda ()
             (type-check (appC (numC 5) (list (numC 3)))
                         toptenv)))

(check-equal?
 (top-interp '{recbind [f : {num -> num} = {([x : num]) => {if {<= x 0} 0 {+ x {f {- x 2}}}}}] f})
 "#<procedure>")

(check-exn
 (regexp "type-check")
 (lambda ()
   (type-check
    (recC
     (rec-clause 'f
                 (arrow (list (num)) (bool)) ; Intentional mismatch for testing
                 (lamC (list (clause 'x (num)))
                       (ifC
                        (appC (idC '<=) (list (idC 'x) (numC 0)))
                        (numC 0) ; This will return num instead of bool
                        (appC (idC '+)
                              (list (idC 'x)
                                    (appC (idC 'f)
                                          (list (appC (idC '-)
                                                      (list (idC 'x) (numC 2))))))))))
     (idC 'f))
    base-tenv)))



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

;parse-type tests

(check-equal? (parse-type '(bool)) (bool))
(check-equal? (parse-type '(str)) (str))   
(check-equal? (parse-type '(num -> bool))
              (arrow (list (num)) (bool)))
(check-equal? (parse-type '((num num) -> num)) (arrow (list (num) (num)) (num)))
(check-exn (regexp "AAQZ") (lambda () (parse-type '(invalid-type))))

 

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
(check-exn (regexp (regexp-quote "dividebyzero")) (lambda () (primsolve '/ (list (numV 7) (numV 0)) topsto)))
(check-exn (regexp (regexp-quote "usersignal")) (lambda () (primsolve 'error (list (numV 7)) topsto)))

(check-equal? (interp (idC 'x) testenv2 teststo2) (numV 7))

(check-equal? (interp (numC 1) topenv topsto) (numV 1))
(check-equal? (interp (boolC #t) topenv topsto) (boolV #t))
(check-equal? (interp (strC "test") topenv topsto) (strV "test"))

(check-equal? (interp (lamC (list (clause 'a (num))) (idC 'a)) topenv topsto) (cloV '(a) (idC 'a) topenv))
(check-equal? (interp (appC (idC '+) (list (numC 7) (numC 7))) topenv topsto) (numV 14))
(check-equal? (interp (appC (lamC (list (clause 'x (num))) (idC 'x))(list (numC 7))) topenv topsto) (numV 7))
(check-equal? (interp (appC (lamC (list (clause 'x (num)) (clause 'y (num))) (appC (idC '+) (list (idC 'x) (idC 'y))))
                            (list (numC 7) (numC 8))) topenv topsto) (numV 15))
(check-equal? (interp (appC (lamC (list (clause 'x (num)) (clause 'y (num))) (appC (idC '+) (list (numC 1) (numC 1))))
                            (list (numC 7) (numC 8))) topenv topsto) (numV 2))
(check-equal? (interp (appC (lamC (list (clause 'x (num))
                                        (clause 'y (num)) (clause 'z (num))) (appC (idC '*)
                                                                                   (list (idC 'z)                                                                                   
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


(check-equal? (parse '((({a : num} {b : num}) => (+ a b)) 7 9))
              (appC (lamC (list (clause 'a (num)) (clause 'b (num)))
                          (appC (idC '+) (list (idC 'a) (idC 'b)))) (list (numC 7) (numC 9))))
(check-equal? (parse 7) (numC 7))
(check-equal? (parse '"bruh") (strC "bruh"))
(check-equal? (parse '"world") (strC "world"))
(check-equal? (parse '(([x : num]) => (+ x 1)))
              (lamC (list (clause 'x (num))) (appC (idC '+) (list (idC 'x) (numC 1)))))
(check-equal? (parse '(([x : num]) => (<= x 1)))
              (lamC (list (clause 'x (num))) (appC (idC '<=) (list (idC 'x) (numC 1)))))
(check-equal? (parse '(({x : num} {y : num}) => (* x y)))
              (lamC (list (clause 'x (num)) (clause 'y (num))) (appC (idC '*) (list (idC 'x) (idC 'y)))))
(check-equal? (parse '(({a : str} {b : str} {c : str}) => (concat a b c)))
              (lamC (list (clause 'a (str)) (clause 'b (str)) (clause 'c (str)))
                    (appC (idC 'concat) (list (idC 'a) (idC 'b) (idC 'c)))))
(check-equal? (parse '((([x : num]) => (+ x 1)) 2))
              (appC (lamC (list (clause 'x (num))) (appC (idC '+) (list (idC 'x) (numC 1))))
                    (list (numC 2))))
(check-equal? (parse '(if true 1 0)) (ifC (boolC #t) (numC 1) (numC 0)))
(check-equal? (parse '(if false "yes" "no")) (ifC (boolC #f) (strC "yes") (strC "no")))
(check-equal? (parse '(if (< 1 2) "true" "false"))
              (ifC (appC (idC '<) (list (numC 1) (numC 2))) (strC "true") (strC "false")))
(check-equal? (parse '{bind
                       [x : num = 1]
                       [y : num = 2]
                       [+ x y]})
              (appC (lamC (list (clause 'x (num)) (clause 'y (num))) (appC (idC '+) (list (idC 'x) (idC 'y))))
                    (list (numC 1) (numC 2)))) 

(check-exn (regexp (regexp-quote "bind")) (lambda () (parse '(bind (z = (() => 3)) (z = 9) (z)))))
(check-exn (regexp (regexp-quote "invalidid")) (lambda () (parse '(+ if 1))))
(check-exn (regexp (regexp-quote "if")) (lambda () (parse '(if true 7))))
(check-exn (regexp (regexp-quote "invalidid")) (lambda () (parse '{{[: : num]} => 7})))
(check-exn (regexp (regexp-quote "bind")) (lambda () (parse '{bind
                                                              [x : num = 7]
                                                              [x : num = 8]
                                                              7})))
(check-exn (regexp (regexp-quote "invalidid")) (lambda () (parse '{bind
                                                                   [: : num = 7]
                                                                   [x : num = 8]
                                                                   7})))


(check-equal? (parse '(+ 1 2))
              (appC (idC '+) (list (numC 1) (numC 2))))
(check-equal? (parse '((([x : num]) => (+ x 1)) 2))
              (appC (lamC (list (clause 'x (num))) (appC (idC '+) (list (idC 'x) (numC 1))))
                    (list (numC 2))))


(check-exn (regexp (regexp-quote "dupparams"))
           (lambda () (parse '(({x : num} {x : num}) => 1))))




(check-equal? (top-interp '(+ 1 6)) "7")
(check-equal? (top-interp 7) "7")
(check-equal? (top-interp '((([x : num]) => x) 7)) "7")
(check-equal? (top-interp '{{[x : num]} => x}) "#<procedure>")
(check-exn (regexp (regexp-quote "type-check")) (lambda () (top-interp '((() => 7) 10))))

(check-exn (regexp (regexp-quote "type-check")) (lambda ()
                                                  (type-check (parse '(recbind
                                                                       (a : (num -> num)
                                                                          = (((c : num))
                                                                             => "abc")) (a 4))) base-tenv)))

