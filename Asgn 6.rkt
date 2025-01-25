#lang typed/racket
(require typed/rackunit)




(define-type ExprC (U numC idC appC strC lamC boolC ifC mutC nullC))
(struct nullC ([s : Symbol]) #:transparent)
(struct numC ([n : Real]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct appC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct strC ([str : String]) #:transparent)
(struct ifC ([check : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct lamC ([params : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct boolC ([b : Boolean]) #:transparent)
(struct mutC ([id : Symbol] [val : ExprC]) #:transparent)

(define-type Value (U numV strV boolV cloV primV nullV arrayV))
(struct numV ([n : Real]) #:transparent)
(struct strV ([str : String]) #:transparent)
(struct boolV ([b : Boolean]) #:transparent)
(struct primV ([op : Symbol]) #:transparent)
(struct cloV ([params : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct nullV ([null : Symbol]) #:transparent)
(struct arrayV ([start : Integer] [length : Natural]) #:transparent)

(define-type Env (Listof binding))
(struct binding ([id : Symbol] [address : Integer]) #:transparent)

(define topsto (make-vector 200))
(vector-fill! topsto (nullV 'null))
(vector-set! topsto 0 (numV 17))
(vector-set! topsto 1 (primV '+))
(vector-set! topsto 2 (primV '-))
(vector-set! topsto 3 (primV '/))
(vector-set! topsto 4 (primV '*))
(vector-set! topsto 5 (primV '<=))
(vector-set! topsto 6 (primV 'equal?))
(vector-set! topsto 7 (boolV #t))
(vector-set! topsto 8 (boolV #f))
(vector-set! topsto 9 (primV 'error))
(vector-set! topsto 10 (primV 'make-array))
(vector-set! topsto 11 (primV 'array))
(vector-set! topsto 12 (primV 'aref))
(vector-set! topsto 13 (primV 'aset!))
(vector-set! topsto 14 (primV 'substring))
(vector-set! topsto 15 (primV 'seq))
(vector-set! topsto 16 (nullV 'nullV))



(define topenv (list
                (binding '+ 1)
                (binding '- 2)
                (binding '/ 3)
                (binding '* 4)
                (binding '<= 5)
                (binding 'equal? 6)
                (binding 'true 7)
                (binding 'false 8)
                (binding 'error 9)
                (binding 'make-array 10)
                (binding 'array 11)
                (binding 'aref 12)
                (binding 'aset! 13)
                (binding 'substring 14)
                (binding 'seq 15)
                (binding 'null 16)))

;takes in any Value and serializes it
(define (serialize [val : Value]) : String
  (match val
    [(? cloV?) "#<procedure>"]
    [(? primV?) "#<primop>"]
    [(numV n) (~v n)]
    [(strV str) (~v str)]
    [(boolV b) (if b "true" "false")]
    [(nullV null) "null"]
    [(arrayV start len) "#<array>"]))

;calls serialize on a list of values
(define (serialhelp [vals : (Listof Value)] [str : String]) : String
  (match vals
    ['() str]
    [(cons f r) (serialhelp r (string-append str (serialize f) "\n"))]))

(check-equal? (serialhelp (list (numV 7) (primV '+)) "") "7\n#<primop>\n")
(check-equal? (serialize (nullV 'null)) "null")
(check-equal? (serialize (numV 7)) "7")
(check-equal? (serialize (arrayV 0 2)) "#<array>")
(check-equal? (serialize (primV '+)) "#<primop>")
(check-equal? (serialize (strV "test")) "\"test\"")
(check-equal? (serialize (strV "test")) "\"test\"")
(check-equal? (serialize (boolV #t)) "true")
(check-equal? (serialize (boolV #f)) "false")
(check-equal? (serialize (cloV '(test) (numC 7) topenv)) "#<procedure>")

;make-array takes a size, value, and store and creates an array
;of the given size populated with the given value
(define (make-array [size : Natural] [init-val : Value] [sto : (Mutable-Vectorof Any)]) : Value
  (cond
    [(< size 1) (error 'make-array "AAQZ: Array size must be at least 1, given ~e" size)]
    [else
     (define start (alloc init-val sto)) ; Allocate the first slot for the array
     (for ([i (in-range 1 size)]) ; Allocate the remaining slots one by one
       (alloc init-val sto))
     (arrayV start size)]))

(define (array [vals : (Listof Value)] [sto : (Mutable-Vectorof Any)]) : Value
  (cond
    [(empty? vals) (error 'array "AAQZ: Array must contain at least one element.")]
    [else
     (define start (alloc (first vals) sto)) ; Allocate and set the first element
     (for ([val (rest vals)])
       (alloc val sto)) ; Allocate each subsequent element
     (arrayV start (length vals))])) ; Return arrayV with start and length

(define (aref [arr : Value] [index : Integer] [sto : (Mutable-Vectorof Any)]) : Value
  (match arr
    [(arrayV start length)
     (if (or (< index 0) (>= index length))
         (error 'aref "AAQZ: Index out of bounds, given ~e" index)
         (cast (vector-ref sto (+ start index)) Value))] ; Access and cast to Value
    [else (error 'aref "AAQZ: Invalid array reference, given ~e" arr)]))

(define (aset! [arr : Value] [index : Integer] [new-val : Value] [sto : (Mutable-Vectorof Any)]) : Value
  (match arr
    [(arrayV start length)
     (if (or (< index 0) (>= index length))
         (error 'aset! "AAQZ: Index out of bounds, given ~e" index)
         (begin
           (vector-set! sto (+ start index) new-val)
           (nullV 'null)))]
    [else (error 'aset! "AAQZ: Invalid array reference, given ~e" arr)]))

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

(define teststo (make-vector 10))

(vector-set! teststo 0 (numV 1))

(check-equal? (alloc (numV 1000) teststo) 1)
(check-equal? (vector-ref teststo 1) (numV 1000))
(check-equal? (alloc (numV 2000) teststo) 2)

(vector-set! teststo 0 'NotNumV)
(check-exn (regexp (regexp-quote "alloc"))(lambda () (alloc (numV 1) teststo)))

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

(define testenv (list
                 (binding '+ 1)
                 (binding '- 2)
                 (binding '/ 3)
                 (binding '* 4)
                 (binding '<= 5)
                 (binding 'equal? 6)
                 (binding 'true 7)
                 (binding 'false 8)
                 (binding 'error 9)))

(define testenv2 (list
                 (binding 'x 10)
                 (binding '+ 1)
                 (binding '- 2)
                 (binding '/ 3)
                 (binding '* 4)
                 (binding '<= 5)
                 (binding 'equal? 6)
                 (binding 'true 7)
                 (binding 'false 8)
                 (binding 'error 9)))

(define teststo2 (make-vector 15))
(vector-fill! teststo2 (nullV 'null))
(vector-set! teststo2 0 (numV 11))
(vector-set! teststo2 1 (primV '+))
(vector-set! teststo2 2 (primV '-))
(vector-set! teststo2 3 (primV '/))
(vector-set! teststo2 4 (primV '*))
(vector-set! teststo2 5 (primV '<=))
(vector-set! teststo2 6 (primV 'equal?))
(vector-set! teststo2 7 (boolV #t))
(vector-set! teststo2 8 (boolV #f))
(vector-set! teststo2 9 (primV 'error))
(vector-set! teststo2 10 (numV 7))
(vector-set! topsto 11 (primV 'seq))


(check-equal? (extendenv testenv 'x 10) testenv2)

;Function is working properly, doesnt reflect store values because
;it would force a bunch of other test cases to change 
(check-equal? (extendhelper testenv '(a b) (list (numV 10) (numV 11)) topsto)
              (list
               (binding 'b 18)
               (binding 'a 17)
               (binding '+ 1)
               (binding '- 2)
               (binding '/ 3)
               (binding '* 4)
               (binding '<= 5)
               (binding 'equal? 6)
               (binding 'true 7)
               (binding 'false 8)
               (binding 'error 9)))

;takes in an environment and returns true if a symbol is in the env
(define (get [env : Env] [sym : Symbol]) : Integer
  (match env
    ['() (error 'get "AAQZ: variable undefined, given : ~e" sym)]
    [(cons val rest)
     (if (equal? (binding-id val) sym) (binding-address val) (get rest sym))]))

(check-exn (regexp (regexp-quote "get")) (lambda () (get topenv 'test)))
(check-equal? (get topenv '+) 1)


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
    ['seq (last args)]
    ['make-array (cond
                   [(equal? (length args) 2)
                    (define size (first args))
                    (define init-val (second args))
                    (match size
                      [(numV n) (make-array (cast n Nonnegative-Integer) init-val sto)]
                      [other (error 'invalidarg "AAQZ : Expected number got ~e" other)])]
                   [else (error 'numargs "AAQZ : expected 2 arguments given ~e" (length args))])]
    ['array (cond
              [(equal? (length args) 0) (error 'numargs "AAQZ : Expected at least 1 argument, given 0")]
              [else (array args sto)])]
    ['aref (cond
                   [(equal? (length args) 2)
                    (define arr (first args))
                    (define index (second args))
                    (match index
                      [(numV n) (aref arr (cast n Nonnegative-Integer) sto)]
                      [other (error 'invalidarg "AAQZ : Expected number got ~e" other)])]
                   [else (error 'numargs "AAQZ : expected 2 arguments given ~e" (length args))])]
    ['aset! (cond
                   [(equal? (length args) 3)
                    (define arr (first args))
                    (define index (second args))
                    (define new-val (third args))
                    (match index
                      [(numV n) (aset! arr (cast n Nonnegative-Integer) new-val sto)]
                      [other (error 'invalidarg "AAQZ : Expected number got ~e" other)])]
                   [else (error 'numargs "AAQZ : expected 3 arguments given ~e" (length args))])]
    ['substring 
     (cond
       [(equal? (length args) 3)
        (define str (first args))
        (define start (second args))
        (define end (third args))
        (match str
          [(strV s)
           (match start
             [(numV start-index)
              (match end
                [(numV end-index)
                 (define start-int (cast start-index Integer))
                 (define end-int (cast end-index Integer))
                 (if (and (<= 0 start-int) (<= start-int end-int) (<= end-int (string-length s)))
                     (strV (substring s start-int end-int))
                     (error 'substring "AAQZ: Index out of bounds, start: ~e, end: ~e" start-int end-int))]
                [else (error 'substring "AAQZ: End argument is not a number")])]
             [else (error 'substring "AAQZ: Start argument is not a number")])]
          [else (error 'substring "AAQZ: First argument is not a string")])]
       [else (error 'numargs "AAQZ : expected 3 arguments given ~e" (length args))])]
    ['error (error 'usersignal "user-error : ~e" (serialhelp args ""))]))

(check-equal? (primsolve 'substring (list (strV "hello world") (numV 0) (numV 5)) topsto) (strV "hello"))
(check-equal? (primsolve 'substring (list (strV "openai") (numV 1) (numV 4)) topsto) (strV "pen"))
(check-exn (regexp "AAQZ: Index out of bounds") 
           (lambda () (primsolve 'substring (list (strV "hello") (numV -1) (numV 2)) topsto)))
(check-exn (regexp "AAQZ: First argument is not a string") 
           (lambda () (primsolve 'substring (list (numV 1) (numV 0) (numV 2)) topsto)))
(check-exn (regexp (regexp-quote "numargs")) 
           (lambda () (primsolve 'substring (list (strV "hello") (numV 1) (numV 0) (numV 2)) topsto)))
(check-exn (regexp (regexp-quote "substring")) 
           (lambda () (primsolve 'substring (list (strV "hello") (numV 1) (strV "World")) topsto)))
(check-exn (regexp (regexp-quote "substring")) 
           (lambda () (primsolve 'substring (list (strV "hello") (strV "World") (numV 2)) topsto)))

(check-equal? (primsolve 'seq (list (numV 1) (numV 2)) topsto) (numV 2))
(check-equal? (primsolve '+ (list (numV 1) (numV 1)) topsto) (numV 2))
(check-equal? (primsolve '- (list (numV 1) (numV 1)) topsto) (numV 0))
(check-equal? (primsolve '* (list (numV 1) (numV 1)) topsto) (numV 1))
(check-equal? (primsolve '/ (list (numV 1) (numV 1)) topsto) (numV 1))
(check-equal? (primsolve '<= (list (numV 1) (numV 1)) topsto) (boolV #t))
(check-equal? (primsolve '<= (list (numV 3) (numV 1)) topsto) (boolV #f))
(check-equal? (primsolve '<= (list (numV 3) (numV 4)) topsto) (boolV #t))
(check-equal? (primsolve 'equal? (list (numV 4) (numV 4)) topsto) (boolV #t))
(check-equal? (primsolve 'equal? (list (numV 3) (primV '+)) topsto) (boolV #f))
(check-equal? (primsolve 'make-array (list (numV 10) (strV "Hello")) topsto) (arrayV 19 10))
(check-equal? (primsolve 'array (list (numV 1) (numV 2) (numV 3)) topsto) (arrayV 29 3))
(check-equal? (primsolve 'aref (list (arrayV 29 3) (numV 1)) topsto) (numV 2))
(check-equal? (primsolve 'aset! (list (arrayV 29 3) (numV 1) (strV "changed")) topsto)(nullV 'null))
(check-exn (regexp (regexp-quote "numargs")) (lambda () (primsolve 'make-array (list) topsto)))
(check-exn (regexp (regexp-quote "numargs")) (lambda () (primsolve 'array (list) topsto)))
(check-exn (regexp (regexp-quote "numargs")) (lambda () (primsolve 'aref (list) topsto)))
(check-exn (regexp (regexp-quote "numargs")) (lambda () (primsolve 'aset! (list) topsto)))
(check-exn (regexp (regexp-quote "invalidarg")) (lambda () (primsolve 'aref
                                                                      (list (arrayV 26 3)(strV "not a num")) topsto)))
(check-exn (regexp (regexp-quote "invalidarg")) (lambda () (primsolve 'aset!
                                                                      (list
                                                                       (arrayV 26 3)
                                                                       (strV "not a num")
                                                                       (numV 10)) topsto)))
(check-exn (regexp (regexp-quote "invalidarg")) (lambda () (primsolve 'make-array
                                                                      (list (strV "not a num") (numV 7)) topsto)))
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

;applies interp to a list of ExprC
(define (interphelper [lst : (Listof ExprC)] [env : Env] [sto : (Mutable-Vectorof Any)]) : (Listof Value)
  (match lst
    ['() '()]
    [(cons f r) (cons (interp f env sto) (interphelper r env sto))]))

;takes in an ExprC and an Env and returns a Value
(define (interp [exp : ExprC] [env : Env] [sto : (Mutable-Vectorof Any)]) : Value
  (match exp
    [(numC n) (numV n)]
    [(strC str) (strV str)]
    [(nullC 'null) (nullV 'null)]
    [(mutC id val)
     (define address (get env id))
     (define interpedval (interp val env sto))
     (vector-set! sto address interpedval)
     (nullV 'null)]
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

;(check-equal? (interphelper (list (numC 7) (numC 7)) topenv) (list (numV 7) (numV 7)))

(check-equal? (interp (idC 'x) testenv2 teststo2) (numV 7))
(check-equal? (interp (nullC 'null) topenv topsto) (nullV 'null))
(check-equal? (interp (mutC 'x (numC 1000)) testenv2 teststo2) (nullV 'null))
(check-exn (regexp (regexp-quote "get")) (lambda () (interp (mutC 'unbound (numC 1000)) testenv teststo2)))
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

;returns true if an id is in the list of invalids
(define (invalidid? [id : Symbol] [invalids : (Listof Symbol)]) : Boolean
  (match invalids
    ['() #f]
    [(cons f r) (if (equal? id f) #t (invalidid? id r))]))

(check-equal? (invalidid? 'if '(if bind)) #t)
(check-equal? (invalidid? 'ab '(if bind)) #f)

;parses the concrete syntax to the abstract syntax
(define (parse [sexpr : Sexp]) : ExprC
  (define invalids '(if => = := bind))
  (match sexpr
    [(list (? list? params) '=> body)
         (cond
           [(equal? #f (check-duplicates params)) (parse-lambda params body)]
           [else (error 'dupparams "AAQZ : function contains duplicate params, given ~e" params)])]
    [(? real? n) (numC n)]
    [(list (? symbol? s) ':= v)
     (define isinvalid? (invalidid? s invalids))
     (cond
       [isinvalid? (error 'invalidid "AAQZ : Cannot use ~e as id" s)]
       [else (mutC s (parse v))])]
    [(? symbol? s)
     (define isinvalid? (invalidid? s invalids))
     (cond
       [isinvalid? (error 'invalidid "AAQZ : Cannot use ~e as id" s)]
       [(equal? 'true s) (boolC #t)]
       [(equal? 'false s) (boolC #f)]
       [(equal? 'null s) (nullC s)]
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
(check-equal? (parse 'null) (nullC 'null))
(check-equal? (parse '{x := 15}) (mutC 'x (numC 15)))
(check-exn (regexp (regexp-quote "invalidid")) (lambda () (parse '(:= := 1))))
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
(define (top-interp [s : Sexp] [size : Integer]) : String
  (define sto (make-vector size))
  (vector-fill! sto (nullV 'null))
  (vector-set! sto 0 (numV 16))
  (vector-set! sto 1 (primV '+))
  (vector-set! sto 2 (primV '-))
  (vector-set! sto 3 (primV '/))
  (vector-set! sto 4 (primV '*))
  (vector-set! sto 5 (primV '<=))
  (vector-set! sto 6 (primV 'equal?))
  (vector-set! sto 7 (boolV #t))
  (vector-set! sto 8 (boolV #f))
  (vector-set! sto 9 (primV 'error))
  (vector-set! sto 10 (primV 'make-array))
  (vector-set! sto 11 (primV 'array))
  (vector-set! sto 12 (primV 'aref))
  (vector-set! sto 13 (primV 'aset!))
  (vector-set! sto 14 (primV 'substring))
  (vector-set! sto 15 (primV 'seq))
  (vector-set! sto 16 (nullV 'null))
  (serialize (interp (parse s) topenv sto)))


(check-equal? (top-interp '(+ 1 6) 250) "7")
(check-equal? (top-interp 7 250) "7")
(check-equal? (top-interp '(((x) => x) 7) 250) "7")
(check-equal? (top-interp '((x) => x) 250) "#<procedure>")
(check-exn (regexp (regexp-quote "numargs")) (lambda () (top-interp '((() => 7) 10) 250)))

;this should raise an error
(parse '(bind
         (=> = "")
         "World"))

;Array testing
(define test-store (make-vector 20))
(vector-set! test-store 0 (numV 1))
(check-equal? (make-array 5 (numV 42) test-store)
              (arrayV 1 5))
(for ([i (in-range 1 6)])
  (check-equal? (vector-ref test-store i) (numV 42)))
(check-equal? (make-array 1 (boolV #t) test-store)
              (arrayV 6 1))
(check-equal? (vector-ref test-store 6) (boolV #t))
(check-equal? (make-array 3 (strV "test") test-store)
              (arrayV 7 3))
(for ([i (in-range 7 10)])
  (check-equal? (vector-ref test-store i) (strV "test")))
(check-exn (regexp "AAQZ: Array size must be at least 1")
           (lambda () (make-array 0 (numV 5) test-store)))
(define test-store2 (make-vector 20))
(vector-set! test-store2 0 (numV 1)) 
(check-equal? (array (list (numV 1) (numV 2) (numV 3)) test-store2)
              (arrayV 1 3))
(check-equal? (vector-ref test-store2 1) (numV 1))
(check-equal? (vector-ref test-store2 2) (numV 2))
(check-equal? (vector-ref test-store2 3) (numV 3))
(check-equal? (array (list (boolV #f)) test-store2)
              (arrayV 4 1))
(check-equal? (vector-ref test-store2 4) (boolV #f))
(check-equal? (array (list (strV "hello") (strV "world")) test-store2)
              (arrayV 5 2))
(check-equal? (vector-ref test-store2 5) (strV "hello"))
(check-equal? (vector-ref test-store2 6) (strV "world"))
(check-exn (regexp "AAQZ: Array must contain at least one element.")
           (lambda () (array '() test-store2)))
(define small-store (make-vector 8))
(vector-set! small-store 0 (numV 1))
(define arr1 (arrayV 1 3))
(check-equal? (aref arr1 0 test-store) (numV 42)) 
(check-equal? (aref arr1 1 test-store) (numV 42)) 
(check-equal? (aref arr1 2 test-store) (numV 42))
(check-exn (regexp "AAQZ: Index out of bounds")
           (lambda () (aref arr1 -1 test-store)))
(check-exn (regexp "AAQZ: Index out of bounds")
           (lambda () (aref arr1 3 test-store)))
(check-exn (regexp "AAQZ: Invalid array reference")
           (lambda () (aref (numV 42) 1 test-store)))
(check-equal? (aset! arr1 0 (numV 99) test-store) (nullV 'null))
(check-equal? (aref  arr1 0 test-store) (numV 99))
(check-exn (regexp "AAQZ: Index out of bounds")
           (lambda () (aset! arr1 -1 (numV 100) test-store)))
(check-exn (regexp "AAQZ: Invalid array reference")
           (lambda () (aset! (numV 42) 1 (numV 100) test-store)))

; Define the memory size
(define memory-size 200)


#;(top-interp
  '{bind [x = 10]
         {seq {x := 5} x}}
  memory-size)

#;(top-interp '{bind [fact = "bogus"]
      {seq {fact := {(x) => {if {equal? x 0}
                                1
                                {* x {fact {- x 1}}}}}}
           {fact 12}}} memory-size)

;while function
  (define while '{bind
    [while = "bogus"]
    [body = "nothing"]
    [guard = "nothing"]
    {seq
     {guard := {(g) => (g)}}  ; Guard function checks if x <= 10
     {body := {(b) => (b)}} ; Body increments x and returns updated value
     {while := {(guard body) =>
                        {if (guard)
                            {seq (body) (while guard body)} ; Recur only if guard passes
                            null}}}
     while}})

; Define both the `while` function and the `in-order` function in one bind block (top-interp
; Check `in-order` function with an out-of-order array
(define in-order-fortesting (top-interp
  '{bind
    [while = "bogus"]
    [in-order = "bogus"]
    [i = "placeholder"]
    [result = true]
    ; Define both `while` and `in-order` functions in one scope
    {seq
     {i := 0}
     {while := {(guard body) =>
                             {if (guard)
                                 {seq {body} {while guard body}}
                                 null}}}
     {in-order := {{arr size} =>
                              {bind
                               [guard = {() => (<= i (- size 2))}]
                               [body = {() => (if (<= (aref arr i) (aref arr (+ i 1))) {i := (+ i 1)} {seq
                                                                        {result := false}
                                                                        {i := (+ i 1)}})}]
                               {seq
                                {while guard body}
                                result}}}}

     ; Call `in-order` with an example array
     {in-order {array 1 2 7 4 5} 5}}}
  memory-size))
(check-equal? in-order-fortesting "false")

(define in-order
  '{bind
    [in-order = "bogus"]
    [i = "placeholder"]
    [result = true]
    ; Define both `while` and `in-order` functions in one scope
    {seq
     {i := 0}
     {in-order := {{arr size} =>
                              {bind
                               [guard = {() => (<= i (- size 2))}]
                               [body = {() => (if (<= (aref arr i) (aref arr (+ i 1))) {i := (+ i 1)} {seq
                                                                        {result := false}
                                                                        {i := (+ i 1)}})}]
                               {seq
                                {while guard body}
                                result}}}}

     ; Call `in-order` with an example array
     inorder}})

