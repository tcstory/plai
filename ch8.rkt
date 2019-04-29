#lang plai-typed

(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

(define-type Result
  (v*s (v : Value) (s : Store)))

(define-type-alias Location number)

(define (num+ (l : Value) (r : Value)) : Value
  (if (and (numV? l) (numV? r))
      (numV (+ (numV-n l) (numV-n r)))
      (error 'num+ "one argument was not a number")))

(define (num* (l : Value) (r : Value)) : Value
  (if (and (numV? l) (numV? r))
      (numV (* (numV-n l) (numV-n r)))
      (error 'num* "one argument was not a number")))

(define-type Value
  (numV (n : number))
  (closV (arg : symbol) (body : ExprC) (env : Env)))

(define-type Binding
  (bind (name : symbol) (val : Location)))

(define-type-alias Env (listof Binding))

(define mt-env empty)

(define extend-env cons)

(define-type Storage
  (cell (location : Location) (val : Value)))

(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)


(define (lookup (name : symbol) (env : Env)) : Location
  (cond
    ((empty? env) (error 'lookup (string-append "name not found" (to-string name))))
    ((cons? env)
     (if (symbol=? name (bind-name (first env)))
         (bind-val (first env))
         (lookup name (rest env))))))

(define (fetch (loc : Location) (sto : Store)) : Value
  (cond
    ((empty? sto) (error 'fetch (string-append "loc no found"
                                               (to-string  loc))))
    ((cons? sto)
     (if (equal? loc (cell-location (first sto)))
         (cell-val (first sto))
         (fetch loc (rest sto))))))
  
(define-type ExprC
  (numC (n : number))
  (varC (s : symbol))
  (appC (fun : ExprC) (arg : ExprC))
  (lamC (arg : symbol) (body : ExprC))
  (plusC (l : ExprC) (r : ExprC))
  (multC (l : ExprC) (r : ExprC))
  (setC (var : symbol) (arg : ExprC))
  (seqC (b1 : ExprC) (b2 : ExprC)))

(define-type ArithS
  (numS (n : number))
  (plusS (l : ArithS) (r : ArithS))
  (multS (l : ArithS) (r : ArithS))
  (bminusS (l : ArithS) (r : ArithS))
  (uminusS (e : ArithS)))

(define (desugar (as : ArithS)) : ExprC
  (type-case ArithS as
    (numS (n) (numC n))
    (plusS (l r) (plusC (desugar l) (desugar r)))
    (multS (l r) (multC (desugar l) (desugar r)))
    (bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r))))
    (uminusS (e) (desugar (bminusS (numS 0) e)))))

(define (parse (s : s-expression)) : ArithS
  (cond
    ((s-exp-number? s) (numS (s-exp->number s)))
    ((s-exp-list? s)
     (let ((sl (s-exp->list s)))
       (case (s-exp->symbol (first sl))
         ((+) (plusS (parse (second sl)) (parse (third sl))))
         ((*) (multS (parse (second sl)) (parse (third sl))))
         ((-) (if (> (length sl) 2)
                  (bminusS (parse (second sl)) (parse (third sl)))
                  (uminusS (parse (second sl)))))
         (else (error 'parse "invalid list input")))))
    (else
     (error 'parse "invalid input"))))

(define (interp (a : ExprC) (env : Env) (sto : Store)) : Result
  (type-case ExprC a
    (numC (n) (v*s (numV n) sto))
    (lamC (arg body) (v*s (closV arg body env) sto))
    (appC (fun arg)
          (type-case Result (interp fun env sto)
            (v*s (v-f s-f)
                 (type-case Result (interp arg env s-f)
                   (v*s (v-a s-a)
                        (let ((where (new-loc)))
                          (interp (closV-body v-f)
                                  (extend-env (bind (closV-arg v-f) where)
                                              (closV-env v-f))
                                  (override-store (cell where v-a) s-a))))))))
    [varC (n) (v*s (fetch (lookup n env) sto) sto)]
    (plusC (l r) (type-case Result (interp l env sto)
                   (v*s (v-l s-l)
                        (type-case Result (interp r env s-l)
                          (v*s (v-r s-r)
                               (v*s (num+ v-l v-r) s-r))))))
    (multC (l r) (type-case Result (interp l env sto)
                   (v*s (v-l s-l)
                        (type-case Result (interp r env s-l)
                          (v*s (v-r s-r)
                               (v*s (num* v-r v-r) s-r))))))
    (setC (var val) (type-case Result (interp val env sto)
                      (v*s (v-val s-val)
                           (let ((where (lookup var env)))
                             (v*s v-val
                             (override-store (cell where v-val) s-val))))))
    (seqC (b1 b2) (type-case Result (interp b1 env sto)
                    (v*s (v-b1 s-b1)
                         (interp b2 env s-b1))))))


