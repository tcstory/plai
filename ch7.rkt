#lang plai-typed

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
  (bind (name : symbol) (val : Value)))

(define-type-alias Env (listof Binding))

(define mt-env empty)

(define extend-env cons)

(define (lookup (name : symbol) (env : Env)) : Value
  (cond
    ((empty? env) (error 'lookup (string-append "name not found" (to-string name))))
    ((cons? env)
     (if (symbol=? name (bind-name (first env)))
         (bind-val (first env))
         (lookup name (rest env))))))
  
(define-type ExprC
  (numC (n : number))
  (idC (s : symbol))
  (appC (fun : ExprC) (arg : ExprC))
  (lamC (arg : symbol) (body : ExprC))
  (plusC (l : ExprC) (r : ExprC))
  (multC (l : ExprC) (r : ExprC)))

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

(define (interp (a : ExprC) (env : Env)) : Value
  (type-case ExprC a
    (numC (n) (numV n))
    (lamC (arg body) (closV arg body env))
    (appC (fun arg)
          (let ((fd (interp fun env)))
            (interp
             (closV-body fd)
             (extend-env (bind (closV-arg fd)
                               (interp arg env))
                         (closV-env fd)))))
    [idC (n) (lookup n env)]
    (plusC (l r) (num+ (interp l env) (interp r env)))
    (multC (l r) (num* (interp l env) (interp r env)))))

;(interp (appC (appC (lamC 'x
;                 (lamC 'y
;                      (plusC (idC 'x) (idC 'y))))
;            (numC 4))
;      (numC 5)) mt-env)

;(interp
; (appC (lamC 'x
;             (appC (lamC 'y (plusC (idC 'y) (idC 'y))) (numC 10)))
;       (numC 2)) mt-env)


