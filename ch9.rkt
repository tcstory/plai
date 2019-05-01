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
  (closV (arg : symbol) (body : ExprC) (env : Env))
  (objV (ns : (listof symbol)) (vs : (listof Value))))

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

(define (lookup-msg (n : symbol) (o : Value)) : Value
  (type-case Value o
    (objV (ns es)
          (letrec ((find (lambda (n0 ns0 es0)
                           (if (symbol=? n0 (first ns0))
                               (first es0)
                               (find n0 (rest ns0) (rest es0))))))
            (find n ns es)))
    (else (numV -1))))

(define-type ExprC
  (numC (n : number))
  (idC (s : symbol))
  (appC (fun : ExprC) (arg : ExprC))
  (lamC (arg : symbol) (body : ExprC))
  (plusC (l : ExprC) (r : ExprC))
  (multC (l : ExprC) (r : ExprC))
  (objC (ns : (listof symbol)) (es : (listof ExprC)))
  (msgC (o : ExprC) (n : symbol))
  (letC (s : symbol) (body : ExprC) (e : ExprC)))

(define-type ExprS
  (numS (n : number))
  (idS (s : symbol))
  (appS (fun : ExprS) (arg : ExprS))
  (lamS (arg : symbol) (body : ExprS))
  (plusS (l : ExprS) (r : ExprS))
  (multS (l : ExprS) (r : ExprS))
  (bminusS (l : ExprS) (r : ExprS))
  (uminusS (e : ExprS))
  (objS (ns : (listof symbol)) (es : (listof ExprS)))
  (msgS (o : ExprS) (n : symbol) (a : ExprS))
  (letS (s : symbol) (body : ExprS) (e : ExprS)))

(define (desugar (as : ExprS)) : ExprC
  (type-case ExprS as
    (numS (n) (numC n))
    (idS (s) (idC s))
    (appS (fun arg) (appC (desugar fun) (desugar arg)))
    (lamS (arg body) (lamC arg (desugar body)))
    (plusS (l r) (plusC (desugar l) (desugar r)))
    (multS (l r) (multC (desugar l) (desugar r)))
    (bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r))))
    (uminusS (e) (desugar (bminusS (numS 0) e)))
    (objS (ns es) (objC ns (map (lambda (e)
                                  (desugar e)) es)))
    (msgS (o n a) (appC (msgC (desugar o) n) (desugar a)))
    (letS (s body e) (letC s (desugar body) (desugar e)))))

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
    (multC (l r) (num* (interp l env) (interp r env)))
    (objC (ns es) (objV ns (map (lambda (e)
                                  (interp e env)) es)))
    (msgC (o n) (lookup-msg n (interp o env)))
    (letC (s body e) (interp e (extend-env
                                (bind s
                                      (interp body env)) env)))))

;(interp (desugar (letS 'o (objS (list 'add1 'sub1)
;               (list (lamS 'x (plusS (idS 'x) (numS 1)))
;                     (lamS 'x (plusS (idS 'x) (numS -1)))))
;      (msgS (idS 'o) 'add1 (numS 3)))) mt-env)