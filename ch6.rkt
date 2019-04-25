#lang plai-typed

(define-type Binding
  (bind (name : symbol) (val : number)))

(define-type-alias Env (listof Binding))

(define mt-env empty)

(define extend-env cons)

(define (lookup (name : symbol) (env : Env)) : number
  (cond
    ((empty? env) (error 'lookup "name not found"))
    ((cons? env)
     (if (symbol=? name (bind-name (first env)))
         (bind-val (first env))
         (lookup name (rest env))))))
  
(define-type ExprC
  (numC (n : number))
  (idC (s : symbol))
  (appC (fun : symbol) (arg : ExprC))
  (plusC (l : ExprC) (r : ExprC))
  (multC (l : ExprC) (r : ExprC)))

(define-type ArithS
  (numS (n : number))
  (plusS (l : ArithS) (r : ArithS))
  (multS (l : ArithS) (r : ArithS))
  (bminusS (l : ArithS) (r : ArithS))
  (uminusS (e : ArithS)))

(define-type FunDefC
  (fdC (name : symbol) (arg : symbol) (body : ExprC)))

(define (get-fundef (n : symbol) (fds : (listof FunDefC))) : FunDefC
  (cond
    ((empty? fds)
     (error 'get-fundef "reference to undefined function"))
    ((cons? fds)
     (if (equal? n (fdC-name (first fds)))
         (first fds)
         (get-fundef n (rest fds))))))
 

(define (subst (what : ExprC) (for : symbol) (in : ExprC)) : ExprC
  (type-case ExprC in
    (numC (n) in)
    (idC (s)
         (if (symbol=? s for)
             what
             in))
    (appC (fun arg)
          (appC fun (subst what for arg)))
    (plusC (l r)
           (plusC (subst what for l) (subst what for r)))
    (multC (l r)
           (multC (subst what for l) (subst what for r)))))

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

(define (interp (a : ExprC) (env : Env) (fds : (listof FunDefC))) : number
  (type-case ExprC a
    (numC (n) n)
    (appC (fun arg)
          (let ((fd (get-fundef fun fds)))
            (interp
             (fdC-body fd)
             (extend-env (bind (fdC-arg fd)
                               (interp arg env fds))
                         mt-env)
             fds)))
    [idC (n) (lookup n env)]
    (plusC (l r) (+ (interp l env  fds) (interp r env fds)))
    (multC (l r) (* (interp l env fds) (interp r env fds)))))

(define fds (list
             (fdC 'double 'x (plusC (idC 'x) (idC 'x)))
             (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
             (fdC 'const5 '_ (numC 5))))


;(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
;              mt-env
;              (list (fdC 'const5 '_ (numC 5))))
;      15)
;
;(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
;              mt-env
;              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
;      16)
;
;(test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
;              mt-env
;              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
;                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
;      22)