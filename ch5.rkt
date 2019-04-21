#lang plai-typed

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

(define (interp (a : ExprC) (fds : (listof FunDefC))) : number
  (type-case ExprC a
    (numC (n) n)
    (appC (f a)
           (local ((define fd (get-fundef f fds)))
             (interp
              (subst (numC (interp a fds)) (fdC-arg fd) (fdC-body fd))
              fds)))
    [idC (_) (error 'interp "shouldn't get here")]
    (plusC (l r) (+ (interp l fds) (interp r fds)))
    (multC (l r) (* (interp l fds) (interp r fds)))))

(define fds (list
             (fdC 'double 'x (plusC (idC 'x) (idC 'x)))
             (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
             (fdC 'const5 '_ (numC 5))))


; (interp (desugar (parse '(+ 1 2))) fds)
; (interp (appC 'double (numC 2)) fds)
; (interp (appC 'quadruple (numC 2)) fds)
; (interp (appC 'const5 (idC '_)) fds)

; (interp (appC 'double (plusC (numC 1) (numC 2))) fds)