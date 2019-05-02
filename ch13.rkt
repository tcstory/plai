#lang plai


(define-syntax my-let-1
  (syntax-rules ()
    ((_ (var val) body)
     ((lambda (var) body) val))))

(define-syntax my-let-2
  (syntax-rules ()
    ((my-let-2 ((var val) ...) body)
     ((lambda (var ...) body) val ...))))

(define-syntax (my-let-3 x)
  (syntax-case x ()
    ((_ (var val) body)
     (identifier? #'var)
     #'((lambda (var) body) val))))

(define-syntax (my-or-3 x)
  (syntax-case x()
    ((my-or-3) #'#f)
    ((my-or-3 e) #'e)
    ((my-or-3 e0 e1 ...)
     #'(if e0
           e0
           (my-or-3 e1 ...)))))

(define-syntax (my-or-4 x)
  (syntax-case x ()
    ((my-or-4)
     #'#f)
    ((my-or-4 e)
     #'e)
    ((my-or-4 e0 e1 ...)
     #'(let ((v e0))
         (if v
             v
             (my-or-4 e1 ...))))))


(define (msg o m . a)
  (apply (o m) a))

(define os-1
  (object/self-3
   (first (x) (msg self 'second (+ x 1)))
   (second (x) (+ x 1))))

;(define-syntax object/self-1
;  (syntax-rules ()
;    ((object/self-1 (mtd-name (var) val) ...)
;     (let ((self (lambda (msg-name)
;                   (lambda (v) (error 'object "nothing here")))))
;       (begin
;         (set! self
;               (lambda (msg)
;                 (case msg
;                   ((mtd-name) (lambda (var) val))
;                   ...)))
;         self)))))

(define-syntax (object/self-3 x)
  (syntax-case x ()
    [(object [mtd-name (var) val] ...)
     (with-syntax ([self (datum->syntax x 'self)])
       #'(let ([self (lambda (msg-name)
                       (lambda (v) (error 'object "nothing here")))])
           (begin
             (set! self
                   (lambda (msg-name)
                     (case msg-name
                       [(mtd-name) (lambda (var) val)]
                       ...)))
             self)))]))
(msg os-1 'first 1)