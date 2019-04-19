#lang plai-typed

(define-type MisspelledAnimal
  (caml (humps : number))
  (yacc (height : number)))

(define (good? (ma : MisspelledAnimal)) : boolean
  (type-case MisspelledAnimal ma
    (caml (h) (>= h 2))
    (yacc (h) (> h 2.1))))

(define (good1? (ma : MisspelledAnimal)) : boolean
  (cond
    ((caml? ma) (>= (caml-humps ma) 2))
    ((yacc? ma) (> (yacc-height ma) 2.1))))


(define ma1 : MisspelledAnimal (caml 2))
(define ma2 (yacc 1.9))

; (test (good? ma1) #t)
; (test (good? ma2) #f)