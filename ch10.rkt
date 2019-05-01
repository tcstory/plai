#lang plai


(define o-1
  (lambda (m)
    (case m
      ((add1) (lambda (x) (+ x 1)))
      ((sub1) (lambda (x) (- x 1))))))

(define (msg o m . a)
  (apply (o m) a))

(define (o-constr-1 x)
  (lambda (m)
    (case m
      ((addX) (lambda (y) (+ x y))))))

(define (o-state-1 count)
  (lambda (m)
    (case m
      ((inc) (lambda () (set! count (+ count 1))))
      ((dec) (lambda () (set! count (- count 1))))
      ((get) (lambda () count)))))

(define (o-state-2 init)
  (let ((count init))
  (lambda (m)
    (case m
      ((inc) (lambda () (set! count (+ count 1))))
      ((dec) (lambda () (set! count (- count 1))))
      ((get) (lambda () count))))))

(define o-static-1
  (let ((counter 0))
    (lambda (amount)
      (begin
        (set! counter (+ counter 1))
        (lambda (m)
          (case m
            ((inc) (lambda (n) (set! amount (+ amount n))))
            ((dec) (lambda (n) (set! amount (- amount n))))
            ((get) (lambda () amount))
            ((count) (lambda () counter))))))))

(define o-self!
  (let ((self 'dummy))
    (begin
      (set! self
            (lambda (m)
              (case m
                ((first) (lambda (x) (msg self 'second (+ x 1))))
                ((second) (lambda (x) (+ x 1))))))
      self)))

(define o-self-no!
  (lambda (m)
    (case m
      ((first) (lambda (self x) (msg/self self 'second (+ x 1))))
      ((second) (lambda (self x) (+ x 1))))))

(define (msg/self o m . a)
  (apply (o m) o a))

(define (mt)
  (let ((self 'dummy))
    (begin
      (set! self
            (lambda (m)
              (case m
                ((add) (lambda () 0)))))
      self)))

(define (node v l r)
  (let ((self 'dummy))
    (begin
      (set! self
            (lambda (m)
              (case m
                ((add) (lambda ()
                         (+ v (msg l 'add) (msg r 'add)))))))
      self)))

(define (node/size parent-maker v l r)
  (let ((parent-object (parent-maker v l r))
        (self 'dummy))
    (begin
      (set! self
            (lambda (m)
              (case m
                ((size) (lambda ()
                          (+ 1 (msg l 'size) (msg r 'size))))
                (else (parent-object m)))))
      self)))

(define (mt/size parent-maker)
  (let ((parent-object (parent-maker))
        (self 'dummy))
    (begin
      (set! self
            (lambda (m)
              (case m
                ((size) (lambda () 0))
                (else (parent-object m)))))
      self)))

(define a-tree
  (node 10
        (node 5 (mt) (mt))
        (node 15 (node 6 (mt) (mt)) (mt))))

(define a-tree/size
  (node/size node
             10
             (node/size node 5 (mt/size mt) (mt/size mt))
             (node/size node 15
                        (node/size node 6 (mt/size mt) (mt/size mt))
                        (mt/size mt))))