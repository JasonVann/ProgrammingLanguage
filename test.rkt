#lang racket
(provide (all-defined-out))
#| making all top-level definitions externally visible. You do not need this
line to use your definitions in the REPL (the bottom buffer). You do need this line (or another approach) to use your definitions from a second testing file.
|#

(define (cube3 x)
  (* x x x))

; is syntactic sugar for
(define cube2
  (lambda (x)
    (* x x x)))

(define (pow1 x y)
  (if (= y 0)
      1
      (* x (pow1 x (- y 1)))))
; currying
(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))

(define sixteen ((pow2 4) 2))

(define (silly-triple x)
  (letrec ((y (+ x 2))
           (f (lambda (z) (+ z y w x)))
           (w (+ x 7)))
    (f -9)))

(define (silly-mod2 x)
  (letrec
      ((even? (lambda (x) (if (zero? x) #t (odd? (- x 1)))))
       (odd? (lambda (x) (if (zero? x) #f (even? (- x 1))))))
    (if (even? x) 0 1)))


  