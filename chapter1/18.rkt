#lang sicp
(define (fast-expt b n)
  (expt-iter b n 1))

(define (expt-iter b n a)
  (cond 
    ((= n 0) a)
    ((even? n) 
    (expt-iter (* b b) (/ n 2) a))
    ((odd? n)
    (expt-iter b (dec n) (* b a))))
  )

(define (double x)
(* 2 x))

(define (halve x)
(/ x 2))

(define (* a b)
  (cond ((= b 0) 0)
    ((even? b) 
    (double (* a (halve b))))
    (else 
    (+ a (* a (dec b))))))


()