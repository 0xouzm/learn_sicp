#lang sicp
; (define (* a b)
; (if (= b 0)
;     0
;     (+ a (* a (- b 1)))))

(define (double x)
(* 2 x))

(define (halve x)
(/ x 2))

(define (multi a b)
  (cond ((= b 0) 0)
    ((even? b) 
    (double (multi a (halve b))))
    (else 
    (+ a (multi a (dec b))))))