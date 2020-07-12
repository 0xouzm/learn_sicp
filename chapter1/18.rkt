#lang sicp
(define (double x)
(* 2 x))

(define (halve x)
(/ x 2))

(define (multi a b)
(multi-iter a b 0)
)

(define (multi-iter a b product)
(cond ((= b 0) product)
  ((even? b) (multi-iter (double a) (halve b) product
  ))
  ((odd? b)
  
    (multi-iter a (dec b) (+ a product))
)))

(multi 2 2)
(multi 3 5)