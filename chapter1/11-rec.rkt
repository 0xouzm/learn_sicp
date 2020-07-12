#lang sicp
(define (f n)
  (if (< n 3)
      n
      (+ (f(dec n)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))
(f 3)
(f 4)


