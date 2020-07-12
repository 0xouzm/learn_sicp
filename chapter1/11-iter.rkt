#lang sicp

(define (f n)
  (define (iter a b c count)
    (cond ((< n 3) n)
          ((<= count 0) a)
          ; 参数为具体的数值，这才是迭代
          (else (iter (+ a (* 2 b) (* 3 c)) a b (dec count)))))
    (iter 2 1 0 (- n 2)))
(f 3)
(f 4)