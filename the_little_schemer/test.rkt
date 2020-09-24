#lang racket

  
(define (eternity)
  (lambda (x)
    (eternity x)))


(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))


((lambda (length)   
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ((lambda (length)   
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 ((lambda (length)   
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)))


((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length mk-length) (cdr l)))))))
 )


;(lambda (x)
; ((mk-length mk-length) x))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (
    le
    (lambda (x)
            ((mk-length mk-length) x))
    )
   )
 )

((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x)))))
   (lambda (length)   
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))))


