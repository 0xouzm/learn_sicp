#lang racket
(lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))

(define eternity
  (lambda (x)
    (eternity x)))


;length0
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 )

;***,length0 变种
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (mk-length (cdr l))))))))

; one more eternity,验证 length1
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length eternity) (cdr l)))))))
 )


; 代入 '(apple) 并展开
;step1
(
 (
  (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((mk-length eternity) (cdr l)))))))
  (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((mk-length eternity) (cdr l)))))))
  )
 '(apple))


; 第二步
(
 (lambda (l)
   (cond
     ((null? l) 0)
     (else (add1 (
                  (
                   (lambda (mk-length)
                     (lambda (l)
                       (cond
                         ((null? l) 0)
                         (else (add1
                                ((mk-length eternity)
                                 (cdr l)))))))
                   eternity) 
                  (cdr l)
                  )
                 ))))
 '(apple))

;第三步
(
 (lambda (l)
   (cond
     ((null? l) 0)
     (else (add1
            ((lambda (l)
               (cond
                 ((null? l) 0)
                 (else (add1
                        ((eternity eternity)
                         (cdr l)))))) (cdr l))))))
 '(apple))

;now we can calculate 1-len list

;ultimate
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length mk-length) (cdr l)))))))
 )

;import lazy evaluation
;(lambda (x)
 ; ((mk-length mk-length) x))



(
 (lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (
    (lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x))
    )
   )
 )

((lambda (le)
   (lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x)
           ((mk-length mk-length) x)))))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 )


(lambda (le)
  ((lambda (f) (f f))
   (lambda (f)
     (le (lambda (x) ((f f) x))))))
