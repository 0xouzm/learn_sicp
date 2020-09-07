#lang racket
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))


(define (rember-f test?)
  (lambda (a l)
    (cond
      ((null? l) '())
      ((test? a (car l)) (cdr l))
      (else
       (cons (car l)
             ((rember-f test?) a (cdr l)))))))


(define (insertL-f test?)
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((test? old (car l)) (cons new (cons old (cdr l))))
      (else
       (cons (car l) ((insertL-f test?) new old (cdr l)))))))


(define (insertR-f test?)
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((test? old (car l)) (cons old (cons new (cdr l))))
      (else (cons (car l) ((insertR-f test?) new old (cdr l)))))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old) (seq new old (cdr l)))
        (else (cons (car l) ((insert-g seq)(new old (cdr l)))))))))

; redefine rember
(define seqrem (lambda (new old l) l))
(define yyy
  (lambda (a l)
    ((insert-g seqrem) 'fuck a l)))

;(yyy 'sausage '(sausage aaa  bbb))


(define (atom-to-function x)
  (cond
    ((eq? x '*) *)
    ((eq? x '+) +)
    (else expt)))


(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat))
       (multiremberT test? (cdr lat)))
      (else
       (cons (car lat) (multiremberT test? (cdr lat)))))))

(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col '() '()))
      ((eq? (car lat) a)
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen)))))
      (else
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat)
                              seen)))))))



(define (a-friend x y)
  (null? y))

(define (new-frined newlat seen)
  (a-friend newlat
            (cons 'tuna seen)))

(define (latest-friend newlat seen)
  (a-friend (cons 'and newlat) seen))


;(multirember&co 'tuna '(tuna) a-friend)


(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)
       (cons new
             (cons oldL
                   (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR
             (cons new
                   (multiinsertLR new oldL oldR (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertLR new oldL oldR (cdr lat)))))))



(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat)
       (col '() 0 0))
      ((eq? oldL (car lat))
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new (cons oldL newlat)) (add1 L) R))))
      ((eq? oldR (car lat))
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR (cons new newlat)) L (add1 R)))))
      (else
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (lambda (newlat L R)
                           (col (cons (car lat) newlat) L R)))))))





(define (even? n)
  (cond
    ((atom? n)
     (= (* (quotient n 2) 2) n))
    (else #f)))


(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l))
          (cons (car l) (evens-only* (cdr l))))
         (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l))
                  (evens-only* (cdr l)))))))

;(evens-only* '((9 1 2 8)  3  10 ((9  9)  7   6) 2))

(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l)
       (col '() 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl)
                             (* (car l) p)
                             s))))
         (else
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col newl p (+ (car l) s)))))))
      (else
       (evens-only*&co (car l)
                       (lambda (al ap as)
                         (evens-only*&co (cdr l)
                                         (lambda (dl dp ds)
                                           (col (cons al dl)
                                                (* ap dp)
                                                (+ as ds))))))))))
                        
(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))

(evens-only*&co '((9 1 2 8) 3 10 ((9  9) 7 6) 2) the-last-friend)
                        
        
