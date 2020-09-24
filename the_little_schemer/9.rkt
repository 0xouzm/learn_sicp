#lang racket
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (pick n lat)
  (cond
    ((zero? (sub1 n)) (car lat))
    (else
     (pick (sub1 n) (cdr lat)))))

(define (looking a lat)
  (keep-looking a (pick 1 lat) lat))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn)
       (keep-looking a (pick sorn lat) lat))
      (else
       (eq? sorn a)))))

;(looking 'caviar '(6 2 4 caviar 5 7 3))         ; #t
(define (build s1 s2)
  (cons s1 (cons s2 '())))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))
;(shift '((a b) (c e d)))
(define (a-pair? x)
  (cond
    ((atom? x) #f)
    ((null? x) #f)
    ((null? (cdr x)) #f)
    ((null? (cdr (cdr x))) #t)
    (else #f)))


(define (align pora)
  (cond
    ((atom? pora) pora)
    ((a-pair? (first pora))
     (align (shift pora)))
    (else (build (first pora)
                 (align (second pora))))))

(define (length* pora)
  (cond
    ((atom? pora) 1)
    (else
     (+ (length* (first pora))
        (length* (second pora))))))


         
(define (weight* pora)
  (cond
    ((atom? pora) 1)
    (else
     (+ (* (weight* (first pora)) 2)
        (weight* (second pora))))))

;(length '(c d (e f)))

;(weight* '(a b))
;(weight* '(a (b c)))
;(length* '((a  b) (c d)))

(define revpair
  (lambda (p)
    (build (second p) (first p))))


(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))
(define A
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))

; basic length
(define (length l)
  (cond
    ((null? l) 0)
    (else (add1 (length (cdr l))))))



(define (eternity x)
  (eternity x))


(define (length0)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (eternity (cdr l)))))))

; this equls to =>
((lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)

;a function can determine the length of lists that contain one or fewer items
(define (length1)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length0 (cdr l)))))))


;length<=1
((lambda (f)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (f (cdr l))))))
   )
 ((lambda (g)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (g (cdr l))))))
    )
  eternity)
 )

((lambda (x)
   (x eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))

;length0
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 )

;length0
(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (mk-length (cdr l))))))))
 '())

;length
(
 (
  (lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 ((mk-length mk-length) (cdr l)))))))
 )
 '(a b c))






     
