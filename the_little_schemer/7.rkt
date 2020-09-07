#lang racket
(define (atom? x)
  (not (and (null? x) (pair? x))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))

(define (set? lat)
  (cond
    ((null? lat) #t)
    ((member? (car lat) (cdr lat)) #f)
    (else (set? (cdr lat)))))


(define (makeset lat)
  (cond
    ((null? lat) '())
    ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
    (else (cons (car lat) (makeset (cdr lat))))))

(define (subset? set1 set2)
  (cond
    ((null? set1) #t)
    (else (and (member? (car set1) set2)
     (subset? (cdr set1) set2)))
))

(define (eqset? set1 set2)
  (and (subset? set1 set2) (subset? set2 set1)))

(define (intersect? set1 set2)
  (cond
    ((null? set1) #f)
    (else
     (or (member? (car set1) set2)
         (intersect? (cdr set1) set2)))))

(define (intersect set1 set2)
  (cond
    ((null? set1) '())
    ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
    (else
     (intersect (cdr set1) set2))))


(define (union set1 set2)
  (cond
    ((null? set1) set2)
    ((member? (car set1) set2) (union (cdr set1) set2))
    (else
     (cons (car set1) (union (cdr set1) set2)))))

(define (intersectall l-set)
  (cond
    ((null? (cdr l-set)) (car l-set))
    (else
     (intersect (car l-set)
      (intersectall (cdr l-set))))))

(define x '( ( 6 pears and) (3 peaches and 6 peppers) (8 pears  and 6 plums) (and 6 prunes with some apples)))

;test assume that the x is one list of non-empty sets 
; (intersectall x)

;(define (fun? rel)
  ;(set? (firsts rel)))

(define (first p)
  (car p))

(define (second p)
  (car (cdr p)))

(define (build s1 s2)
  (cons s1 (cons s2 '())))


(define (reveal rel)
  (cond
    ((null? rel) '())
    (else
     (cons (build (second (car rel)) (first (car rel)))
           (reveal (cdr rel))))))

(reveal '((8 a)  (pumpkin pie) (got sick)))