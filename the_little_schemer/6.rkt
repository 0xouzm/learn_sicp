#lang racket
(define (atom? x)
  (not (and (pair? x) (null? x))))


(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (numbered? aexp))
      ((eq? (car (cdr aexp)) '+)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
            ((eq? (car (cdr aexp)) '*)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
                  ((eq? (car (cdr aexp)) '^)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))))))


;(define value
;  (lambda (nexp)
;    (cond
;      ((atom? nexp) nexp)
 ;     ((eq? (car (cdr nexp)) '+)
  ;     (+ (value (car nexp))
   ;       (value (car (cdr (cdr nexp))))))
      ; same for '* and '^
    ;  )))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car nexp) '+)
       (+ (value (cdr nexp))
          (value (cdr (cdr (nexp))))))
      ((eq? (car nexp) '*)
       (* (value (cdr nexp))
          (value (cdr (cdr nexp)))))
      (else
       (expt (value (cdr nexp))
          (value (cdr (cdr nexp))))))))

(define operator
  (lambda (aexp)
    (car aexp)))
(define lat? ( lambda (l) (cond ((null? l)  #t ) ((atom? (car l)) (lat? (cdr  l))) (else #f ))) )


(lat? '((()) (()()) (()()())))
