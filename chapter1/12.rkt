#lang sicp
(define (pascal row col)
  (cond
    ((> col row)(error ("invalid col value")))
    ((or (= col row) (= col 0)) 1)
    (else (+
           (pascal (dec row) (dec col))
           (pascal (dec row) col))
          )
    )
  )
(pascal 4 0)
(pascal 4 4)
(pascal 4 2)