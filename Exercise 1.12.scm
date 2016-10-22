; Exercise 1.12: The following pattern of numbers is called Pascal’s triangle.

;          1
;        1   1
;      1   2   1
;    1   3   3   1
;  1   4   6   4   1
;        . . .
; The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it.35 Write a procedure that computes elements of Pascal’s triangle by means of a recursive process.

#lang planet neil/sicp

(define (pascal row col)
  (cond ((or (< row col)
             (< row 1)
             (< col 1)) 0)
        ((or (= col 1)
             (= col row)) 1)

        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))
  )
)

(pascal 5 6)
(pascal 0 1)
(pascal 1 -1)

(pascal 1 1)
(pascal 12 1)
(pascal 12 12)

(pascal 5 3)
(pascal 5 2)
(pascal 12 2)
