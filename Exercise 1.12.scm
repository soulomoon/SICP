; Exercise 1.12: The following pattern of numbers is called Pascal’s triangle.

;          1
;        1   1
;      1   2   1
;    1   3   3   1
;  1   4   6   4   1
;        . . .
; The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it.35 Write a procedure that computes elements of Pascal’s triangle by means of a recursive process.

#lang planet neil/sicp

(define (ps row col)
  (cond ((or (< row col)
             (< row 1)
             (< col 1)) 0)
        ((or (= col 1)
             (= col row)) 1)
        (else (+ (ps (- row 1) (- col 1))
                 (ps (- row 1) col)))
  )
)

(ps 5 6)
(ps 0 1)
(ps 1 -1)
(display "-------------------")(newline)
(ps 1 1)
(ps 3 3)
(ps 12 1)
(display "-------------------")(newline)
(ps 2 2)
(ps 12 2)
(ps 12 2)

;version col start from 0
(define (ps row col)
  (cond ((or (< row (+ col 1))
             (< row 1)
             (< col 0)) 0)
        ((or (= col 0)
             (= (+ col 1) row)) 1)
        (else (+ (ps (- row 1) (- col 1))
                 (ps (- row 1) col)))
  )
)



(ps 5 6)
(ps 0 1)
(ps 1 -1)
(display "-------------------")(newline)
(ps 1 0)
(ps 1 0)
(ps 12 11)
(display "-------------------")(newline)
(ps 2 2)
(ps 12 2)
(ps 12 2)
