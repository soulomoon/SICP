; Exercise 2.21: The procedure square-list takes a list of numbers as argument and returns a list of the squares of those numbers.

; (square-list (list 1 2 3 4))
; (1 4 9 16)
; Here are two different definitions of square-list. Complete both of them by filling in the missing expressions:
#lang planet neil/sicp
(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map square items)
)

(square-list (list 1 2 3 4))
(square-list-map (list 1 2 3 4))
``````````````````````````````````````````````````````
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
(mcons 1 (mcons 4 (mcons 9 (mcons 16 '()))))
(mcons 1 (mcons 4 (mcons 9 (mcons 16 '()))))
> 