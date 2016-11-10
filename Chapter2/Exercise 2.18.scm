; Exercise 2.18: Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order:

#lang planet neil/sicp
(define (reverse l)
    (define (iter s r)
        (if (null? (cdr s))
            (cons (car s) r)
            (iter (cdr s) (cons (car s) r))
        )
    )
    (iter l nil)
)
(define (reverse_recursive l)
    (if (null? (cdr l))
        l
        (append (reverse_recursive (cdr l)) (list (car l)))
    )
)

(list 1 4 9 16 25)
(reverse (list 1 4 9 16 25))
(reverse_recursive (list 1 4 9 16 25))
; (25 16 9 4 1)
````````````````````````````
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
(mcons 1 (mcons 4 (mcons 9 (mcons 16 (mcons 25 '())))))
(mcons 25 (mcons 16 (mcons 9 (mcons 4 (mcons 1 '())))))
> 