; Exercise 2.7: Alyssa’s program is incomplete because she has not specified the implementation of the interval abstraction. Here is a definition of the interval constructor:

; (define (make-interval a b) (cons a b))
; Define selectors upper-bound and lower-bound to complete the implementation.
#lang planet neil/sicp

(define (make_interval a b) (cons a b))
(define (upper_bound x) (car x))
(define (lower_bound x) (cdr x))

(define a 
    (make_interval 1 2)
)
(upper_bound a)
(lower_bound a)
````````````````````````````````````````
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
1
2
> 