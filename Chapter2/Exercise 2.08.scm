; Exercise 2.8: Using reasoning analogous to Alyssa’s, describe how the difference of two intervals may be computed. Define a corresponding subtraction procedure, called sub-interval.
#lang planet neil/sicp

(define (make_interval a b) (cons a b))
(define (upper_bound x) (car x))
(define (lower_bound x) (cdr x))

(define (sub_interval x y) 
    (make_interval
        (- (upper_bound x) (lower_bound y))
        (- (lower_bound x) (upper_bound y))
    )
)

(define a 
    (make_interval 1 2)
)

(define b 
    (make_interval 2 3)
)
(sub_interval a b)