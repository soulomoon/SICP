; Exercise 2.10: Ben Bitdiddle, an expert systems programmer, looks over Alyssa’s shoulder and comments that it is not clear what it means to divide by an interval that spans zero. Modify Alyssa’s code to check for this condition and to signal an error if it occurs.
#lang planet neil/sicp

(define (make_interval a b) (cons a b))
(define (upper_bound x) (car x))
(define (lower_bound x) (cdr x))

(define (mul_interval x y)
  (let ((p1 (* (lower_bound x) 
               (lower_bound y)))
        (p2 (* (lower_bound x) 
               (upper_bound y)))
        (p3 (* (upper_bound x) 
               (lower_bound y)))
        (p4 (* (upper_bound x) 
               (upper_bound y))))
    (make_interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div_interval x y)
    (if (< 0 (* (upper_bound y) (lower_bound y))) 
        (mul_interval x 
            (make_interval 
                (/ 1.0 (upper_bound y)) 
                (/ 1.0 (lower_bound y))))
        (error "Argument crossing 0:
                   CONS" y)
    )
)


(define a 
    (make_interval 1 2)
)

(define b 
    (make_interval 2 0)
)
(div_interval a b)
```````````````````````````````````````
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
. . Argument crossing 0:
                   CONS {2 . 0}
> 