; Exercise 2.11: In passing, Ben also cryptically comments: “By testing the signs of the endpoints of the intervals, it is possible to break mul-interval into nine cases, only one of which requires more than two multiplications.” Rewrite this procedure using Ben’s suggestion.


#lang planet neil/sicp

(define (make_interval a b) (cons a b))
(define (upper_bound x) (car x))
(define (lower_bound x) (cdr x))

(define (mul_interval x y)
    (define (case1 z)
        (and (> (upper_bound z) 0) (> (lower_bound z) 0))
    )
    (define (case2 z)
        (and (< (upper_bound z) 0) (< (lower_bound z) 0))
    )
    (define (case3 z)
        (not (or (case1 z) (case2 z)))
    )
    (let 
        (
            (x_up (upper_bound x))
            (x_lo (lower_bound x))
            (y_up (upper_bound y))
            (y_lo (lower_bound y))
        )
        (cond
            ((and (case1 x) (case1 y)) 
                (make_interval (* x_up y_up) (* x_lo y_lo)))
            ((and (case1 x) (case2 y))
                (make_interval (* x_lo y_up) (* x_up y_lo)))
            ((and (case1 x) (case3 y))
                (make_interval (* x_lo y_up) (* x_lo y_lo)))
            ((and (case2 x) (case1 y))
                (make_interval (* x_up y_lo) (* x_lo y_up)))
            ((and (case2 x) (case2 y))
                (make_interval (* x_lo y_lo) (* x_up y_up)))

            ((and (case2 x) (case3 y))
                (make_interval (* x_lo y_up) (* x_up y_up)))
            
            ((and (case3 x) (case1 y))
                (make_interval (* x_up y_lo) (* x_lo y_lo)))
            
            ((and (case3 x) (case2 y))
                (make_interval (* x_up y_lo) (* x_up y_up)))
            
            ((and (case3 x) (case3 y))
                (make_interval (min (* x_up y_lo) (* x_lo y_up))(max (* x_up y_up) (* x_lo y_lo))))
        )
    )
)



(define a (make_interval 4 5))
(define b (make_interval 2 3))
(mul_interval a b)
(define a1 (make_interval -4 5))
(define b1 (make_interval 2 3))
(mul_interval a1 b1)
(define a2 (make_interval 4 5))
(define b2 (make_interval -2 3))
(mul_interval a2 b2)
(define a3 (make_interval -4 -3))
(define b3 (make_interval 2 3))
(mul_interval a3 b3)
(define a4 (make_interval 4 5))
(define b4 (make_interval -3 -2))
(mul_interval a4 b4)
(define a5 (make_interval -4 -3))
(define b5 (make_interval -3 -2))
(mul_interval a5 b5)
(define a6 (make_interval -4 -3))
(define b6 (make_interval -3 1))
(mul_interval a6 b6)
(define a7 (make_interval -4 3))
(define b7 (make_interval -3 -2))
(mul_interval a7 b7)
(define a8 (make_interval -4 3))
(define b8 (make_interval -3 1))
(mul_interval a8 b8)

````````````````````````````````````````
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
(mcons 8 15)
(mcons -12 15)
(mcons -10 15)
(mcons -12 -6)
(mcons -15 -8)
(mcons 6 12)
(mcons 9 12)
(mcons 8 12)
(mcons -9 12)

