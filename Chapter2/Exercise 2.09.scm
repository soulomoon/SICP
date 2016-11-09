; Exercise 2.9: The width of an interval is half of the difference between its upper and lower bounds. The width is a measure of the uncertainty of the number specified by the interval. For some arithmetic operations the width of the result of combining two intervals is a function only of the widths of the argument intervals, whereas for others the width of the combination is not a function of the widths of the argument intervals. Show that the width of the sum (or difference) of two intervals is a function only of the widths of the intervals being added (or subtracted). Give examples to show that this is not true for multiplication or division.
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

(define (add_interval x y)
    (make_interval
        (+ (upper_bound x) (upper_bound y))
        (+ (lower_bound x) (lower_bound y))
    )
)

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

(define (width_interval x)
    (/ (- (lower_bound X) (upper_bound X)) 2)
)


(define a 
    (make_interval 1 2)
)

(define b 
    (make_interval 2 3)
)

(+ (width_interval a) (width_interval b))
(width_interval (sub_interval a b))
(display "``````````````````````````")
(newline)
(+ (width_interval a) (width_interval b))
(width_interval (add_interval a b))
(display "``````````````````````````")
(newline)
(+ (width_interval a) (width_interval b))
(let (
        (width_a (width_interval a))
        (width_b (width_interval b))
        (cent_a (/ (+ (upper_bound a) (lower_bound a)) 2))
        (cent_b (/ (+ (upper_bound b) (lower_bound b)) 2))
    )
    (+ (* width_a cent_b) (* width_b cent_a))
)
(width_interval (mul_interval a b))

````````````````````````````````````````````````````````````
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
1
1
``````````````````````````
1
1
``````````````````````````
1
2
2
> 

