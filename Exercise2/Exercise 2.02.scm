; Exercise 2.2: Consider the problem of representing line segments in a plane. Each segment is represented as a pair of points: a starting point and an ending point. Define a constructor make-segment and selectors start-segment and end-segment that define the representation of segments in terms of points. Furthermore, a point can be represented as a pair of numbers: the xx coordinate and the yy coordinate. Accordingly, specify a constructor make-point and selectors x-point and y-point that define this representation. Finally, using your selectors and constructors, define a procedure midpoint-segment that takes a line segment as argument and returns its midpoint (the point whose coordinates are the average of the coordinates of the endpoints). To try your procedures, you’ll need a way to print points:
#lang planet neil/sicp
(define (print_point p)
  (newline)
  (display "(")
  (display (x_point p))
  (display ",")
  (display (y_point p))
  (display ")")
)

(define (make_point x y)
    (cons x y)
)

(define (x_point p)
    (car p)
)
(define (y_point p)
    (cdr p)
)

(define (make_segment p q)
    (cons p q)
)

(define (start_segment s)
    (car s)
)

(define (end_segment s)
    (cdr s)
)

(define (midpoint_segment s)
    (define (mid a b)
        (/ (+ a b) 2) 
    )
    (let 
        (
            (xs (x_point (start_segment s)))
            (ys (y_point (start_segment s)))
            (xe (x_point (end_segment s)))
            (ye (y_point (end_segment s)))
        )
        (make_point
            (mid xs xe)
            (mid ys ye)
        )
    )
)

(define a (make_point 10 5)) 
(define b (make_point 15 7))
(print_point a)
(print_point b)

(define s (make_segment a b))
(define p (midpoint_segment s))
(print_point p)

```````````````````````````````````````````````
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.

(10,5)
(15,7)
(25/2,6)
> 