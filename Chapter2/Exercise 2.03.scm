; Exercise 2.3: Implement a representation for rectangles in a plane. (Hint: You may want to make use of Exercise 2.2.) In terms of your constructors and selectors, create procedures that compute the perimeter and the area of a given rectangle. Now implement a different representation for rectangles. Can you design your system with suitable abstraction barriers, so that the same perimeter and area procedures will work using either representation?

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

(define (segment_len s)
    (define (long a b)
        (sqrt (+ (* a a) (* b b))) 
    )
    (let 
        (
            (xs (x_point (start_segment s)))
            (xe (x_point (end_segment s)))
            (ys (y_point (start_segment s)))
            (ye (y_point (end_segment s)))
        )
        (long (- xs xe) (- ys ye))
    )
)

(define (height rectangle)
    (segment_len (car rectangle))
)

(define (width rectangle)
    (segment_len (cdr rectangle))
)

(define (rectangle_primeter rectangle)
    (* (+ (height rectangle) (width rectangle)) 2)
)

(define (rectangle_area rectangle)
    (* (height rectangle) (width rectangle))
)

; takes in two segment
(define (make_rectangle s v)
    (cons s v)
)
;takes in four point
(define (1make_rectangle a b c d)
    (let ((s (make_segment a b)) (v (make_segment b c)))
        (cons s v)
    )
)

(define a (make_point 0 0)) 
(define b (make_point 10 0))
(define c (make_point 10 10)) 
(define d (make_point 0 10))

(define s (make_segment a b))
(define v (make_segment c d))

(define rect1 (make_rectangle s v))
(define rect2 (1make_rectangle a b c d))

(rectangle_area rect1)
(rectangle_area rect2)

(rectangle_primeter rect1)
(rectangle_primeter rect2)


Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
100
100
40
40
> 