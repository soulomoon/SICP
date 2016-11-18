; Exercise 2.48: A directed line segment in the plane can be represented as a pair of vectors—the vector running from the origin to the start-point of the segment, and the vector running from the origin to the end-point of the segment. Use your vector representation from Exercise 2.46 to define a representation for segments with a constructor make-segment and selectors start-segment and end-segment.
(define (make_vect x y)
    (list x y)
)

(define (xcor_vect vect) (car vect))
(define (ycor_vect vect) (cadr vect))
(define (add_vect vect1 vect2)
    (map + vect1 vect2)
)

(define (sub_vect vect1 vect2)
    (map - vect1 vect2)
)

(define (scale_vect vect n)
    (map (lambda (v) (* v n)) vect)
)

(define (make-segment vector1 vector2)
    (list vector1 vector2)
)

(define (start_segment seg)
    (car seg)
)
(define (end_segment seg)
    (cadr seg)
)


(define a (make_vect 1 2))
(define b (make_vect 2 3))

(define z (make-segment a b))
(define (multiplay . args) 
    (for-each (lambda (x) (display x) (newline)) args)
)
(multiplay 
            a
            b
            z
            "start_segment:"
            (start_segment z)
            "end_segment:"
            (end_segment z)
            )
; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; (1 2)
; (2 3)
; ((1 2) (2 3))
; start_segment:
; (1 2)
; end_segment:
; (2 3)
; > 
