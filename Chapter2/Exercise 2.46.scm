; Exercise 2.46: A two-dimensional vector vv running from the origin to a point can be represented as a pair consisting of an xx-coordinate and a yy-coordinate. Implement a data abstraction for vectors by giving a constructor make-vect and corresponding selectors xcor-vect and ycor-vect. In terms of your selectors and constructor, implement procedures add-vect, sub-vect, and scale-vect that perform the operations vector addition, vector subtraction, and multiplying a vector by a scalar:
; (x1,y1)+(x2,y2)(x1,y1)−(x2,y2)s⋅(x,y)===(x1+x2,y1+y2),(x1−x2,y1−y2),(sx,sy).

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



(define a (make_vect 1 2))


(define (multiplay . args) 
    (for-each (lambda (x) (display x) (newline)) args)
)

(multiplay 
            (add_vect a a) 
            (sub_vect a a)
            (scale_vect a 10)
            )
            
Welcome to DrRacket, version 6.7 [3m].
Language: SICP (PLaneT 1.18); memory limit: 128 MB.
(2 4)
(0 0)
(10 20)
> 