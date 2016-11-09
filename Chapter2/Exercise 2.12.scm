; After debugging her program, Alyssa shows it to a potential user, who complains that her program solves the wrong problem. He wants a program that can deal with numbers represented as a center value and an additive tolerance; for example, he wants to work with intervals such as 3.5 ±± 0.15 rather than [3.35, 3.65]. Alyssa returns to her desk and fixes this problem by supplying an alternate constructor and alternate selectors:

; (define (make-center-width c w)
;   (make-interval (- c w) (+ c w)))

; (define (center i)
;   (/ (+ (lower-bound i) 
;         (upper-bound i)) 
;      2))

; (define (width i)
;   (/ (- (upper-bound i) 
;         (lower-bound i)) 
;      2))
; Unfortunately, most of Alyssa’s users are engineers. Real engineering situations usually involve measurements with only a small uncertainty, measured as the ratio of the width of the interval to the midpoint of the interval. Engineers usually specify percentage tolerances on the parameters of devices, as in the resistor specifications given earlier.

; Exercise 2.12: Define a constructor make-center-percent that takes a center and a percentage tolerance and produces the desired interval. You must also define a selector percent that produces the percentage tolerance for a given interval. The center selector is the same as the one shown above.
#lang planet neil/sicp

(define (make_interval a b) (cons a b))
(define (upper_bound x) (car x))
(define (lower_bound x) (cdr x))

(define (make_center_width c w)
  (make_interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower_bound i) 
        (upper_bound i)) 
     2))

(define (width i)
  (/ (- (upper_bound i) 
        (lower_bound i)) 
     2))

(define (make_center_percentage c p)
    (make_center_width c (* c p))
)

(define (percentage x)
    (let
        (
            (c (center x))
        )
        (if
            (= c (upper_bound x))
            0
            (/ (- (lower_bound x) c) c)
        ) 
    )
)

(define a (make_center_percentage 10 (/ 1 10)))

(center a)
(percentage a)


``````````````````````````````
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
10
1/10
> 