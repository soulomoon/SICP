; After considerable work, Alyssa P. Hacker delivers her finished system. Several years later, after she has forgotten all about it, she gets a frenzied call from an irate user, Lem E. Tweakit. It seems that Lem has noticed that the formula for parallel resistors can be written in two algebraically equivalent ways:
; R1R2R1+R2
; R1R2R1+R2
; and
; 11/R1+1/R2.
; 11/R1+1/R2.
; He has written the following two programs, each of which computes the parallel-resistors formula differently:

; (define (par1 r1 r2)
;   (div-interval 
;    (mul-interval r1 r2)
;    (add-interval r1 r2)))

; (define (par2 r1 r2)
;   (let ((one (make-interval 1 1)))
;     (div-interval 
;      one
;      (add-interval 
;       (div-interval one r1) 
;       (div-interval one r2)))))
; Lem complains that Alyssa’s program gives different answers for the two ways of computing. This is a serious complaint.

; Exercise 2.14: Demonstrate that Lem is right. Investigate the behavior of the system on a variety of arithmetic expressions. Make some intervals AA and BB, and use them in computing the expressions A/AA/A and A/BA/B. You will get the most insight by using intervals whose width is a small percentage of the center value. Examine the results of the computation in center-percent form (see Exercise 2.12).
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
(define (sub_interval x y) 
    (make_interval
        (- (upper_bound x) (lower_bound y))
        (- (lower_bound x) (upper_bound y))
    )
)

(define (add_interval x y)
  (make_interval (+ (lower_bound x) 
                    (lower_bound y))
                 (+ (upper_bound x) 
                    (upper_bound y))))

(define (par1 r1 r2)
  (div_interval 
   (mul_interval r1 r2)
   (add_interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make_interval 1 1)))
    (div_interval 
     one
     (add_interval 
      (div_interval one r1) 
      (div_interval one r2)))))

(define a (make_center_percentage 1 0.1))
(define b (make_center_percentage 2 0.2))


(display "here is the difference")
(newline)
(par1 a b)
(par2 a b)

(newline)
(display "center:")
(newline)

(center (par1 a b))
(center (par2 a b))

(newline)
(display "center:")
(newline)

(percentage (par1 a b))
(percentage (par2 a b))

(display "it is because the A/A is not 1 for interval, so you don't simplify the equation to r1r2/(r1+r2)")
(display "~~~~~~~~~~~~~~~~~~~~~~~~~~~")
(newline)

(define p (div_interval a a))
(define q (div_interval a b))

(center p)
(center q)

(percentage p)
(percentage q)

````````````````````````````````````````````````````````````````````````````````
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
here is the difference
(mcons 0.4114285714285715 1.056)
(mcons 0.576 0.7542857142857143)

center:
0.7337142857142858
0.6651428571428572

center:
0.4392523364485981
0.1340206185567011
it is because the A/A is not 1 for interval, so you don't simplify the equation to r1r2/(r1+r2)~~~~~~~~~~~~~~~~~~~~~~~~~~~
1.0202020202020203
0.53125
0.19801980198019797
0.29411764705882354
> 

