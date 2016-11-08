; Exercise 1.29: Simpson’s Rule is a more accurate method of numerical integration than the method illustrated above. Using Simpson’s Rule, the integral of a function ff between aa and bb is approximated as
; h3(y0+4y1+2y2+4y3+2y4+⋯+2yn−2+4yn−1+yn),
; h3(y0+4y1+2y2+4y3+2y4+⋯+2yn−2+4yn−1+yn),
; where h=(b−a)/nh=(b−a)/n, for some even integer nn, and yk=f(a+kh)yk=f(a+kh). (Increasing nn increases the accuracy of the approximation.) Define a procedure that takes as arguments ff, aa, bb, and nn and returns the value of the integral, computed using Simpson’s Rule. Use your procedure to integrate cube between 0 and 1 (with n=100n=100 and n=1000n=1000), and compare the results to those of the integral procedure shown above.

#lang planet neil/sicp
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x)
    (* x x x)
)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) 
     dx))


(define (integralEnhance f a b n)
    (define h
        (/ (- b a) n))
        
    (define (get-k x)
        (/ (- x a) h))
    
    (define (get-ratio k)
        (cond
        ((= (remainder k n) 0) 1)
        ((= (remainder k 2) 0) 2)
        (else 4)
        ))

    (define (fx x)
        (* (/ h 3) (get-ratio (get-k x)) (f x)))

    (define (get-next x)
        (+ x h))

    (sum fx a get-next b)
)

(integral cube 0 1 0.01)
(integralEnhance cube 0 1 100)
(integral cube 0 1 0.001)
(integralEnhance cube 0 1 1000)
(integralEnhance cube 0 1 2)

0.24998750000000042
1/4
0.249999875000001
1/4
1/4
