
; Exercise 1.45: We saw in 1.3.3 that attempting to compute square roots by naively finding a fixed point of y↦x/yy↦x/y does not converge, and that this can be fixed by average damping. The same method works for finding cube roots as fixed points of the average-damped y↦x/y2y↦x/y2. Unfortunately, the process does not work for fourth roots—a single average damp is not enough to make a fixed-point search for y↦x/y3y↦x/y3 converge. On the other hand, if we average damp twice (i.e., use the average damp of the average damp of y↦x/y3y↦x/y3) the fixed-point search does converge. Do some experiments to determine how many average damps are required to compute nthnth roots as a fixed-point search based upon repeated average damping of y↦x/yn-1y↦x/yn-1. Use this to implement a simple procedure for computing nthnth roots using fixed-point, average-damp, and the repeated procedure of Exercise 1.43. Assume that any arithmetic operations you need are available as primitives.
#lang planet neil/sicp
(define (average x y) (/ (+ x y) 2))
(define (repeated f k)
    (define (compose f g)
        (lambda (x) (f (g x)))
    )
    (if (= 0 k)
        (lambda (x) x)
        (compose f (repeated f (- k 1)))
    )
)

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (damp_twice f)
    ((repeated average-damp 2) f)
)



(define tolerance 0.00001)
(define (fixed_point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (cubic x)
    (* x x x)
)
(define (cubic_root x)
  (fixed_point
   (average-damp
    (lambda (y) (/ x y y)))
   1.0))

(define (expn_root x n)
    (define (expn a x)
        (if (= x 1)
            a
            (* (expn a (- x 1)) a)
        )
    )
    (define (damp f n)
        ((repeated average-damp n) f)
    )
    (fixed_point
        (damp
            (lambda (y) (/ x (expn y (- n 1))))
            (- n 1)
        )
        1.0
    )
)



(expn_root 1024 10)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
2.0004987354916155
>
