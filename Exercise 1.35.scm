; Exercise 1.35: Show that the golden ratio φφ (1.2.2) is a fixed point of the transformation x↦1+1/xx↦1+1/x, and use this fact to compute φφ by means of the fixed-point procedure.
#lang planet neil/sicp
(define tolerence 0.00001)

(define (fixedPoint f first_guess)
    (define (closeEnough x y)
        (let ((dis (abs (- x y))))
            (if (< dis tolerence)
                true
                false
            )
        )
    )
    (define (try old_guess)
        (let ((new_guess (f old_guess)))
            (if (closeEnough new_guess old_guess)
                new_guess
                (try new_guess))
        )
    )
    (try first_guess)
)

(define (golden_ratio_approximation x)
    (+ 1 (/ 1.0 x)))


(fixedPoint golden_ratio_approximation 2)
(fixedPoint golden_ratio_approximation 112)
1.6180327868852458
1.61803281074865
