; Exercise 1.46: Several of the numerical methods described in this chapter are instances of an extremely general computational strategy known as iterative improvement. Iterative improvement says that, to compute something, we start with an initial guess for the answer, test if the guess is good enough, and otherwise improve the guess and continue the process using the improved guess as the new guess. Write a procedure iterative-improve that takes two procedures as arguments: a method for telling whether a guess is good enough and a method for improving a guess. Iterative-improve should return as its value a procedure that takes a guess as argument and keeps improving the guess until it is good enough. Rewrite the sqrt procedure of 1.1.7 and the fixed-point procedure of 1.3.3 in terms of iterative-improve.
#lang planet neil/sicp

(define (iterative_improve good_enough? improve)
    (lambda (x)
        (define (iter x)
            (if (good_enough? x)
                x
                (iter (improve x))
            )
        )
        (iter x)
    )
)
(define tolerance 0.001)
;sqrt procedure
(define (sqrt y)
    (define (sqrt_good_enough? x)
        (if (< (abs (- (* x x) y)) tolerance)
            true
            false
        )
    )
    (define (improve x)
        (/ (+ (/ y x) x) 2)
    )
    ((iterative_improve sqrt_good_enough? improve) 1.0)
)

;fixed-point
(define (fixed-point f first-guess)
    (define (close-enough? x)
        (< (abs (- x (f x))) tolerance))

    (define (improve x)
        (f x)
    )
    ((iterative_improve close-enough? improve) 1.0)
)



(sqrt 4)
(fixed-point cos 1.0)

``````````````````````````````````
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
2.0000000929222947
0.7395672022122561
>