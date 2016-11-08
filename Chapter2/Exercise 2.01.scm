; Exercise 2.1: Define a better version of make-rat that handles both positive and negative arguments. Make-rat should normalize the sign so that if the rational number is positive, both the numerator and denominator are positive, and if the rational number is negative, only the numerator is negative.
#lang planet neil/sicp
(define (make-rat n d)
    (let ((g (gcd n d)))
        (let(
                (new_n (/ n g)) 
                (new_d (/ d g))
                (positive_bo (> (* n d) 0))
            )
            (cond
                (positive_bo (cons (abs new_n) (abs new_d)))
                (else (cons (- 0 (abs new_n)) (abs new_d)))
            )
        )
    )
)
(make-rat 10 5)
(make-rat -109 5)
(make-rat 100 -25)
(make-rat -14 -2)
(make-rat 5242 42342)


`````````````````````````````````````````````````
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
(mcons 2 1)
(mcons -109 5)
(mcons -4 1)
(mcons 7 1)
(mcons 2621 21171)
> 