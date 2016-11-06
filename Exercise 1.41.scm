; Exercise 1.41: Define a procedure double that takes a procedure of one argument as argument and returns a procedure that applies the original procedure twice. For example, if inc is a procedure that adds 1 to its argument, then (double inc) should be a procedure that adds 2. What value is returned by

; (((double (double double)) inc) 5)
#lang planet neil/sicp


(define (double f)
    (lambda (x)
        (f (f x))
    )
)


((         (double double) inc) 5)
(((double (double double)) inc) 5)

``````````````````````````````````````````````````````````

Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
9
21
>