; Exercise 1.43: If ff is a numerical function and nn is a positive integer, then we can form the nthnth repeated application of ff, which is defined to be the function whose value at xx is f(f(...(f(x))...))f(f(...(f(x))...)). For example, if ff is the function x↦x+1x↦x+1, then the nthnth repeated application of ff is the function x↦x+nx↦x+n. If ff is the operation of squaring a number, then the nthnth repeated application of ff is the function that raises its argument to the 2n-th2n-th power. Write a procedure that takes as inputs a procedure that computes ff and a positive integer nn and returns the procedure that computes the nthnth repeated application of ff. Your procedure should be able to be used as follows:

; ((repeated square 2) 5)
; 625
; Hint: You may find it convenient to use compose from Exercise 1.42.
#lang planet neil/sicp

(define (square x)
    (* x x)
)

(define (repeated_recursive f k)
    (define (compose f g)
        (lambda (x) (f (g x)))
    )
    (if (= 0 k)
        (lambda (x) x)
        (compose f (repeated_recursive f (- k 1)))
    )
)

(define (repeated f k)
    (define (compose f g)
        (lambda (x) (f (g x)))
    )
    (define (next g)
        (compose f g)
    )
    (define (iter n result)
        (if (= k n)
            result
            (iter (+ n 1) (next result))
        )
    )
    (iter 1 f)
)

((repeated_recursive square 2) 5)
((repeated square 2) 5)
````````````````````````````````````````````````````````````
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
625
625
>