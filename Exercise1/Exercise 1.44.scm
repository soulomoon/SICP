; Exercise 1.44: The idea of smoothing a function is an important concept in signal processing. If ff is a function and dxdx is some small number, then the smoothed version of ff is the function whose value at a point xx is the average of f(x-dx)f(x-dx), f(x)f(x), and f(x+dx)f(x+dx). Write a procedure smooth that takes as input a procedure that computes ff and returns a procedure that computes the smoothed ff. It is sometimes valuable to repeatedly smooth a function (that is, smooth the smoothed function, and so on) to obtain the n-fold smoothed function. Show how to generate the n-fold smoothed function of any given function using smooth and repeated from Exercise 1.43.
#lang planet neil/sicp

(define (square x)
    (* x x)
)

(define (smooth f)
    (let ((dx 0.1))
        (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))
    )
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
        (if (= n k)
            result
            (iter (+ n 1) (next result))
        )
    )
    (iter 1 f)
)


(define (repeated_smooth f n)
    ((repeated smooth n) f)
)

(define (repeated_smooth_recursive f n)
    ((repeated_recursive smooth n) f)
)

((smooth square) 2)
((smooth (smooth square)) 2)
((smooth (smooth (smooth square))) 2)
((repeated_smooth square 3) 2)
((repeated_smooth_recursive square 3) 2)
```````````````````````````````````````````````````````````````
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
4.006666666666667
4.013333333333333
4.02
4.02
4.02
>