; Exercise 1.40: Define a procedure cubic that can be used together with the newtons-method procedure in expressions of the form

; (newtons-method (cubic a b c) 1)
; to approximate zeros of the cubic x3+ax2+bx+cx3+ax2+bx+c.

#lang planet neil/sicp


(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (newtons-method g guess)

    (define (deriv g)
        (let ((dx 0.0001))

            (lambda (x)
                (/ (- (g (+ x dx)) (g x))
                dx))
        )
    )

    (define (newton-transform g)
        (lambda (x)
            (- x (/ (g x)
                    ((deriv g) x)))))
    (fixed-point
        (newton-transform g)
        guess
    )
)


(define (cubic a b c)
    (lambda (x) (+ (* x x x) (* a x x) (* b x) c))
)

(newtons-method (cubic 1 1 1) 1)

Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
-0.999999999999198
>