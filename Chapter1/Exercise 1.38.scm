; Exercise 1.38: In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus Continuis, which included a continued fraction expansion for e-2e-2, where ee is the base of the natural logarithms. In this fraction, the NiNi are all 1, and the DiDi are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses your cont-frac procedure from Exercise 1.37 to approximate ee, based on Euler’s expansion.
#lang planet neil/sicp
(define (d x)
    (define (valued n)
        (if (or (= (remainder n 3) 1)
                (= (remainder n 3) 0)
            )
            (* n 2)
            1
            )
    )
    (valued (- x 1))
)

(define (n x) 1)

(define (cont_frac_iter n d k)
    (define (search_frac g result)
        (let ((new_result (/ (n g) (+ (d g) result))))
            (cond
                ((= g 0) result)
                (else (search_frac (- g 1) new_result))
            )
        )
    )
    (search_frac k 0)
)

(cont_frac_iter n d 100.0)

````````
2.859393841608222
>