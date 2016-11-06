; Exercise 1.39: A continued fraction representation of the tangent function was published in 1770 by the German mathematician J.H. Lambert:
; tanx=x1-x23-x25-...,
; tan⁡x=x1−x23−x25−…,
; where xx is in radians. Define a procedure (tan-cf x k) that computes an approximation to the tangent function based on Lambert’s formula. k specifies the number of terms to compute, as in Exercise 1.37.

#lang planet neil/sicp


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

(define (tan-cf x k)
    (define (d g)
        (+ (* g 2) 1)
    )

    (define (n g)
        (if (= g 1) x (- 0 (* x x)))
    )
    (cont_frac_iter n d k)
)


(tan-cf 1.0 10)
(tan-cf 1.0 100)
(tan-cf 1.0 1000)
(tan-cf 1.0 10000)

```````````````````````````````````
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
0.3579073840656693
0.3579073840656693
0.3579073840656693
0.3579073840656693
>