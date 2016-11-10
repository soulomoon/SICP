; Exercise 2.17: Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list:

#lang planet neil/sicp
; Exercise 2.17: Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list:
(define (last_pair l)
    (if (null? (cdr l))
        l
        (last_pair (cdr l))
    )
)

(last_pair (list 23 72 149 34))
```````````````````````````````````````
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
(mcons 34 '())
> 