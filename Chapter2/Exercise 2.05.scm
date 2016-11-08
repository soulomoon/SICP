; Exercise 2.5: Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations if we represent the pair aa and bb as the integer that is the product 2a3b2a3b. Give the corresponding definitions of the procedures cons, car, and cdr.
#lang planet neil/sicp
(define (pow base n)
    (if (= n 0)
        1
        (* base (pow base (- n 1)))
    )
)
(define (root x base)
    (if (= x 1)
        0
        (+ (root (/ x base) base) 1)
    )
)

(define (cons a b)
    (* (pow 2 a) (pow 3 b))
)

(define (car c)
    (if (= 1 (gcd c 3)) 
        (root c 2)
        (car (/ c 3))
    )
)

(define (cdr c)
    (if (= 1 (gcd c 2))
        (root c 3)
        (cdr (/ c 2))
    )
)
; for testing
(define (show x y)
    (display x)
    (display " ")
    (display y)
    (newline)
    
    (define a (cons x y))
    (display (car a))
    (newline)
    (display (cdr a))
    (newline)
    (display "```````````````````````")
    (newline)

) 


(show 1 2)
(show 21 2)
(show 1 22)
(show 1 22)
(show 121 32)
(show 131 22)
(show 11 212)
``````````````````````````````````````````````````````````
Welcome to DrRacket, version 6.6 [3m].
Language: planet neil/sicp, with debugging; memory limit: 128 MB.
1 2
1
2
```````````````````````
21 2
21
2
```````````````````````
1 22
1
22
```````````````````````
1 22
1
22
```````````````````````
121 32
121
32
```````````````````````
131 22
131
22
```````````````````````
11 212
11
212
```````````````````````