; Exercise 2.26: Suppose we define x and y to be two lists:

(define x (list 1 2 3))
(define y (list 4 5 6))
; What result is printed by the interpreter in response to evaluating each of the following expressions:

(append x y)
(cons x y)
(list x y)
;(list x y) = (cons x (cons y nil))
(cons x (cons y nil))
````````````````````````````````````
Welcome to DrRacket, version 6.7 [3m].
Language: SICP (PLaneT 1.18); memory limit: 128 MB.
{mcons 1 {mcons 2 {mcons 3 {mcons 4 {mcons 5 {mcons 6 '()}}}}}}
{mcons
 {mcons 1 {mcons 2 {mcons 3 '()}}}
 {mcons 4 {mcons 5 {mcons 6 '()}}}}
{mcons
 {mcons 1 {mcons 2 {mcons 3 '()}}}
 {mcons {mcons 4 {mcons 5 {mcons 6 '()}}} '()}}
{mcons
 {mcons 1 {mcons 2 {mcons 3 '()}}}
 {mcons {mcons 4 {mcons 5 {mcons 6 '()}}} '()}}
> 