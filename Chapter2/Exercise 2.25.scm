; Exercise 2.25: Give combinations of cars and cdrs that will pick 7 from each of the following lists:

; (1 3 (5 7) 9)
; ((7))
; (1 (2 (3 (4 (5 (6 7))))))

;fist we build the lists
(define a (list 1 3 (list 5 7) 9))

(define b (list (list 7)))

(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;now we get the 7 out
(car (cdaddr a))

(caar b)

(cdr (cadadr (cadadr (cadadr c))))

```````````````````````````````````````````
Welcome to DrRacket, version 6.7 [3m].
Language: SICP (PLaneT 1.18); memory limit: 128 MB.
7
7
7
> 