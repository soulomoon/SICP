; Exercise 2.27: Modify your reverse procedure of Exercise 2.18 to produce a deep-reverse procedure that takes a list as argument and returns as its value the list with its elements reversed and with all sublists deep-reversed as well. For example,
(define (deep-reverse l)
    (cond 
        (
            (null? (cdr l))
            (if (pair? (car l))
                (list (deep-reverse (car l)))
                l
            )
        )
        (
            (pair? (car l))
            (append 
                (deep-reverse (cdr l)) 
                (list (deep-reverse (car l)))
            )
        )
        (
            else 
            (append (deep-reverse (cdr l)) (list (car l)))
        )
    )
)

(define (deep-reverse-two l)
    (define (get_new_car x)
        (let ((cr (car x)))
            (list
                (if (pair? cr)
                    (deep-reverse-two cr)
                    cr
                )
            )
        )
    )
    (if (null? (cdr l))
        (get_new_car l)
        (append (deep-reverse-two (cdr l)) (get_new_car l))
    )
)

(define x 
  (list (list 1 2) (list 3 4)))

x
; ((1 2) (3 4))

(reverse x)
; ((3 4) (1 2))

(deep-reverse x)
(deep-reverse-two x)
; ((4 3) (2 1))


```````````````````````````````````````
Welcome to DrRacket, version 6.7 [3m].
Language: SICP (PLaneT 1.18); memory limit: 128 MB.
{mcons {mcons 1 {mcons 2 '()}} {mcons {mcons 3 {mcons 4 '()}} '()}}
{mcons {mcons 3 {mcons 4 '()}} {mcons {mcons 1 {mcons 2 '()}} '()}}
{mcons {mcons 4 {mcons 3 '()}} {mcons {mcons 2 {mcons 1 '()}} '()}}
{mcons {mcons 4 {mcons 3 '()}} {mcons {mcons 2 {mcons 1 '()}} '()}}
> 