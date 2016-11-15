; Exercise 2.28: Write a procedure fringe that takes as argument a tree (represented as a list) and returns a list whose elements are all the leaves of the tree arranged in left-to-right order. For example,

(define (fringe l)
    (define (get_new_car x)
        (let ((cr (car x)))
            (if (pair? cr)
                (fringe cr)
                (list cr)
            )
        )
    )
    (if (null? (cdr l))
        (get_new_car l)
        (append (get_new_car l) (fringe (cdr l)))
    )
)





(define x 
  (list (list 1 2) (list 3 4)))


(fringe x)
; (1 2 3 4)


(fringe (list x x))
; (1 2 3 4 1 2 3 4)

`````````````````````````````````````````````````````````````
Welcome to DrRacket, version 6.7 [3m].
Language: SICP (PLaneT 1.18); memory limit: 128 MB.
{mcons 1 {mcons 2 {mcons 3 {mcons 4 '()}}}}
{mcons
 1
 {mcons 2 {mcons 3 {mcons 4 {mcons 1 {mcons 2 {mcons 3 {mcons 4 '()}}}}}}}}
> 