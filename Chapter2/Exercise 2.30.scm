; Exercise 2.30: Define a procedure square-tree analogous to the square-list procedure of Exercise 2.21. That is, square-tree should behave as follows:

; (square-tree
;  (list 1
;        (list 2 (list 3 4) 5)
;        (list 6 7)))
; (1 (4 (9 16) 25) (36 49))
; Define square-tree both directly (i.e., without using any higher-order procedures) and also by using map and recursion.

(define (square x) (* x x))
;directly
(define (square-tree tree)
    (cond 
        ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))
    )
)
(define (square_map tree)
    (map 
        (lambda (sub_tree)
            (cond
                ((null? sub_tree) nil)
                ((not (pair? sub_tree)) (square sub_tree))
                (else (square_map sub_tree))
            )
        )
        tree
    )
)


(display
    (square-tree
    (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
)
(newline)
(display
    (square_map
    (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
)

Welcome to DrRacket, version 6.7 [3m].
Language: SICP (PLaneT 1.18); memory limit: 128 MB.
(1 (4 (9 16) 25) (36 49))
(1 (4 (9 16) 25) (36 49))
> 