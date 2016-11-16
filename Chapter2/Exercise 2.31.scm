; Exercise 2.31: Abstract your answer to Exercise 2.30 to produce a procedure tree-map with the property that square-tree could be defined as
(define (square x) (* x x))


(define (tree-map term tree)
    (map 
        (lambda (sub_tree)
            (cond
                ((null? sub_tree) nil)
                ((not (pair? sub_tree)) (term sub_tree))
                (else (tree-map term sub_tree))
            )
        )
        tree
    )
)

(define (square-tree tree) 
  (tree-map square tree))


(display
    (square-tree
    (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
)

Welcome to DrRacket, version 6.7 [3m].
Language: SICP (PLaneT 1.18); memory limit: 128 MB.
(1 (4 (9 16) 25) (36 49))
> 