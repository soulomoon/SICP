; Exercise 2.65: Use the results of Exercise 2.63 and Exercise 2.64 to give Θ(n)Θ(n) implementations of union-set and intersection-set for sets implemented as (balanced) binary trees.107
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set_list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set_list 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set_list 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-set_list 
                          set1 
                          (cdr set2)))))))

(define (adjoin-set x set)
    (cond
        ((or (null? set) (= x (car set))) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin x (cdr set))))
    )
)

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define (union_set_list set1 set2)
    (cond 
        ((null? set1) set2)
        ((null? set2) set1)
        (
            else
            (let ((x1 (car set1)) (x2 (car set2)))
                (cond
                    ((= x1 x2) (cons x1 (union_set_list (cdr set1) (cdr set2))))
                    ((< x1 x2) (cons x1 (union_set_list (cdr set1) set2)))
                    ((> x1 x2) (cons x2 (union_set_list set1 (cdr set2))))
                )
            )
        )
    )
)

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

(define (union_set tree1 tree2)
    (let (
            (set1 (tree->list-2 tree1))
            (set2 (tree->list-2 tree2))
         )
         (let (
                (list_set (union_set_list set1 set2))
              )
              (list->tree list_set)
         )
    )
)

(define (intersection-set tree1 tree2)
    (let (
            (set1 (tree->list-2 tree1))
            (set2 (tree->list-2 tree2))
         )
         (let (
                (list_set (intersection-set_list set1 set2))
              )
              (list->tree list_set)
         )
    )
)
(define a (list->tree (list 1 2 3 4)))
(define b (list->tree (list 4 5 6 7 8)))
(display a)(newline)
(display b)(newline)
(display (tree->list-2 b))(newline)
(display (union_set a b))(newline)
(display (intersection-set a b))(newline)




; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; (2 (1 () ()) (3 () (4 () ())))
; (6 (4 () (5 () ())) (7 () (8 () ())))
; (4 5 6 7 8)
; (4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () (8 () ()))))
; (4 () ())
; > 