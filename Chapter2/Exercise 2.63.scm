; Exercise 2.63: Each of the following two procedures converts a binary tree to a list.

; (define (tree->list-1 tree)
;   (if (null? tree)
;       '()
;       (append 
;        (tree->list-1 
;         (left-branch tree))
;        (cons (entry tree)
;              (tree->list-1 
;               (right-branch tree))))))

; (define (tree->list-2 tree)
;   (define (copy-to-list tree result-list)
;     (if (null? tree)
;         result-list
;         (copy-to-list 
;          (left-branch tree)
;          (cons (entry tree)
;                (copy-to-list 
;                 (right-branch tree)
;                 result-list)))))
;   (copy-to-list tree '()))
; Do the two procedures produce the same result for every tree? If not, how do the results differ? What lists do the two procedures produce for the trees in Figure 2.16?
; Do the two procedures have the same order of growth in the number of steps required to convert a balanced tree with nn elements to a list? If not, which one grows more slowly?
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

  (define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? 
          x 
          (left-branch set)))
        ((> x (entry set))
         (element-of-set? 
          x 
          (right-branch set)))))


(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree 
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

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
; 1 yes because they are ordered by size
(define nulltree (list nil nil nil))
(define a (list 7 (list 3 (list 1 nil nil) (list 5 nil nil)) (list 9 nil (list 11 nil nil))))
(display a)
(newline)
(display (tree->list-1 a))
(newline)
(display (tree->list-2 a))
(newline)

; 2 first between o(n) to o(n^2) second o(n)


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; (7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))
; (1 3 5 7 9 11)
; (1 3 5 7 9 11)
; > 