; Exercise 2.66: Implement the lookup procedure for the case where the set of records is structured as a binary tree, ordered by the numerical values of the keys.
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
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
(define (key record) (car record))
(define (lookup_list given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key 
                 (key (car set-of-records)))
         (car set-of-records))
        (else 
         (lookup_list given-key 
                 (cdr set-of-records)))))


(define (lookup given-key set-of-tree-records)
    (let
        (
            (set_of_list_records (tree->list-2 set-of-tree-records))
        )
        (lookup_list given-key set_of_list_records)
    )
)
(define a (list 
            (list 1 'a) 
            (list 2 'b) 
            (list 3 'c) 
          )
)
(define b (list->tree a))

(display (lookup_list 2 a))(newline)
(display (lookup 2 b))

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; (2 b)
; (2 b)
; > 