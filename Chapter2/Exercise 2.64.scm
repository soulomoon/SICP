; Exercise 2.64: The following procedure list->tree converts an ordered list to a balanced binary tree. The helper procedure partial-tree takes as arguments an integer nn and list of at least nn elements and constructs a balanced tree containing the first nn elements of the list. The result returned by partial-tree is a pair (formed with cons) whose car is the constructed tree and whose cdr is the list of elements not included in the tree.


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

(define a (list->tree (list 1 2 3 4 5 6 7 8)))
(display a)
;1 Write a short paragraph explaining as clearly as you can how partial-tree works. Draw the tree produced by list->tree for the list (1 3 5 7 9 11).

; 1 set the size of left tree as  (quotient (- n 1) 2)), always equal or smaller by one than the half of (- n 1),
; 2 then call (partial-tree elts left-size), and suppose it returns a (cons lefttree, rest of the eles), (implementation details on procedure on the last part).
; 3 separate the the above results and take away the first of the eles as entry of the current point.
; 4 set the size of the right tree as (- n (+ left-size 1)), (equation: right-size + left-size + 1(current-point) = n, and right -size is at best biggier than left size by one or equal)
; 5 same as step 2
; 6 (cons (make-tree this-entry 
                    ;              left-tree 
                    ;              right-tree)
                    ;   remaining-elts)
; for remaining-eles, at the left most of the leave, it is full, that leave in step 2 take one away(left-size), then this entry get the second one, then the left get one too,it makes one sub blanced tree, and return as a (lefttree) with the (remaining-elts and proceed with the same procedure)





;2 What is the order of growth in the number of steps required by list->tree to convert a list of nn elements?

; 0(n)
; every elements is visit once, with (cons '() elts) at the leave or cdr in the rest entry to take a apart from the list