; Exercise 2.29: A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of a certain length, from which hangs either a weight or another binary mobile. We can represent a binary mobile using compound data by constructing it from two branches (for example, using list):

; (define (make-mobile left right)
;   (list left right))
; A branch is constructed from a length (which must be a number) together with a structure, which may be either a number (representing a simple weight) or another mobile:

; (define (make-branch length structure)
;   (list length structure))
;1 Write the corresponding selectors left-branch and right-branch, which return the branches of a mobile, and branch-length and branch-structure, which return the components of a branch.
; Using your selectors, define a procedure total-weight that returns the total weight of a mobile.
; A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied by its top-right branch (that is, if the length of the left rod multiplied by the weight hanging from that rod is equal to the corresponding product for the right side) and if each of the submobiles hanging off its branches is balanced. Design a predicate that tests whether a binary mobile is balanced.
; Suppose we change the representation of mobiles so that the constructors are
; (define (make-mobile left right)
;   (cons left right))

; (define (make-branch length structure)
;   (cons length structure))
; How much do you need to change your programs to convert to the new representation?
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))
;selectors
(define (left_branch mobile)
    (car mobile)
)
(define (right_branch mobile)
    (cadr mobile)
)

(define (get_structure branch)
    (cadr branch)
 )

(define (get_length branch)
    (car branch)
)

;total_weight
(define (total_weight mobile)
    (define (get_structure_size item)
        (if (pair? item)
            (total_weight item)
            item
        )
    )
    (let (
            (lefty (left_branch mobile))
            (righty (right_branch mobile))
        ) 
        (+ 
            (get_structure_size (get_structure lefty))
            (get_structure_size (get_structure righty))
        )
    )
)

;balanced
(define (mobile_balanced? mobile) 
    (define (get_weight structure)
        (if (pair? structure)
            (total_weight structure)
            structure
        )
    )
    (define (torque branch)
        (* (get_length branch) (get_weight (get_structure branch)))
    )
    (let (
            (lefty (left_branch mobile))
            (righty (right_branch mobile))
        ) 
        (= 
            (torque lefty)
            (torque righty)
        )
    )
)

(define (balanced? mobile)
    (if (pair? mobile)
        (and
            (mobile_balanced? mobile) 
            (balanced? (get_structure (left_branch mobile)))
            (balanced? (get_structure (right_branch mobile)))
        )
        true
    )
)



(define l (make-branch 3 2))
(define r (make-branch 2 3))
(define a (make-mobile l r))
(define b (make-mobile (make-branch 2 a) (make-branch 2 a)))
(left_branch a)
(right_branch a)
(get_structure (right_branch a))
(get_length (left_branch a))
(total_weight a)
(balanced? a)
(balanced? b)
``````````````````````````````````````````````````````````````````````````````
; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; {mcons 3 {mcons 2 '()}}
; {mcons 2 {mcons 3 '()}}
; 3
; 3
; 5
; #t
; #t
; > 