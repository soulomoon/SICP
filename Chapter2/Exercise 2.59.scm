; Exercise 2.59: Implement the union-set operation for the unordered-list representation of sets.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
        
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

(define (union_set set1 set2)
    (if
        (null? set1) 
        set2
        (union_set (cdr set1) (adjoin-set (car set1) set2))
    )
)

(define a '(1 2 a b))
(define b '(2 3 b c))

(display (intersection-set a b))(newline)
(display (union_set a b))(newline)
(display a)(newline)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; (2 b)
; (a 1 2 3 b c)
; (1 2 a b)
; > 