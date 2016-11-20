; Exercise 2.62: Give a Θ(n)Θ(n) implementation of union-set for sets represented as ordered lists.


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-set 
                          set1 
                          (cdr set2)))))))

(define (adjoin-set x set)
    (cond
        ((or (null? set) (= x (car set))) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin x (cdr set))))
    )
)

(define (union_set set1 set2)
    (cond 
        ((null? set1) set2)
        ((null? set2) set1)
        (
            else
            (let ((x1 (car set1)) (x2 (car set2)))
                (cond
                    ((= x1 x2) (cons x1 (union_set (cdr set1) (cdr set2))))
                    ((< x1 x2) (cons x1 (union_set (cdr set1) set2)))
                    ((> x1 x2) (cons x2 (union_set set1 (cdr set2))))
                )
            )
        )
    )
)
; (define (adjoin-set x set)
;   (if (element-of-set? x set)
;       set
;       (cons x set)))

(define a '(1 2))
(define b '(2 3))

(display (intersection-set a b))(newline)
(display (union_set a b))(newline)
(display a)(newline)




; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; (2)
; (1 2 3)
; (1 2)
; > 