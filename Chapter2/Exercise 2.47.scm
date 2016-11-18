; Exercise 2.47: Here are two possible constructors for frames:

; (define (make-frame origin edge1 edge2)
;   (list origin edge1 edge2))

; (define (make-frame origin edge1 edge2)
;   (cons origin (cons edge1 edge2)))
; For each constructor supply the appropriate selectors to produce an implementation for frames.

(define (make_frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (make_frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin_frame frame)
    (car frame)
)
(define (edge1_frame frame)
    (cadr frame)
)
(define (edge2_frame1 frame)
    (caddr frame)
)
(define (edge2_frame2 frame)
    (cddr frame)
)

(define a (make_frame1 1 2 3))
(define b (make_frame2 1 2 3))

(define (multiplay . args) 
    (for-each (lambda (x) (display x) (newline)) args)
)
(multiplay 
    a
    (origin_frame a) 
    (edge1_frame a) 
    (edge2_frame1 a) 
    b 
    (origin_frame b) 
    (edge1_frame b) 
    (edge2_frame2 b) 
)

Welcome to DrRacket, version 6.7 [3m].
Language: SICP (PLaneT 1.18); memory limit: 128 MB.
(1 2 3)
1
2
3
(1 2 . 3)
1
2
3
> 
