; Exercise 3.18: Write a procedure that examines a list and determines whether it contains a cycle, that is, whether a program that tried to find the end of the list by taking successive cdrs would go into an infinite loop. Exercise 3.13 constructed such lists.
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(define (cycle? x)
  (define marked_list '())
  (define (cycle_inner x)
    (cond
      ((null? x) false) 
      ((member x marked_list) true)
      (else (begin (set! marked_list (cons x marked_list)) 
                   (cycle_inner (cdr x)))) 
    )
  )
  (cycle_inner x)
)
(cycle? (list 'a 'b 'c 'd))
(cycle? z)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; #f
; #t
; > 