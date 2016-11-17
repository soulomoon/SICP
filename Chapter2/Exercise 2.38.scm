; Exercise 2.38: The accumulate procedure is also known as fold-right, because it combines the first element of the sequence with the result of combining all the elements to the right. There is also a fold-left, which is similar to fold-right, except that it combines elements working in the opposite direction:

; (define (fold-left op initial sequence)
;   (define (iter result rest)
;     (if (null? rest)
;         result
;         (iter (op result (car rest))
;               (cdr rest))))
;   (iter initial sequence))
; What are the values of

; (fold-right / 1 (list 1 2 3))
; (fold-left  / 1 (list 1 2 3))
; (fold-right list nil (list 1 2 3))
; (fold-left  list nil (list 1 2 3))
; Give a property that op should satisfy to guarantee that fold-right and fold-left will produce the same values for any sequence.

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-left_2 op initial sequence)
    (if (null? sequence)
      initial
      (op 
          (fold-left_2 op 
                      initial
                      (cdr sequence))
          (car sequence))))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op 
                      initial 
                      (cdr sequence)))))

(display (fold-right / 1 (list 1 2 3)))
(newline)
;1/(2/(3/1))=3/2
(display (fold-left  / 1 (list 1 2 3)))
(newline)
;((1/1)/2)/3=1/6
(display (fold-right list nil (list 1 2 3)))
(newline)
; (1 (2 (3 ())))
(display (fold-left  list nil (list 1 2 3)))
(newline)
; (((() 1) 2) 3)
(display (fold-left_2  list nil (list 1 2 3)))

; associative law, actually it should be in and out instead of right and left

Welcome to DrRacket, version 6.7 [3m].
Language: SICP (PLaneT 1.18); memory limit: 128 MB.
3/2
1/6
(1 (2 (3 ())))
(((() 1) 2) 3)
(((() 3) 2) 1)
> 