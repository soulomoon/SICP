
; Exercise 2.39: Complete the following definitions of reverse (Exercise 2.18) in terms of fold-right and fold-left from Exercise 2.38:

; (define (reverse sequence)
;   (fold-right 
;    (lambda (x y) ⟨??⟩) nil sequence))

; (define (reverse sequence)
;   (fold-left 
;    (lambda (x y) ⟨??⟩) nil sequence))


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op 
                      initial 
                      (cdr sequence)))))

(define (reverse_right sequence)
  (fold-right 
   (lambda (x y) (append y (list x))) nil sequence))

(define (reverse_left sequence)
  (fold-left 
   (lambda (x y) (append (list y) x)) nil sequence))

(display (list 1 4 9 16 25))
(newline)
(display (reverse_right (list 1 4 9 16 25)))
(newline)
(display (reverse_left (list 1 4 9 16 25)))
(newline)
; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; (1 4 9 16 25)
; (25 16 9 4 1)
; (25 16 9 4 1)
; > 