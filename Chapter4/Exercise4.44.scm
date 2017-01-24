; Exercise 4.44: Exercise 2.42 described the “eight-queens puzzle” of placing queens on a chessboard so that no two attack each other. Write a nondeterministic program to solve this puzzle.
#lang swindle
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) 
       (an-element-of (cdr items))))
       
(define (require p)
  (if (not p) (amb)))
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (num) (amb 1 2 3 4 5 6 7 8))
(define all_rows (list 1 2 3 4 5 6 7 8))
(define all_queens (list 'q1 'q2 'q3 'q4 'q5 'q6 'q7 'q8))
(define (position x) (list x (num)))
(define (solve-queens)
  (let* ((queens
          (map 
            (lambda (r queen) 
              (list queen (position r)))
            all_rows
            all_queens))
          (cols (map caadr queens)))
          (require 
            (distinct? cols))
          queens))

(display (solve-queens))


; Welcome to DrRacket, version 6.7 [3m].
; Language: swindle, with debugging; memory limit: 2048 MB.
; ((q1 (1 1)) (q2 (2 1)) (q3 (3 1)) (q4 (4 1)) (q5 (5 1)) (q6 (6 1)) (q7 (7 1)) (q8 (8 1)))
; > 