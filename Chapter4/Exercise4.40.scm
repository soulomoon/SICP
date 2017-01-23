; Exercise 4.40: In the multiple dwelling problem, how many sets of assignments are there of people to floors, both before and after the requirement that floor assignments be distinct? It is very inefficient to generate all possible assignments of people to floors and then leave it to backtracking to eliminate them. For example, most of the restrictions depend on only one or two of the person-floor variables, and can thus be imposed before floors have been selected for all the people. Write and demonstrate a much more efficient nondeterministic procedure that solves this problem based upon generating only those possibilities that are not already ruled out by previous restrictions. (Hint: This will require a nest of let expressions.)

#lang swindle
(require (file "/Users/soulomoon/git/SICP/Chapter4/Exercise4.39.scm"))

(define (square x) (* x x))
(define (require p)
  (if (not p) (amb)))
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((fletcher (amb 1 2 3 4 5)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (not (= cooper 1)))
      (require 
        (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
        (require
          (not (= (abs (- smith fletcher)) 1)))
        (let ((baker (amb 1 2 3 4 5)))
          (require (not (= baker 5)))
          (let ((miller (amb 1 2 3 4 5)))
            (require (> miller cooper))
            (require
              (distinct? (list baker cooper fletcher 
                               miller smith)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))
     
;(for-each (lambda (x) (display x)(newline )) (amb-collect (multiple-dwelling)))

(collect-garbage)
(define (runtime) (current-milliseconds))
(define (report start_time)
  (- (runtime) start_time))  
(define (test-time n)
  (let ((starttime 0)
        (result 0))
    (define (iter n)
      (if (< n 0)
          result
          (begin
            (set! starttime (runtime))
            (amb-collect (multiple-dwelling))
            (set! result (+ result (report starttime)))
            (iter (- n 1)))))
    (iter n)))
(collect-garbage)
(display (test-time 200))
(newline)
(collect-garbage)
(display (test-time1 200))

; the new impelementation only need 1/10 times

; Welcome to DrRacket, version 6.7 [3m].
; Language: swindle, with debugging; memory limit: 512 MB.
; 517
; 4679
; > 