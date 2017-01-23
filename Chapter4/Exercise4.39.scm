; Exercise 4.39: Does the order of the restrictions in the multiple-dwelling procedure affect the answer? Does it affect the time to find an answer? If you think it matters, demonstrate a faster program obtained from the given one by reordering the restrictions. If you think it does not matter, argue your case.

#lang swindle
(provide test-time1)
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
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
      (distinct? (list baker cooper fletcher 
                       miller smith)))
    (require (> miller cooper))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require 
      (not (= (abs (- fletcher cooper)) 1)))
    (require
      (not (= (abs (- smith fletcher)) 1)))
    (require (not (= baker 5)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))
(define (runtime) (current-milliseconds))
(define (report start_time)
  (- (runtime) start_time))

(collect-garbage)  
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
; (display (test-time 200))
(define test-time1 test-time)
; (module test-time1 swindle
;   (provide test-time1)

; (define test-time1 test-time)
; )
; it does not effect the answer
; and it does effect the speed
; if each require take about the same time. the above would be the bestsolution
; here we should see distinct is o(n^2) others are o(1), but since there are only 5 of them , it does not make a huge difference, and the times it takes for the first is depend on the number of the list, 
; and each require would effect the latter require to succed or not, so it is actually a harder math problem, which i wish not spending too much time on it.