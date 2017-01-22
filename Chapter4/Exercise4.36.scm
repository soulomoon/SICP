; Exercise 4.36: Exercise 3.69 discussed how to generate the stream of all Pythagorean triples, with no upper bound on the size of the integers to be searched. Explain why simply replacing an-integer-between by an-integer-starting-from in the procedure in Exercise 4.35 is not an adequate way to generate arbitrary Pythagorean triples. Write a procedure that actually will accomplish this. (That is, write a procedure for which repeatedly typing try-again would in principle eventually generate all Pythagorean triples.)

; because it would nerver stop


#lang swindle


(define (square x) (* x x))
(define (require p)
  (if (not p) (amb)))
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) 
       (an-element-of (cdr items))))
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))


 
(define (a-pythagorean-triple-from high)
  (let* ((k (an-integer-starting-from high))
         (j (an-integer-between 1 k))
         (i (an-integer-between 1 j)))
        (require (= (+ (square i) (square j))
                (square k)))
    (list i j k)))


(print (a-pythagorean-triple-from 33))


; Welcome to DrRacket, version 6.7 [3m].
; Language: swindle, with debugging; memory limit: 128 MB.
; (16 30 34)
; > 