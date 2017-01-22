; Exercise 4.35: Write a procedure an-integer-between that returns an integer between two given bounds. This can be used to implement a procedure that finds Pythagorean triples, i.e., triples of integers (i,j,k)(i,j,k) between the given bounds such that i≤ji≤j and i2+j2=k2i2+j2=k2, as follows:

; (define (a-pythagorean-triple-between low high)
;   (let ((i (an-integer-between low high)))
;     (let ((j (an-integer-between i high)))
;       (let ((k (an-integer-between j high)))
;         (require (= (+ (* i i) (* j j)) 
;                     (* k k)))
;         (list i j k)))))

#lang swindle


(define (square x) (* x x))
(define (require p)
  (if (not p) (amb)))
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))
 
(define (a-pythagorean-triple-between low high)
  (let* ((i (an-integer-between low high))
         (j (an-integer-between i high))
         (k (an-integer-between j high)))
    (require (= (+ (square i) (square j))
                (square k)))
    (list i j k)))
(print (a-pythagorean-triple-between 1 20))