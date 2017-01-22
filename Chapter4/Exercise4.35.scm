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
(print (amb-collect (a-pythagorean-triple-between 1 100)))


; Welcome to DrRacket, version 6.7 [3m].
; Language: swindle, with debugging; memory limit: 128 MB.
; ((3 4 5) (5 12 13) (6 8 10) (7 24 25) (8 15 17) (9 12 15) (9 40 41) (10 24 26) (11 60 61) (12 16 20) (12 35 37) (13 84 85) (14 48 50) (15 20 25) (15 36 39) (16 30 34) (16 63 65) (18 24 30) (18 80 82) (20 21 29) ...)
; > 