; Exercise 3.64: Write a procedure stream-limit that takes as arguments a stream and a number (the tolerance). It should examine the stream until it finds two successive elements that differ in absolute value by less than the tolerance, and return the second of the two elements. Using this, we could compute square roots up to a given tolerance by

; (define (sqrt x tolerance)
;   (stream-limit (sqrt-stream x) tolerance))
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")
(define (average x y)
  (/ (+ x y) 2))

(define (stream-limit s tolerance)
  (let ((first-ele (stream-car s))
        (second-ele (stream-car (stream-cdr s)))
        (rest-stream (stream-cdr s)))
        (let ((differ (abs (- first-ele second-ele))))
              (if (< differ tolerance)
                  second-ele
                  (stream-limit rest-stream tolerance)))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)

(sqrt 2 0.00001)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 1.4142135623746899
; > 