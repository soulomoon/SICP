; Exercise 3.55: Define a procedure partial-sums that takes as argument a stream SS and returns the stream whose elements are S0S0, S0+S1S0+S1, S0+S1+S2,…S0+S1+S2,…. For example, (partial-sums integers) should be the stream 1, 3, 6, 10, 15, ….
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")


(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s))))

(define b (partial-sums integers))

(define a 
  (stream-map (lambda (n) 
                (stream-ref b n)) (stream-enumerate-interval 0 20)))

(display-stream a)


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.

; 1
; 3
; 6
; 10
; 15
; 21
; 28
; 36
; 45
; 55
; 66
; 78
; 91
; 105
; 120
; 136
; 153
; 171
; 190
; 210
; 231'done
; > 