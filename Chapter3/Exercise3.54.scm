; Exercise 3.54: Define a procedure mul-streams, analogous to add-streams, that produces the elementwise product of its two input streams. Use this together with the stream of integers to complete the following definition of the stream whose nthnth element (counting from 0) is n+1n+1 factorial:

; (define factorials 
;   (cons-stream 1 (mul-streams ⟨??⟩ ⟨??⟩)))
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")
(define (add-streams s1 s2) 
  (stream-map + s1 s2))
(define (mul-streams s1 s2) 
  (stream-map * s1 s2))
(define ones (cons-stream 1 ones))
(define integers 
  (cons-stream 1 (add-streams ones integers)))

(define factorials 
  (cons-stream 1 (mul-streams integers factorials)))

(define a 
  (stream-map (lambda (n) 
                (stream-ref factorials n)) (stream-enumerate-interval 0 20)))

(display-stream a)
; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.

; 1
; 1
; 2
; 6
; 24
; 120
; 720
; 5040
; 40320
; 362880
; 3628800
; 39916800
; 479001600
; 6227020800
; 87178291200
; 1307674368000
; 20922789888000
; 355687428096000
; 6402373705728000
; 121645100408832000
; 2432902008176640000'done
; > 