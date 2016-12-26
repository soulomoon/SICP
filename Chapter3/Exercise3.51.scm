; Exercise 3.51: In order to take a closer look at delayed evaluation, we will use the following procedure, which simply returns its argument after printing it:

; (define (show x)
;   (display-line x)
;   x)
; What does the interpreter print in response to evaluating each expression in the following sequence?187

; (define x 
;   (stream-map 
;    show 
;    (stream-enumerate-interval 0 10)))

; (stream-ref x 5)
; (stream-ref x 7)
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")

(define (show x)
  (display-line x)
  x)

(define x 
  (stream-map 
   show 
   (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)
(stream-ref x 4)


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.

; 0
; 1
; 2
; 3
; 4
; 55

; 6
; 77
; 4
; > 


;it runs it to get what it need when it need it, and cach it in memory.