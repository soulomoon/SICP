; Exercise 3.61: Let SS be a power series (Exercise 3.59) whose constant term is 1. Suppose we want to find the power series 1/S1/S, that is, the series XX such that SX=1SX=1. Write S=1+SRS=1+SR where SRSR is the part of SS after the constant term. Then we can solve for XX as follows:
; S⋅X(1+SR)⋅XX+SR⋅XX====1,1,1,1−SR⋅X.
; S⋅X=1,(1+SR)⋅X=1,X+SR⋅X=1,X=1−SR⋅X.
; In other words, XX is the power series whose constant term is 1 and whose higher-order terms are given by the negative of SRSR times XX. Use this idea to write a procedure invert-unit-series that computes 1/S1/S for a power series SS with constant term 1. You will need to use mul-series from Exercise 3.60.
(load "/home/soulomoon/git/SICP/Chapter3/Exercise3.60.scm")

(define (invert-unit-series S)
  (let ((Sr (stream-cdr S)))
    (define X 
      (cons-stream 
        1 
        (scale-stream 
          (mul-series Sr X) 
          -1)))
    X))

(define nones (invert-unit-series ones))
(define s (mul-series nones ones))

; (display-stream 
;   (stream-map (lambda (n) 
;                 (stream-ref s n)) 
;                   (stream-enumerate-interval 0 10)))


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.

; 1
; 0
; 0
; 0
; 0
; 0
; 0
; 0
; 0
; 0
; 0'done
; > 