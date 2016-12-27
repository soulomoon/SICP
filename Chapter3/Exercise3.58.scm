; Exercise 3.58: Give an interpretation of the stream computed by the following procedure:

; (define (expand num den radix)
;   (cons-stream
;    (quotient (* num radix) den)
;    (expand (remainder (* num radix) den) 
;            den 
;            radix)))
; (Quotient is a primitive that returns the integer quotient of two integers.) What are the successive elements produced by (expand 1 7 10)? What is produced by (expand 3 8 10)?
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) 
           den 
           radix)))


(display-stream 
  (stream-map 
    (lambda (n) 
      (stream-ref (expand 1 7 10) n)) (stream-enumerate-interval 0 20)))
; 1 7 10
; ~1
; 3 7 10
; ~4
; 2 7 10
; ~2
; 6 7 10
; ~8

(display-stream 
  (stream-map 
    (lambda (n) 
      (stream-ref (expand 3 8 10) n)) (stream-enumerate-interval 0 20)))
; 3 8 10
; ~3
; 6 8 10
; ~7
; 4 8 10
; ~5
; 0 8 10
; ~0
; 0 8 10
; ~0