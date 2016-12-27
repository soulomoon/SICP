; Exercise 3.52: Consider the sequence of expressions

; (define sum 0)

; (define (accum x)
;   (set! sum (+ x sum))
;   sum)

; (define seq 
;   (stream-map 
;    accum 
;    (stream-enumerate-interval 1 20)))

; (define y (stream-filter even? seq))

; (define z 
;   (stream-filter 
;    (lambda (x) 
;      (= (remainder x 5) 0)) seq))

; (stream-ref y 7)
; (display-stream z)
; What is the value of sum after each of the above expressions is evaluated? What is the printed response to evaluating the stream-ref and display-stream expressions? Would these responses differ if we had implemented (delay ⟨exp⟩) simply as (lambda () ⟨exp⟩) without using the optimization provided by memo-proc? Explain.
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)
; 0

(define seq 
  (stream-map 
   accum 
   (stream-enumerate-interval 1 20)))

; 1


(define y (stream-filter even? seq))

; 6

(define z 
  (stream-filter 
   (lambda (x) 
     (= (remainder x 5) 0)) seq))
; 10

(stream-ref y 7)
; 136
(display-stream z)
; 210

; if without memo-proc, the accum would not be the same, so if trying to use accum more, the result would increase with each time you use it.


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 136

; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210'done
; > 

