; Exercise 3.57: How many additions are performed when we compute the nthnth Fibonacci number using the definition of fibs based on the add-streams procedure? Show that the number of additions would be exponentially greater if we had implemented (delay ⟨exp⟩) simply as (lambda () ⟨exp⟩), without using the optimization provided by the memo-proc procedure described in 3.5.1.192
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")

(define counter 0)

(define (+withcounter s1 s2)  
  (set! counter (+ counter 1))
  (+ s1 s2))

(define (add-streams s1 s2) 
  (stream-map +withcounter s1 s2))

(define fibs 
  (cons-stream 
   0 (cons-stream
      1 (add-streams 
          (stream-cdr fibs) fibs))))
          
(newline )(stream-ref fibs 20)
(newline )(display counter)(newline )

; 19 times, it would be o(n)
; if not remember each time it accessed a fibs,
; supose it is n, then you have to access n-1 and n-2, make each step you take in to fib would be near twice the time you need for previous then the total time would be o(2^n)
; remembering means that it would eleminate the eatra n-2 part,each time, then you only need o(n) time,and it also take o(n) space to storage.

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.

; 6765

; 19
; > 