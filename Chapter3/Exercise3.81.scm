; Exercise 3.81: Exercise 3.6 discussed generalizing the random-number generator to allow one to reset the random-number sequence so as to produce repeatable sequences of “random” numbers. Produce a stream formulation of this same generator that operates on an input stream of requests to generate a new random number or to reset the sequence to a specified value and that produces the desired stream of random numbers. Don’t use assignment in your solution.
(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")


(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define (rand op_stream)
  (define (next op_stream number)
    (cons-stream 
      number
      (rand_inner op_stream number)))
  (define (rand_inner op_stream number)
      (let ((m (if (null? op_stream) '() (stream-car op_stream))))
        (cond 
          ((null? m) (next op_stream number))
          ((eq? m 'reset)
            (next 
              (stream-cdr (stream-cdr op_stream)) 
              (stream-car (stream-cdr op_stream))))
          ((eq? m 'generate) 
            (next
              (stream-cdr op_stream)
              (rand-update number)))
          (else (error "unknow request" m)))))
  (rand_inner op_stream 10))

(define cmds
  (list_to_stream 
    (list
      'generate
      'generate
      'generate
      'reset
      99
      'generate
      'generate
      'reset
      66
      'generate
)))

(define a (rand cmds))
(display-10 a)


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 
; 42
; 17
; 104
; 99
; 32
; 1
; 66
; 30
; 30
; 30
; 30'done
; > 