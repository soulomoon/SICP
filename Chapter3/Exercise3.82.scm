; Exercise 3.82: Redo Exercise 3.5 on Monte Carlo integration in terms of streams. The stream version of estimate-integral will not have an argument telling how many trials to perform. Instead, it will produce a stream of estimates based on successively more trials.

(load "/home/soulomoon/git/SICP/Chapter3/stream.scm")


(define (monte-carlo experiment-stream 
                     passed 
                     failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) 
      passed 
      failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (random_stream low high)
  (cons-stream
    (random-in-range low high)
    (random_stream low high)))


(define (round_test_stream x1 x2 y1 y2)
  (stream-map 
    (lambda (x y) 
      (<= 
        (+ 
          (square (- x (average x1 x2))) 
          (square (- y (average y1 y2)))) 
        (square (/ (- x2 x1) 2))))
    (random_stream x1 x2)
    (random_stream y1 y2)))

(define (estimate-integral x1 x2 y1 y2)
  (let ((es (round_test_stream x1 x2 y1 y2)))
        (stream-map
          (lambda (x) (* x 4))
          (monte-carlo es 0 0))))


(define pi
(estimate-integral 20 80 40 100))

(stream-ref pi 1)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 3 13005/100001
; > 

