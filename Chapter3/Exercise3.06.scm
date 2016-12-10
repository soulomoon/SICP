; Exercise 3.6: It is useful to be able to reset a random-number generator to produce a sequence starting from a given value. Design a new rand procedure that is called with an argument that is either the symbol generate or the symbol reset and behaves as follows: (rand 'generate) produces a new random number; ((rand 'reset) ⟨new-value⟩) resets the internal state variable to the designated ⟨new-value⟩. Thus, by resetting the state, one can generate repeatable sequences. These are very handy to have when testing and debugging programs that use random numbers.
#lang racket
(define random-init
  (random 100))
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define rand
  (let ((x random-init))
    (lambda () (set! x (rand-update x)) x)))

(define rand_test
  (let ((random-init 100)) 
    (lambda (x)
      (cond 
        ((eq? x 'reset) (lambda (y) (set! random-init y)))
        ((eq? x 'generate) (lambda () 
                                   (set! random-init 
                                         (rand-update random-init)) 
                                   random-init))
        (else (error "unknow words"))))))

((rand_test 'generate))
((rand_test 'generate))
((rand_test 'reset) 100)
((rand_test 'generate))
((rand_test 'generate))


((rand_test 'reset) 43)
((rand_test 'generate))
((rand_test 'generate))
((rand_test 'reset) 43)
((rand_test 'generate))
((rand_test 'generate))


; Welcome to DrRacket, version 6.7 [3m].
; Language: racket, with debugging; memory limit: 128 MB.
; 59
; 95
; 59
; 95
; 44
; 71
; 44
; 71
; > 