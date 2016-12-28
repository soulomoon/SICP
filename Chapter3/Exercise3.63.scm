; Exercise 3.63: Louis Reasoner asks why the sqrt-stream procedure was not written in the following more straightforward way, without the local variable guesses:

; (define (sqrt-stream x)
;   (cons-stream 
;    1.0
;    (stream-map (lambda (guess)
;                  (sqrt-improve guess x))
;                (sqrt-stream x))))
; Alyssa P. Hacker replies that this version of the procedure is considerably less efficient because it performs redundant computation. Explain Alyssa’s answer. Would the two versions still differ in efficiency if our implementation of delay used only (lambda () ⟨exp⟩) without using the optimization provided by memo-proc (3.5.1)?
(load "/home/soulomoon/git/SICP/Chapter3/Exercise3.62.scm")

(define (sqrt-stream x)
  (define guesses
    (cons-stream 
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)

(define (sqrt-stream x)
  (cons-stream 
   1.0
   (stream-map (lambda (guess)
                 (sqrt-improve guess x))
               (sqrt-stream x))))

; if define the guess it would run in the same guess, memo-proc enable it to store the already runned value inside delayed object, help it run faster,
; be if using louisreasoner's version, it every stream-map would callout a different (sqrt-stream), thus, the memo-proc is not working.
; when memo-proc, two should be the same