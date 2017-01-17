; Exercise 4.24: Design and carry out some experiments to compare the speed of the original metacircular evaluator with the version in this section. Use your results to estimate the fraction of time that is spent in analysis versus execution for various procedures.
(load "/Users/soulomoon/git/SICP/Chapter4/ex4.03.rkt")

(define (report start_time)
  (display " *** ")
  (display (- (runtime) start_time))
  (newline ))



(define a 
'(begin 
    (define (factorial n)
          (if (< n 2)
              1
(* (factorial (- n 1)) n)))
    (define (fib n)
          (cond ((= n 0) 0)
                ((= n 1) 1)
                (else (+ (fib (- n 1)) (fib (- n 2))))))

(factorial 50)
(fib 20)
))

(define
  (runtime-interpret exp) 
  (let ((start_time (runtime)))
    (display (interpret exp))
    (report start_time)
  ))

(newline )(display "without separating")(newline )
(runtime-interpret a)
(runtime-interpret a)
(runtime-interpret a)
(load "/Users/soulomoon/git/SICP/Chapter4/separating.scm")
(newline )(display "separating")(newline )
(runtime-interpret a)
(runtime-interpret a)
(runtime-interpret a)

; took about half of the time when separating eval and analysis
; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.

; without separating
; 6765 *** 622443
; 6765 *** 620444
; 6765 *** 603262

; separating
; 6765 *** 351626
; 6765 *** 273320
; 6765 *** 345168
; > 