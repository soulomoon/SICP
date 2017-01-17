; Exercise 4.24: Design and carry out some experiments to compare the speed of the original metacircular evaluator with the version in this section. Use your results to estimate the fraction of time that is spent in analysis versus execution for various procedures.
(load "/Users/soulomoon/git/SICP/Chapter4/separating.scm")
(define (report start_time)
  (display " *** ")
  (display (- (runtime) start_time))
  (newline ))

(define (eval# exp env) 
  (let ((start_time (runtime)))
        ; (report start_time)
        (let ((analyzed (analyze exp)))
              (report start_time)
              (display (analyzed env))
              (report start_time))))


(define a '(begin
  (define (test)
  (define b 2)
  (define (c) 3)
  (+ b (c)))
(test)))
(define e '(+ 1 1))
(define b '(- 2 2))

; (interpret a)
(interpret e)
(interpret b)
(interpret a)

; eval is about a 2 3 times longer than analysis

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
;  *** 684
; 2 *** 1473
;  *** 63
; 0 *** 189
;  *** 286
; 5 *** 646
; > 