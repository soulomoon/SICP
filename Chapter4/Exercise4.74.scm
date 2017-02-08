; Exercise 4.74: Alyssa P. Hacker proposes to use a simpler version of stream-flatmap in negate, lisp-value, and find-assertions. She observes that the procedure that is mapped over the frame stream in these cases always produces either the empty stream or a singleton stream, so no interleaving is needed when combining these streams.

; Fill in the missing expressions in Alyssa’s program.
; (define (simple-stream-flatmap proc s)
;   (simple-flatten (stream-map proc s)))

; (define (simple-flatten stream)
;   (stream-map ⟨??⟩
;               (stream-filter ⟨??⟩ 
;                              stream)))
; Does the query system’s behavior change if we change it in this way?
(load "/Users/soulomoon/git/SICP/Chapter4/query-system.scm")
(define (simple-flatten stream)
  (stream-map (lambda (s) (stream-car s))
              (stream-filter (lambda (s) (not (stream-null? s)))
                             stream)))

(define a
  (simple-flatten 
   (cons-stream
    (cons-stream 'a nil)
    (cons-stream 
     (cons-stream 'b nil)
     nil))))

a
(stream-cdr a)

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 2048 MB.
; {mcons 'a #<promise>}
; {mcons 'b #<promise>}
; > 