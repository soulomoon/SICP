; Exercise 4.28: Eval uses actual-value rather than eval to evaluate the operator before passing it to apply, in order to force the value of the operator. Give an example that demonstrates the need for this forcing.

(define (id x) x)
((id +) 1 1)

eval id is a compund procedure, which mean + woule be delay-it'ed.
so you have to force-it to get + back.
; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 'done
; eval#-----(begin (define (id x) x) ((id +) 1 1))
; eval#-----(define (id x) x)
; eval#-----(lambda (x) x)
; eval#-----((id +) 1 1)
; eval#-----(id +)
; eval#-----id
; delay-it--------+
; eval#-----x
; eval#-----+
; force-it------thunk+
; eval#-----1
; eval#-----1
; 2
; > 