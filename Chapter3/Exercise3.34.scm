; Exercise 3.34: Louis Reasoner wants to build a squarer, a constraint device with two terminals such that the value of connector b on the second terminal will always be the square of the value a on the first terminal. He proposes the following simple device made from a multiplier:

; (define (squarer a b) (multiplier a a b))
; There is a serious flaw in this idea. Explain.
(load "/home/soulomoon/git/SICP/Chapter3/constraint.scm")

(define (squarer a b) (multiplier a a b))

(define a (make-connector))
(probe 'a a)
(define b (make-connector))
(probe 'b b)

(squarer a b)
(set-value! a 1 'user)
(forget-value! a 'user)
(set-value! b 10 'user)

; Probe: a = 1
; Probe: b = 1'done

; Probe: a = ?
; Probe: b = ?'done

; Probe: b = 10'done
; > 

; as we can see, if you try to set the second moniter's value,  the first do not have enough imformation to get a root. that you have to impletment a single x in the constraint expression.