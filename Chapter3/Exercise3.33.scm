; Exercise 3.33: Using primitive multiplier, adder, and constant constraints, define a procedure averager that takes three connectors a, b, and c as inputs and establishes the constraint that the value of c is the average of the values of a and b.

(load "/home/soulomoon/Documents/git/SICP/Chapter3/constraint.scm")
(define (averager a b c)
  (let ((sum (make-connector))
        (const (make-connector)))
        (constant 2 const)
        (adder a b sum)
        (multiplier const c sum)))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(probe 'a a)
(probe 'b b)
(probe 'c c)

(averager a b c)

(set-value! a 1 'user)
(set-value! b 10 'user)
(forget-value! a 'user)
(set-value! c 22 'user)

(get-value c)


; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; #<procedure:...3/constraint.scm:102:2>
; #<procedure:...3/constraint.scm:102:2>
; #<procedure:...3/constraint.scm:102:2>
; #<procedure:...3/constraint.scm:70:2>

; Probe: a = 1'done

; Probe: c = 11/2
; Probe: b = 10'done

; Probe: c = ?
; Probe: a = ?'done

; Probe: a = 34
; Probe: c = 22'done
; 22
; > 