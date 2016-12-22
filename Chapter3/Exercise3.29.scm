; Exercise 3.29: Another way to construct an or-gate is as a compound digital logic device, built from and-gates and inverters. Define a procedure or-gate that accomplishes this. What is the delay time of the or-gate in terms of and-gate-delay and inverter-delay?
(load "/home/soulomoon/git/SICP/Chapter3/signal.scm")
(load "/home/soulomoon/git/SICP/Chapter3/Exercise3.28.scm")
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) 
                        (get-signal a2))))
         (set-signal! output new-value)))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and a b)
  (if (and (= a 1) (= b 1))
      1
      0))

(define (or-gate a1 a2 output)
    (let ((ow1 (make-wire))
          (ow2 (make-wire))
          (out (make-wire)))
          (inverter a1 ow1)
          (inverter a2 ow2)
          (and-gate ow1 ow2 out)
          (inverter out output)))

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))

; (get-signal a)
; (get-signal b)
; (get-signal c)
(or-gate a b c)
(get-signal c)
(set-signal! a 1)
(get-signal c)
(set-signal! b 1)
(get-signal c)
'3.29

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 'ok
; 0
; 'done
; 1
; 'done
; 1
; > 