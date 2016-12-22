; Exercise 3.28: Define an or-gate as a primitive function box. Your or-gate constructor should be similar to and-gate.
(load "/home/soulomoon/git/SICP/Chapter3/signal.scm")

(define (inverter input output)
  (define (invert-input)
    (let ((new-value 
           (logical-not (get-signal input))))
         (set-signal! output new-value)))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) 
                        (get-signal a2))))
         (set-signal! output new-value)))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a b)
  (if (or (= a 1) (= b 1))
      1
      0))

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
'3.28

; Welcome to DrRacket, version 6.7 [3m].
; Language: SICP (PLaneT 1.18); memory limit: 128 MB.
; 'ok
; 0
; 'done
; 1
; 'done
; 1
; > 